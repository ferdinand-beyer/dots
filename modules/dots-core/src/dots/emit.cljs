(ns dots.emit
  "Emit ClojureScript adapter code."
  (:require [clojure.string :as str]
            [dots.node.fs :as fs]
            [dots.node.path :as path]
            [dots.util.io :as io]
            [dots.util.names :as names]
            [dots.util.table :as table]))

(defn- writer-coll
  "Returns a pseudo-collection that writes items with `writer` when they
   are conjoined."
  [writer]
  (reify ICollection
    (-conj [this x]
      (-write writer (str x))
      this)))

(defn- reduce-into [to f from]
  (reduce f to from))

(defn- emit-core-symbol [coll symbol ns-data]
  (if (contains? (:vars ns-data) symbol)
    (conj coll "cljs.core/" symbol)
    (conj coll symbol)))

(defn- emit-doc-string
  ([coll doc]
   (emit-doc-string coll doc "  "))
  ([coll doc indent]
   (conj coll
         indent "\""
         (-> doc
             (str/replace "\\" "\\\\")
             (str/replace "\"" "\\\"")
             (str/replace "\n" (str "\n" indent " ")))
         "\"")))

(defn- emit-get [coll path]
  (case (count path)
    1 (conj coll (first path))
    2 (conj coll "(.-" (second path) " " (first path) ")")
    (-> coll
        (conj "(.. " (first path))
        (into (mapcat #(list " -" %)) (next path))
        (conj ")"))))

(defn- emit-set [coll path expr ns-data]
  (-> coll
      (conj "(")
      (emit-core-symbol "set!" ns-data)
      (conj " ")
      (emit-get path)
      (conj " " expr ")")))

(defn- emit-args [coll args]
  (if (seq args)
    (-> coll (conj " ") (into (interpose " " args)))
    coll))

(defn- emit-call* [coll path emit-args]
  (case (count path)
    1 (-> coll
          (conj "(" (first path))
          (emit-args)
          (conj ")"))
    2 (-> coll
          (conj "(." (second path) " " (first path))
          (emit-args)
          (conj ")"))
    (-> coll
        (conj "(.. " (first path))
        (into (mapcat #(list " -" %)) (butlast (next path)))
        (conj " (" (last path))
        (emit-args)
        (conj "))"))))

(defn- emit-call [coll path args]
  (emit-call* coll path #(emit-args % args)))

(defn- emit-apply-args [coll path args rest-arg ns-data]
  (-> coll
      (conj " ")
      (as-> % (let [this-path (butlast path)]
                (if (seq this-path)
                  (emit-get % this-path)
                  (conj % "nil"))))
      (conj " (")
      (emit-core-symbol "to-array" ns-data)
      (conj " ")
      (as-> % (if (seq args)
                (-> %
                    (conj "(")
                    (emit-core-symbol (if (next args) "list*" "cons") ns-data)
                    (emit-args args)
                    (conj " " rest-arg ")"))
                (conj % rest-arg)))
      (conj ")")))

(defn- emit-apply [coll path args rest-arg ns-data]
  (emit-call* coll (concat path (list "apply"))
              #(emit-apply-args % path args rest-arg ns-data)))

(defn- emit-call-or-apply [coll path args rest-arg ns-data]
  (if rest-arg
    (emit-apply coll path args rest-arg ns-data)
    (emit-call coll path args)))

;; TODO: Support var-args variant, maybe using (.construct js/Reflect ctor args)
(defn- emit-construct [coll path args]
  (-> coll
      (conj "(new ")
      (emit-get path)
      (emit-args args)
      (conj ")")))

(defn- module-path [{:keys [module-name path]} ns-data]
  (if-let [alias (get-in ns-data [:requires module-name])]
    (cons (str alias "/" (first path)) (next path))
    (throw (ex-info (str "Missing require for module: " module-name)
                    {:type ::missing-require
                     :module-name module-name
                     :ns-data ns-data}))))

(defn- this-arg [args]
  (str "^js " (first args)))

(defmulti ^:private emit-expr
  {:arglists '([coll expr ns-data])}
  (fn [_ expr _]
    (:op expr)))

(defmethod emit-expr :global-get
  [coll expr ns-data]
  (emit-get coll (module-path expr ns-data)))

(defmethod emit-expr :global-set
  [coll {:keys [args] :as expr} ns-data]
  (emit-set coll (module-path expr ns-data) (first args) ns-data))

(defmethod emit-expr :global-call
  [coll {:keys [args rest-arg] :as expr} ns-data]
  (emit-call-or-apply coll (module-path expr ns-data) args rest-arg ns-data))

(defmethod emit-expr :global-construct
  [coll {:keys [args] :as expr} ns-data]
  (emit-construct coll (module-path expr ns-data) args))

(defmethod emit-expr :arg-get
  [coll {:keys [path args]} _]
  (emit-get coll (cons (this-arg args) path)))

(defmethod emit-expr :arg-set
  [coll {:keys [path args]} ns-data]
  (emit-set coll (cons (this-arg args) path) (second args) ns-data))

(defmethod emit-expr :arg-call
  [coll {:keys [path args rest-arg]} ns-data]
  (emit-call-or-apply coll (cons (this-arg args) path) (next args) rest-arg ns-data))

(defn- emit-arity-expr [coll indent exprs ns-data]
  (let [{:keys [args rest-arg] :as expr} (first exprs)
        args (cond-> args rest-arg (concat (list "&" rest-arg)))]
    (-> coll
        (conj "^js [" (str/join " " args) "]\n" indent)
        (emit-expr expr ns-data))))

#_(defn- emit-arglists [coll arities]
    (let [arglists (->> arities
                        (mapcat val)
                        (map :args)
                        distinct
                        sort)]
      (-> coll
          (conj "\n  {:arglists '(")
          (into (mapcat #(list "[" (str/join " " %) "] ")) arglists)
          (conj ")}"))))

;; TODO: Merge ranges of aritites (w/ arglists)?
;; TODO: Alias global functions instead of wrapping them?
(defn- emit-defn-arities
  [coll arities ns-data]
  (if (= 1 (count arities))
    (let [[_ exprs] (first arities)]
      (-> coll
          (conj "  ")
          (emit-arity-expr "  " exprs ns-data)))
    (-> coll
        ;(emit-arglists arities)
        (into (comp (map (fn [[_ exprs]]
                           (-> ["  ("]
                               (emit-arity-expr "   " exprs ns-data)
                               (conj ")"))))
                    (interpose (list "\n"))
                    cat)
              (sort-by key arities)))))

(defn- emit-defn
  [coll {:keys [var-name doc arities]} ns-data]
  (-> coll
      (conj "(")
      (emit-core-symbol "defn" ns-data)
      (conj " " var-name "\n")
      (cond-> doc (-> (emit-doc-string doc)
                      (conj "\n")))
      (emit-defn-arities arities ns-data)
      (conj ")\n")))

(defn- emit-def
  [coll {:keys [var-name doc init]} ns-data]
  (-> coll
      (conj "(")
      (emit-core-symbol "def" ns-data)
      (conj " " var-name)
      (as-> % (if doc
                (-> %
                    (conj "\n")
                    (emit-doc-string doc)
                    (conj "\n  "))
                (conj % " ")))
      (emit-expr init ns-data)
      (conj ")\n")))

(defn- emit-var
  [coll var-data ns-data]
  (let [coll (conj coll "\n")]
    (if (:init var-data)
      (emit-def coll var-data ns-data)
      (emit-defn coll var-data ns-data))))

(defn- emit-ns-form [coll {:keys [ns-path doc requires excludes]}]
  (-> coll
      (conj "(ns " (str/join "." ns-path))
      (cond->
       (seq doc) (-> (conj "\n")
                     (emit-doc-string doc))
       (seq excludes) (conj "\n  (:refer-clojure :exclude ["
                            (str/join " " (sort excludes))
                            "])")
       (seq requires) (-> (conj "\n  (:require")
                          (into (mapcat (fn [[module alias]]
                                          (list " [\"" module "\" :as " alias "]")))
                                (sort-by first requires))
                          (into ")")))
      (conj ")\n")))

(defn- namespace-munge [ns]
  (str/replace (str ns) \- \_))

(defn- ns-filepath [out-dir ns-path]
  {:pre [(seq ns-path)]}
  (let [segments (map namespace-munge ns-path)
        dirs     (butlast segments)
        filename (str (last segments) ".cljs")]
    (path/join (apply path/join out-dir dirs) filename)))

(defn- collect-requires [ns-data]
  (let [module-names (into #{} (comp (map val)
                                     (mapcat (fn [{:keys [init arities]}]
                                               (cons init (mapcat val arities))))
                                     (keep :module-name))
                           (:vars ns-data))]
    (cond-> ns-data
      (seq module-names) (assoc :requires (zipmap module-names
                                                  (map names/cljs-name module-names))))))

(defn- collect-excludes [ns-data]
  (let [excludes (filter names/cljs-core-name? (keys (:vars ns-data)))]
    (cond-> ns-data
      (seq excludes) (assoc :excludes (set excludes)))))

(defn- emit-namespace
  [out-dir {:keys [ns-path vars] :as ns-data}]
  (when (seq vars)
    (let [file-path (ns-filepath out-dir ns-path)
          ns-data   (-> ns-data
                        (collect-requires)
                        (collect-excludes))]
      (fs/mkdir-sync (path/dirname file-path) #js {:recursive true})
      (io/with-open [writer (io/file-writer file-path)]
        (-> (writer-coll writer)
            (emit-ns-form ns-data)
            (reduce-into (fn [coll var-data]
                           (emit-var coll var-data ns-data))
                         (table/tvals vars)))))))

(defn- emit-namespaces
  [out-dir namespaces]
  (doseq [ns namespaces]
    (emit-namespace out-dir ns)))

(defn emit-project
  [namespaces {:keys [output-dir]}]
  {:pre [(seq output-dir)]}
  ;; deps.edn file
  ;; src directory
  ;; src/deps.cljs file w/ :npm-deps (?)
  ;; package.json file (?)
  (emit-namespaces (path/join output-dir "src") (vals namespaces)))
