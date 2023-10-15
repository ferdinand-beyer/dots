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

(defn- emit-set [coll path expr]
  (-> coll
      (conj "(set! ")
      (emit-get path)
      (conj " " expr ")")))

(defn- emit-args [coll args]
  (if (seq args)
    (-> coll (conj " ") (into (interpose " " args)))
    coll))

(defn- emit-call [coll path args]
  (case (count path)
    1 (-> coll
          (conj "(" (first path))
          (emit-args args)
          (conj ")"))
    2 (-> coll
          (conj "(." (second path) " " (first path))
          (emit-args args)
          (conj ")"))
    (-> coll
        (conj "(.. " (first path))
        (into (mapcat #(list " -" %)) (butlast (next path)))
        (conj " (" (last path))
        (emit-args args)
        (conj "))"))))

(defn- emit-construct [coll path args]
  (-> coll
      (conj "(new ")
      (emit-get path)
      (emit-args args)
      (conj ")")))

(defn- module-path [{:keys [module-name path]} ns-data]
  (if-let [alias (get-in ns-data [:requires module-name])]
    (cons (str alias "/" (first path)) (next path))
    (throw (ex-info (str "Missing require for module" module-name)
                    {:type ::missing-require
                     :module-name module-name
                     :ns-data ns-data}))))

(defmulti ^:private emit-expr
  {:arglists '([coll expr args ns-data])}
  (fn [_ expr _ _]
    (:op expr)))

(defmethod emit-expr :global-get
  [coll expr _ ns-data]
  (emit-get coll (module-path expr ns-data)))

(defmethod emit-expr :global-set
  [coll expr args ns-data]
  (emit-set coll (module-path expr ns-data) (first args)))

(defmethod emit-expr :global-call
  [coll expr args ns-data]
  (emit-call coll (module-path expr ns-data) args))

(defmethod emit-expr :global-construct
  [coll expr args ns-data]
  (emit-construct coll (module-path expr ns-data) args))

(defmethod emit-expr :arg-get
  [coll {:keys [path]} args _]
  (emit-get coll (cons (first args) path)))

(defmethod emit-expr :arg-set
  [coll {:keys [path]} args _]
  (emit-set coll (cons (first args) path) (second args)))

(defmethod emit-expr :arg-call
  [coll {:keys [path]} args _]
  (emit-call coll (cons (first args) path) (next args)))

;; TODO: Support variadic (neg? arity)
(defn- emit-arity-expr [coll indent _arity exprs ns-data]
  (let [{:keys [args] :as expr} (first exprs)]
    (-> coll
        (conj "[" (str/join " " args) "]\n" indent)
        (emit-expr expr args ns-data))))

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
    (let [[arity exprs] (first arities)]
      (-> coll
          (conj "  ")
          (emit-arity-expr "  " arity exprs ns-data)))
    (-> coll
        ;(emit-arglists arities)
        (into (comp (map (fn [[arity exprs]]
                           (-> ["  ("]
                               (emit-arity-expr "   " arity exprs ns-data)
                               (conj ")"))))
                    (interpose (list "\n"))
                    cat)
              (sort-by key arities)))))

(defn- emit-core-symbol [coll symbol ns-data]
  (if (contains? (:vars ns-data) symbol)
    (conj coll "cljs.core" symbol)
    (conj coll symbol)))

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
      (emit-expr init nil ns-data)
      (conj ")\n")))

(defn- emit-var
  [coll var-data ns-data]
  (let [coll (conj coll "\n")]
    (if (:init var-data)
      (emit-def coll var-data ns-data)
      (emit-defn coll var-data ns-data))))

(defn- emit-ns-form [coll {:keys [ns-path doc vars requires]}]
  ;; - docstring
  ;; - :refer-clojure :exclude var names
  ;; - :requires
  (let [excludes (filter names/core-name? (keys vars))]
    (-> coll
        (conj "(ns " (str/join "." ns-path))
        (cond->
         (seq doc) (-> (conj "\n")
                       (emit-doc-string doc))
         (seq excludes) (conj "\n  (:refer-clojure :exclude ["
                              (str/join " " excludes)
                              "])")
         (seq requires) (-> (conj "\n  (:require")
                            (into (mapcat (fn [[module alias]]
                                            (list " [\"" module "\" :as " alias "]")))
                                  requires)
                            (into ")")))
        (conj ")\n"))))

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

(defn- emit-namespace
  [out-dir {:keys [ns-path vars] :as ns-data}]
  (when (seq vars)
    (let [file-path (ns-filepath out-dir ns-path)
          ns-data   (collect-requires ns-data)]
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
