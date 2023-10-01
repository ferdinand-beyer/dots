(ns dots.emit
  (:require [clojure.string :as str]
            [dots.node.fs :as fs]
            [dots.node.path :as path]
            [dots.util.io :as io]
            [dots.util.names :as names]))

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
         "\"\n")))

(defn- emit-get [coll path]
  (case (count path)
    0 nil
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
    0 (conj coll "nil")
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

(defn- module-path [module-name path ns-data]
  (let [alias (get-in ns-data [:requires module-name] "ALIAS-MISSING")]
    (cons (str alias "/" (first path)) (next path))))

(defmulti ^:private emit-op
  {:arglists '([coll arity-data args ns-data])}
  (fn [_ arity-data _ _]
    (:op arity-data)))

(defmethod emit-op :module-get
  [coll {:keys [module-name path]} _ ns-data]
  (emit-get coll (module-path module-name path ns-data)))

(defmethod emit-op :module-set
  [coll {:keys [module-name path]} args ns-data]
  (emit-set coll (module-path module-name path ns-data) (first args)))

(defmethod emit-op :module-call
  [coll {:keys [module-name path]} args ns-data]
  (emit-call coll (module-path module-name path ns-data) args))

(defmethod emit-op :arg-get
  [coll {:keys [path]} args _]
  (emit-get coll (cons (first args) path)))

(defmethod emit-op :arg-set
  [coll {:keys [path]} args _]
  (emit-set coll (cons (first args) path) (second args)))

(defmethod emit-op :arg-call
  [coll {:keys [path]} args _]
  (emit-call coll (cons (first args) path) (next args)))

(defn- emit-arity [coll {:keys [arglists] :as arity} ns-data]
  (let [args (first arglists)]
    (-> coll
        (conj "  ([" (str/join " " args) "]\n" "   ")
        (emit-op arity args ns-data)
        (conj ")\n"))))

;; TODO: Support :alias w/ :module-get op in addition to :arities
;; TODO: When only one arity, use simple defn
;; TODO: When all arities have a unique arglist, don't emit :arglists
;; TODO: Sort arities?
;; TODO: Merge ranges of aritites
;; TODO: Emit ops
(defn- emit-var
  [coll {:keys [var-name doc arities]} ns-data]
  ;; - (clj-excluded? x) -> need fqn cljs.core/x
  (-> coll
      (conj "\n" "(defn " var-name "\n")
      (cond-> doc (emit-doc-string doc))
      (conj "  {:arglists '(")
      (into (comp (mapcat :arglists)
                  (mapcat #(list "[" (str/join " " %) "] ")))
            (vals arities))
      (conj ")}\n")
      (reduce-into (fn [coll arity]
                     (emit-arity coll arity ns-data))
                   (vals arities))
      (conj "  )\n")))

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
                                            (list "\n   [" module " :as " alias "]")))
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
  (let [module-names (into #{} (comp (mapcat (comp :arities val))
                                     (keep (comp :module-name val)))
                           (:vars ns-data))]
    (cond-> ns-data
      (seq module-names) (assoc :requires (zipmap module-names
                                                  (map names/cljs-name module-names))))))

(defn- emit-namespace
  [out-dir {:keys [ns-path vars] :as ns-data}]
  (let [file-path (ns-filepath out-dir ns-path)
        ns-data   (collect-requires ns-data)]
    (fs/mkdir-sync (path/dirname file-path) #js {:recursive true})
    (io/with-open [writer (io/file-writer file-path)]
      (-> (writer-coll writer)
          (emit-ns-form ns-data)
          (reduce-into (fn [coll var-data]
                         (emit-var coll var-data ns-data))
                       (vals vars))))))

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
