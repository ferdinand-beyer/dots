(ns dots.adapt
  "Determine which ClojureScript adapter namespaces and vars to generate
   from TypeScript module information."
  (:require [clojure.string :as str]
            [dots.util.names :as names]
            [dots.util.table :as table]))

(def empty-ctx
  {:namespaces  {}
   :symbol-path []
   :ns-path     []})

(defn- resolve-node [node path]
  (let [name (first path)]
    (if (nil? name)
      node
      (when-let [n (or (get-in node [:exports name])
                       (get-in node [:members name]))]
        (recur n (next path))))))

(defn- resolve-fqn [ctx fqn]
  (let [module (:module ctx)]
    (when (= (:name module) (first fqn))
      (resolve-node module (next fqn)))))

(defn- update-ns-path [{:keys [ns-path root-ns-path] :as ctx} f]
  (let [ns-path (if (and (empty? ns-path) root-ns-path)
                  root-ns-path
                  (f ns-path))
        ns-key  (when (seq ns-path)
                  (str/join "." (map name ns-path)))]
    (assoc ctx :ns-path ns-path :ns-key ns-key)))

(defn- update-ns [ctx f & args]
  (if-let [k (:ns-key ctx)]
    (apply update-in ctx [:namespaces k] f args)
    (do
      (println "Warning: Attempting to add to var outside of namespace")
      ctx)))

(defn- assoc-ns-path [ctx]
  (update-ns ctx assoc :ns-path (:ns-path ctx)))

(defn- enter-namespace [ctx symbol-name ns-data]
  (-> ctx
      ;; ? We might want to separate "symbol path" and "namespace path"?
      ;; E.g. if we don't want to mirror the hierarchy?
      (update :symbol-path conj symbol-name)
      (update-ns-path #(conj % (names/cljs-name symbol-name)))
      (assoc-ns-path)
      (update-ns merge ns-data)))

(defn- leave-namespace [ctx]
  (-> ctx
      (update :symbol-path pop)
      ;; ? Keep a separate stack of mapped ns-paths,
      ;; so that users can map symbol-paths arbitarily
      (update-ns-path pop)))

(defn- add-var [ctx var-name data]
  ;; TODO: Handle name conflicts (rename?)
  (update-ns ctx
             update :vars
             table/tupdate var-name
             merge data {:var-name var-name}))

(defn- expr-conflict [x y]
  (cond
    (not= (:op x) (:op y))
    {:type ::op-conflict
     :message (str "Conflicting operations: "
                   (:op x) " and " (:op y))}
    (not= (:module-name x) (:module-name y))
    {:type ::module-confict
     :message (str "Conflicting module: "
                   (:module-name x) " and "
                   (:module-name y))}
    (not= (:path x) (:path y))
    {:type ::path-confict
     :message (str "Conflicting paths: "
                   (:path x) " and " (:path y))}))

(defn- init-expr-conflict [var-data expr]
  (if-let [existing (first (:init var-data))]
    (expr-conflict existing expr)
    (cond
      (seq (:arities var-data))
      {:type ::init-arity-conflict
       :message "Var :init and :arities are mutually exclusive"}
      (not= :global-get (:op expr))
      {:type ::invalid-init-op
       :message "Only :global-get supported as var init expr"})))

(defn- arity-expr-conflict [var-data arity expr]
  (if (seq (:init var-data))
    {:type ::init-arity-conflict
     :message "Var :init and :arities are mutually exclusive"}
    (when-let [existing (first (get-in var-data [:arities arity]))]
      (expr-conflict existing expr))))

(defn- expr-exception [ctx var-name expr conflict]
  (ex-info (:message conflict)
           {:type     (:type conflict)
            :ns       (:ns-key ctx)
            :var-name var-name
            :expr     expr}))

(defn- add-init [ctx var-name expr]
  (update-ns ctx update-in [:vars var-name]
             (fn [var-data]
               (if-let [conflict (init-expr-conflict var-data expr)]
                 (throw (expr-exception ctx var-name expr conflict))
                 (assoc var-data :init expr)))))

(defn- add-arity [ctx var-name arity expr]
  (update-ns ctx update-in [:vars var-name]
             (fn [var-data]
               (if-let [conflict (arity-expr-conflict var-data arity expr)]
                 (throw (expr-exception ctx var-name expr conflict))
                 (update-in var-data [:arities arity] (fnil conj []) expr)))))

(defn- adapt-signature [{:keys [params]}]
  (let [params        (mapv (fn [param]
                              ;; TODO: Keep some type information for hints, doc-string
                              (-> (select-keys param [:name :doc :optional? :rest?])
                                  (update :name names/cljs-name)))
                            params)
        params-map    (into {} (map (juxt :name identity)) params)
        [params rest] (if (:rest? (peek params))
                        [(pop params) (peek params)]
                        [params nil])
        [req opt]     (split-with (complement :optional?) params)
        req-args      (mapv :name req)
        req-arity     {:args req-args}
        arities       (if (seq opt)
                        (loop [arities [req-arity]
                               args    req-args
                               opt     opt]
                          (if-let [param (first opt)]
                            (let [args (conj args (:name param))]
                              (recur (conj arities {:args args}) args (next opt)))
                            (if rest
                              (conj (pop arities) (assoc (peek arities) :rest-arg (:name rest)))
                              arities)))
                        [(cond-> req-arity rest (assoc :rest-arg (:name rest)))])]
    ;; TODO: Add return type information for doc-string
    (cond-> {:params  params-map
             :arities arities}
      rest (assoc :variadic rest))))

;; !!! Typescript supports multiple variadic signatures, e.g.:
;; showWarningMessage(message, options?, ...items)
;; ClojureScript does not.  We can, however, omit longer signatures, and add arglists instead
(defn- add-signature [ctx var-name signature expr-fn]
  (let [{:keys [_params arities]} (adapt-signature signature)]
    ;; TODO: Store info from params and return type in var-map
    ;; Also need to take "this" arg into account, when added by expr-fn
    ;; Probably need a second pass to construct a doc-string?
    (reduce (fn [ctx {:keys [args rest-arg]}]
              (let [{:keys [args] :as expr} (expr-fn args rest-arg)
                    expr (cond-> expr rest-arg (assoc :rest-arg rest-arg))]
                (add-arity ctx var-name (count args) expr)))
            ctx
            arities)))

(defn- add-signatures [ctx var-name signatures expr-fn]
  (reduce (fn [ctx signature]
            (add-signature ctx var-name signature expr-fn))
          ctx
          signatures))

(defmulti adapt-trait
  (fn [_ctx trait _node]
    trait))

(defn- adapt* [ctx node]
  (if (:internal? node)
    ctx
    (reduce (fn [ctx trait]
              (adapt-trait ctx trait node))
            ctx
            (:traits node))))

(defmethod adapt-trait :default
  [ctx trait node]
  (println "Warning: No adapt-trait implementation for" trait (:name node))
  ctx)

(defmethod adapt-trait :module
  [ctx _ {:keys [name exports] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (table/tvals exports))
    (leave-namespace %)))

(defn- adapt-variable-interface [ctx node expr interface-node]
  (-> ctx
      (enter-namespace (:name node) (select-keys node [:doc]))
      (assoc :bind-expr expr)
      (as-> % (reduce adapt* % (table/tvals (:members interface-node))))
      (dissoc :bind-expr)
      (leave-namespace)))

(defmethod adapt-trait :variable
  [ctx _ {:keys [name type] :as node}]
  (let [var-name       (names/cljs-name name)
        [_ & path]     (:symbol-path ctx)
        expr           {:module-name (get-in ctx [:module :import-name])
                        :path        (vec path)}
        interface-node (when (:object? type)
                         (some->> (:fqn type) (resolve-fqn ctx)))]
    (if (seq path)
      ;; Variable within a module
      (let [expr (update expr :path conj name)]
        (-> ctx
            (add-var var-name (select-keys node [:doc]))
            ;; TODO: Add :init expr for consts, e.g. generate a `def` instead of `defn`?
            (add-arity var-name 0 (assoc expr :op :global-get))
            ;; TODO: If the type is callable, add arities to call?
            (cond->
             interface-node (adapt-variable-interface node expr interface-node))))
      ;; Exported module has variable trait.
      (cond-> ctx
        interface-node (adapt-variable-interface node expr interface-node)))))

(defmethod adapt-trait :function
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        [_module & path] (:symbol-path ctx)
        path     (conj (vec path) name)
        ctx      (add-var ctx var-name (select-keys node [:doc]))]
    (add-signatures ctx var-name signatures
                    (fn [args]
                      {:op          :global-call
                       :module-name (get-in ctx [:module :import-name])
                       :path        path
                       :args        args}))))

(defn- adapt-interface [ctx {:keys [name members] :as node}]
  (-> ctx
      (enter-namespace name (select-keys node [:doc]))
      (as-> % (reduce adapt* % (table/tvals members)))
      (leave-namespace)))

(defmethod adapt-trait :class
  [ctx _ node]
  ;; TODO: Look for construct signatures?
  ;; TODO: Adapt exports (static members)
  (adapt-interface ctx node))

(defmethod adapt-trait :interface
  [ctx _ node]
  ;; TODO: Add `invoke` var when the interface has call signatures?
  (adapt-interface ctx node))

(defmethod adapt-trait :enum
  [ctx _ {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (table/tvals members))
    (leave-namespace %)))

(defmethod adapt-trait :enum-member
  [ctx _ {:keys [name] :as node}]
  (let [var-name        (names/cljs-name name)
        [_module & path] (:symbol-path ctx)]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-init var-name {:op          :global-get
                            :module-name (get-in ctx [:module :import-name])
                            :path        (conj (vec path) name)}))))

;; TODO: For class/interface members, check if the name is a valid identifier,
;; to exclude/handle "indirect" names such as `[Symbol.iterator]`
;; TODO: Clojurify names, e.g. '?' suffix for booleans, remove `is-` and `get-` prefixes
;; But then: Handle collisions, e.g. "name" and "getName" (maybe use `-name` for property access?)

;; TODO: Rename "this" arg if another arg exists with the same name

(defmethod adapt-trait :property
  [ctx _ {:keys [name] :as node}]
  ;; TODO: If the type is callable, add arities (e.g. VS-Code events)
  (let [var-name  (names/cljs-name name)
        ctx      (add-var ctx var-name (select-keys node [:doc]))
        type-name (names/cljs-name (last (:symbol-path ctx)))]
    (if-let [bind-expr (:bind-expr ctx)]
      (let [expr (-> bind-expr
                     (assoc :op :global-get)
                     (update :path conj name))]
        (add-arity ctx var-name 0 expr))
      (add-arity ctx var-name 1 {:op   :arg-get
                                 :path [name]
                                 :args [type-name]}))))

(defmethod adapt-trait :method
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        expr-fn  (if-let [bind-expr (:bind-expr ctx)]
                   (let [expr (-> bind-expr
                                  (assoc :op :global-call)
                                  (update :path conj name))]
                     (fn [args]
                       (assoc expr :args args)))
                   (let [type-name (names/cljs-name (last (:symbol-path ctx)))
                         path      [name]]
                     (fn [args]
                       {:op   :arg-call
                        :path path
                        :args (into [type-name] args)})))]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-signatures var-name signatures expr-fn))))

(defn- default-root-ns-path [import-name]
  (->> (str/split import-name "/")
       (map names/cljs-name)
       (into ["dots"])))

(defn adapt [module opts]
  (let [{:keys [import-name]} module
        {:keys [namespace]}   opts]
    (-> empty-ctx
        (assoc :module module
               :root-ns-path (if namespace
                               (str/split namespace #"\.")
                               (default-root-ns-path import-name)))
        (adapt* module)
        :namespaces)))
