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

(defn- enter-namespace [ctx symbol-name doc-string]
  (-> ctx
      ;; ? We might want to separate "symbol path" and "namespace path"?
      ;; E.g. if we don't want to mirror the hierarchy?
      (update :symbol-path conj symbol-name)
      (update-ns-path #(conj % (names/cljs-name symbol-name)))
      (assoc-ns-path)
      (cond-> doc-string (update-ns assoc :doc doc-string))))

(defn- leave-namespace [ctx]
  (-> ctx
      (update :symbol-path pop)
      ;; ? Keep a separate stack of mapped ns-paths,
      ;; so that users can map symbol-paths arbitarily
      (update-ns-path pop)))

(defn- preferred-name [n type]
  (-> n
      names/strip-getter-prefix
      (cond-> (:boolean? type) (str "?"))))

(defn- add-var
  ([ctx var-name doc-string]
   (add-var ctx var-name doc-string nil))
  ([ctx var-name doc-string type]
   (let [var-name (names/munge-name var-name)
         preferred-name (preferred-name var-name type)]
     (update-ns ctx
                update :vars
                table/tupdate var-name
                (fn [var-data]
                  (cond-> (assoc var-data :var-name var-name)
                    doc-string (assoc :doc doc-string)
                    (not= var-name preferred-name) (assoc :preferred-name preferred-name)))))))

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
  (if-let [existing (:init-expr var-data)]
    (expr-conflict existing expr)
    (cond
      (seq (:arities var-data))
      {:type ::init-arity-conflict
       :message "Var :init-expr and :arities are mutually exclusive"}
      (not= :global-get (:op expr))
      {:type ::invalid-init-op
       :message "Only :global-get supported as var init expr"})))

(defn- arity-expr-conflict [var-data arity expr]
  (if (:init-expr var-data)
    {:type ::init-arity-conflict
     :message "Var :init-expr and :arities are mutually exclusive"}
    (when-let [existing (get-in var-data [:arities arity :expr])]
      (expr-conflict existing expr))))

(defn- expr-exception
  ([conflict]
   (ex-info (:message conflict) conflict))
  ([ctx var-name expr conflict]
   (ex-info (str (:message conflict) " in " (:ns-key ctx) "/" var-name)
            {:type     (:type conflict)
             :ns       (:ns-key ctx)
             :var-name var-name
             :expr     expr})))

(defn- add-init [ctx var-name expr]
  (update-ns ctx update-in [:vars var-name]
             (fn [var-data]
               (if-let [conflict (init-expr-conflict var-data expr)]
                 (throw (expr-exception ctx var-name expr conflict))
                 (assoc var-data :init-expr expr)))))

(defn- add-arity-args [arity-data expr args variadic?]
  (cond-> (if (some? arity-data)
            (update arity-data :arglists conj args)
            {:arglists #{args}
             :expr expr})
    variadic? (assoc :variadic? true)))

(defn- add-this-arg [args ctx]
  (let [type-name (last (:ns-path ctx))
        this-arg  (if (some #(= type-name %) args)
                    (str "this-" type-name)
                    type-name)]
    (into [this-arg] args)))

(defn- add-arity
  ([ctx var-name expr]
   (add-arity ctx var-name expr []))
  ([ctx var-name expr args]
   (add-arity ctx var-name expr args nil))
  ([ctx var-name expr args rest-arg]
   {:pre [(vector? args)]}
   (let [args  (cond-> args
                 (#{:arg-get :arg-set :arg-call} (:op expr)) (add-this-arg ctx)
                 rest-arg (conj rest-arg))
         arity (cond-> (count args) rest-arg dec)]
     (update-ns ctx update-in [:vars var-name]
                (fn [var-data]
                  (if-let [conflict (arity-expr-conflict var-data arity expr)]
                    (throw (expr-exception ctx var-name expr conflict))
                    (update-in var-data [:arities arity] add-arity-args expr args rest-arg)))))))

(defn- merge-arities [arities to from]
  (let [to-data   (get arities to)
        from-data (get arities from)]
    (if-let [conflict (expr-conflict (:expr to-data) (:expr from-data))]
      (throw (expr-exception conflict))
      (let [merged (-> to-data
                       (update :arglists into (:arglists from-data))
                       (cond-> (:variadic? to-data) (assoc :variadic? true)))]
        (-> arities
            (assoc to merged)
            (dissoc from))))))

(defn- prepare-params [params]
  (->> params
       (map-indexed (fn [i {:keys [name type]
                            :as   param}]
                      (let [name           (-> name names/cljs-name names/munge-name)
                            preferred-name (preferred-name name type)]
                        (-> (select-keys param [:doc :type :optional? :rest?])
                            (assoc :index i, :name name)
                            (cond-> (not= name preferred-name) (assoc :preferred-name preferred-name))))))
       (into {} (map (juxt :name identity)))))

(defn- rename-params [params-map]
  (reduce (fn [m {:keys [preferred-name]
                  :as   param}]
            (let [param (-> param
                            (dissoc :preferred-name)
                            (cond-> (and preferred-name
                                         (not (contains? params-map preferred-name)))
                              (assoc :name preferred-name)))]
              (assoc m (:name param) param)))
          {}
          (vals params-map)))

(defn- adapt-signature [{:keys [params]}]
  (let [params-map    (-> params prepare-params rename-params)
        params        (vec (sort-by :index (vals params-map)))
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
    ;; TODO: Add return type information for doc-string and type hints
    (cond-> {:params  params-map
             :arities arities}
      rest (assoc :variadic rest))))

(defn- add-signature [ctx var-name signature expr]
  (let [{:keys [_params arities]} (adapt-signature signature)]
    ;; TODO: Store info from params and return type in var-map
    ;; Also need to take "this" arg into account, when added by expr-fn
    ;; Probably need a second pass to construct a doc-string?
    (reduce (fn [ctx {:keys [args rest-arg]}]
              (add-arity ctx var-name expr args rest-arg))
            ctx
            arities)))

(defn- add-signatures [ctx var-name signatures expr]
  (reduce (fn [ctx signature]
            (add-signature ctx var-name signature expr))
          ctx
          signatures))

(defmulti adapt-trait
  (fn [_ctx trait _node]
    trait))

;; TODO: Order traits, e.g. adapt module before class?
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
    (enter-namespace % name (:doc node))
    (reduce adapt* % (table/tvals exports))
    (leave-namespace %)))

(defn- adapt-variable-interface [ctx node var-name expr interface-node]
  (let [signatures (:signatures interface-node)]
    (-> ctx
        ;; TODO: Handle arity clash: get property vs call with zero args
        (cond-> (seq signatures) (add-signatures var-name signatures (assoc expr :op :global-call)))
        (enter-namespace (:name node) (:doc node))
        (assoc :bind-expr expr)
        (as-> % (reduce adapt* % (table/tvals (:members interface-node))))
        (dissoc :bind-expr)
        (leave-namespace))))

(defn- resolve-interface-type [ctx type]
  (when (:object? type)
    (if-let [fqn (:fqn type)]
      (resolve-fqn ctx fqn)
      (if-let [type-ref (:reference type)]
        ;; Resolve type reference
        (recur ctx type-ref)
        (when-let [types (:union type)]
          (when (and (:undefined? type)
                     (= 2 (count types)))
            ;; Type | undefined
            (recur ctx (first (remove :undefined? types)))))))))

(defmethod adapt-trait :variable
  [ctx _ {:keys [name type] :as node}]
  (let [var-name  (names/cljs-name name)
        expr      {:module-name (get-in ctx [:module :import-name])
                   :path        (vec (next (:symbol-path ctx)))}
        interface (resolve-interface-type ctx type)]
    (if (:ns-key ctx)
      ;; Variable within a module
      (let [expr (update expr :path conj name)]
        (-> ctx
            (add-var var-name (:doc node) type)
            ;; ? :init-expr for consts, e.g. generate a `def` instead of `defn`?
            (add-arity var-name (assoc expr :op :global-get))
            (cond-> interface (adapt-variable-interface node var-name expr interface))))
      ;; Exported module has variable trait.
      (cond-> ctx
        interface (adapt-variable-interface node var-name expr interface)))))

(defn- signatures-return-type [signatures]
  (apply merge (map :return-type signatures)))

(defmethod adapt-trait :function
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        [_module & path] (:symbol-path ctx)
        path     (conj (vec path) name)
        ctx      (add-var ctx var-name (:doc node) (signatures-return-type signatures))]
    (add-signatures ctx var-name signatures {:op          :global-call
                                             :module-name (get-in ctx [:module :import-name])
                                             :path        path})))

(defn- adapt-interface [ctx {:keys [name members] :as node}]
  (-> ctx
      (enter-namespace name (:doc node))
      (as-> % (reduce adapt* % (table/tvals members)))
      (leave-namespace)))

(defmethod adapt-trait :class
  [ctx _ node]
  ;; TODO: Adapt constructors (->name) from construct signatures
  (adapt-interface ctx node))

(defmethod adapt-trait :interface
  [ctx _ node]
  ;; TODO: Add `invoke` var when the interface has call signatures?
  ;; TODO: Create a "constructor" to create compatible objects?
  ;; (defn ->interface [& members] #js {...members...})
  (adapt-interface ctx node))

(defmethod adapt-trait :enum
  [ctx _ {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (:doc node))
    (reduce adapt* % (table/tvals members))
    (leave-namespace %)))

(defmethod adapt-trait :enum-member
  [ctx _ {:keys [name] :as node}]
  (let [var-name        (names/cljs-name name)
        [_module & path] (:symbol-path ctx)]
    (-> ctx
        (add-var var-name (:doc node))
        (add-init var-name {:op          :global-get
                            :module-name (get-in ctx [:module :import-name])
                            :path        (conj (vec path) name)}))))

;; TODO: For class/interface members, check if the name is a valid identifier,
;; to exclude/handle "indirect" names such as `[Symbol.iterator]`
;; TODO: Clojurify names, e.g. '?' suffix for booleans, remove `is-` and `get-` prefixes
;; But then: Handle collisions, e.g. "name" and "getName" (maybe use `-name` for property access?)

(def ^:private setter-args ["value"])

(defn- add-setter-var [ctx {:keys [name] :as node} expr]
  (let [var-name (str "set-" (names/cljs-name name) "!")]
    (-> ctx
        (add-var var-name (:doc node))
        (add-arity var-name expr setter-args))))

(defmethod adapt-trait :property
  [ctx _ {:keys [name const? type] :as node}]
  (let [var-name   (names/cljs-name name)
        bind-expr  (:bind-expr ctx)
        get-expr   (if bind-expr
                     (-> bind-expr
                         (assoc :op :global-get)
                         (update :path conj name))
                     {:op   :arg-get
                      :path [name]})
        set-expr   (when-not const?
                     (assoc get-expr :op (if bind-expr :global-set :arg-set)))
        signatures (some-> (resolve-interface-type ctx type) :signatures)]
    (-> ctx
        (add-var var-name (:doc node) type)
        (add-arity var-name get-expr)
        (cond->
         ;; TODO: Handle arity clash: get property vs call with zero args
         (seq signatures) (add-signatures var-name
                                          signatures
                                          (assoc get-expr :op (if bind-expr :global-call :arg-call)))
         set-expr (add-setter-var node set-expr)))))

;; TODO: Methods can be optional, should we add a null-check arity?
(defmethod adapt-trait :method
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        expr     (if-let [bind-expr (:bind-expr ctx)]
                   (-> bind-expr
                       (assoc :op :global-call)
                       (update :path conj name))
                   {:op   :arg-call
                    :path [name]})]
    (-> ctx
        (add-var var-name (:doc node) (signatures-return-type signatures))
        (add-signatures var-name signatures expr))))

(defn- rename-vars
  "Assigns vars their preferred name if that is possible without collision."
  [vars]
  (reduce (fn [new-vars {:keys [var-name preferred-name] :as var-data}]
            (let [var-data (dissoc var-data :preferred-name)]
              (if (and preferred-name (not (contains? vars preferred-name)))
                (assoc new-vars preferred-name (assoc var-data :var-name preferred-name))
                (assoc new-vars var-name var-data))))
          {}
          (table/tvals vars)))

(defn- unify-variadic-arities
  "Merges variadic arities of all vars.

   While TypeScript supports multiple variadic signatures for one function,
   ClojureScript only supports one.  We therefore need to resolve this by
   reducing all variadic arities into the shortest one."
  [ctx]
  (->> (for [[ns-key   {:keys [vars]}] (:namespaces ctx)
             [var-name {:keys [arities]}] vars
             :let [variadics (filter #(:variadic? (val %)) arities)]
             :when (next variadics)]
         (let [arity-keys (sort (keys variadics))
               shortest   (first arity-keys)
               arities    (reduce (fn [arities key]
                                    (merge-arities arities shortest key))
                                  arities
                                  (next arity-keys))]
           [ns-key var-name arities]))
       (reduce (fn [ctx [ns-key var-name arities]]
                 (assoc-in ctx [:namespaces ns-key :vars var-name :arities] arities))
               ctx)))

(defn- post-process-namespace [ns-data]
  (-> ns-data
      (update :vars rename-vars)))

(defn- post-process-namespaces [ctx]
  (update-vals (:namespaces ctx) post-process-namespace))

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
        unify-variadic-arities
        post-process-namespaces)))
