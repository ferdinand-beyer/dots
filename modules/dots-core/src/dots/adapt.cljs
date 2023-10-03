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

(defn- update-ns-path [ctx f]
  (let [ns-path (f (:ns-path ctx))
        ns-key  (when (seq ns-path)
                  (str/join "." (map name ns-path)))]
    (assoc ctx :ns-path ns-path :ns-key ns-key)))

(defn- update-ns [ctx f & args]
  {:pre [(:ns-key ctx)]}
  (apply update-in ctx [:namespaces (:ns-key ctx)] f args))

(defn- assoc-ns-path [ctx]
  (update-ns ctx assoc :ns-path (:ns-path ctx)))

(defn- enter-namespace [ctx symbol-name ns-data]
  (-> ctx
      ;; TODO We might want to separate "symbol path" and "namespace path"?
      ;; E.g. if we don't want to mirror the hierarchy?
      (update :symbol-path conj symbol-name)
      (update-ns-path #(conj % (names/cljs-name symbol-name)))
      (assoc-ns-path)
      (update-ns merge ns-data)))

(defn- leave-namespace [ctx]
  (-> ctx
      (update :symbol-path pop)
      ;; TODO: Keep a separate stack of mapped ns-paths,
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
      (if (and (neg? arity)
               (not= (count (:args existing))
                     (count (:args expr))))
        {:type ::variadic-conflict
         :message "Different number of required arguments in variadic arity"}
        (expr-conflict existing expr)))))

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

(defn- signature-arglists [{:keys [params]}]
  (let [[req opt] (split-with (complement :optional?) params)
        args (into [] (map (comp names/cljs-name :name)) req)]
    (loop [arglists [args]
           args args
           opt opt]
      (if-let [arg (first opt)]
        (let [args (conj args (names/cljs-name (:name arg)))]
          (recur (conj arglists args) args (next opt)))
        arglists))))

(defn- add-signatures [ctx var-name signatures expr-fn]
  (reduce (fn [ctx signature]
            ;; TODO: Check for variadic params
            ;; TODO: Consider param types and docstrings
            ;; TODO: Consider return value?
            (reduce (fn [ctx args]
                      (let [{:keys [args] :as expr} (expr-fn args)]
                        (add-arity ctx var-name (count args) expr)))
                    ctx
                    (signature-arglists signature)))
          ctx
          signatures))

(defmulti adapt-trait
  (fn [_ctx trait _node]
    trait))

(defn- adapt* [ctx node]
  (reduce (fn [ctx trait]
            (adapt-trait ctx trait node))
          ctx
          (:traits node)))

(defmethod adapt-trait :default
  [ctx trait _node]
  (println "Warning: No adapt-trait implementation for" trait)
  ctx)

(defmethod adapt-trait :module
  [ctx _ {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (table/tvals members))
    (leave-namespace %)))

(defmethod adapt-trait :variable
  [ctx _ {:keys [name] :as node}]
  (let [var-name        (names/cljs-name name)
        [module & path] (:symbol-path ctx)]
    ;; TODO: If the type is callable, add arities to call?
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        ;; TODO: Add :init expr for consts?
        (add-arity var-name 0 {:op          :global-get
                               :module-name module
                               :path        (conj (vec path) name)}))))

(defmethod adapt-trait :function
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        [module & path] (:symbol-path ctx)
        path     (conj (vec path) name)
        ctx      (add-var ctx var-name (select-keys node [:doc]))]
    (add-signatures ctx var-name signatures
                    (fn [args]
                      {:op          :global-call
                       :module-name module
                       :path        path
                       :args        args}))))

(defn- adapt-interface [ctx {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (table/tvals members))
    (leave-namespace %)))

(defmethod adapt-trait :class
  [ctx _ node]
  ;; TODO: Look for construct signatures?
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
        [module & path] (:symbol-path ctx)]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-init var-name {:op          :global-get
                            :module-name module
                            :path        (conj (vec path) name)}))))

(defmethod adapt-trait :type-alias
  [ctx _ _node]
  ;; TODO
  ctx)

;; TODO: For class/interface members, check if the name is a valid identifier,
;; to exclude/handle "indirect" names such as `[Symbol.iterator]`
;; TODO: Clojurify names, e.g. '?' suffix for booleans, remove `is-` and `get-` prefixes
;; But then: Handle collisions, e.g. "name" and "getName" (maybe use `-name` for property access?)

(defmethod adapt-trait :property
  [ctx _ {:keys [name] :as node}]
  ;; TODO: If the type is callable, add a function?
  (let [var-name  (names/cljs-name name)
        type-name (names/cljs-name (last (:symbol-path ctx)))]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-arity var-name 1 {:op   :arg-get
                               :path [name]
                               :args [type-name]}))))

(defmethod adapt-trait :get-accessor
  [ctx _ _node]
  ;; TODO: Like :property?
  ctx)

(defmethod adapt-trait :set-accessor
  [ctx _ _node]
  ;; TODO: :arg-set, use `set-<name>!` for name?
  ctx)

(defmethod adapt-trait :method
  [ctx _ {:keys [name signatures] :as node}]
  (let [var-name  (names/cljs-name name)
        type-name (names/cljs-name (last (:symbol-path ctx)))
        path      [name]
        ctx       (add-var ctx var-name (select-keys node [:doc]))]
    (add-signatures ctx var-name signatures
                    (fn [args]
                      {:op   :arg-call
                       :path path
                       :args (into [type-name] args)}))))

(defn adapt [module _opts]
  ;; TODO: Support ns-prefix, e.g. "dots"
  ;; And/or a function: symbol-path => ns-path
  (-> empty-ctx
      (adapt* module)
      :namespaces))
