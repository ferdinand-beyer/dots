(ns dots.adapt
  (:require [clojure.string :as str]
            [dots.util.names :as names]))

(def empty-ctx
  {:namespaces {}
   :ns-path []
   :symbol-path []})

(defn- update-ns-path [ctx f]
  (let [ns-path (f (:ns-path ctx))
        ns-key  (when (seq ns-path)
                  (str/join "." (map name ns-path)))]
    (assoc ctx :ns-path ns-path :ns-key ns-key)))

(defn- update-ns [ctx f & args]
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
      (update-ns-path pop)))

(defn- add-var [ctx var-name data]
  ;; TODO: Handle name conflicts (rename?)
  (update-ns ctx update :vars
             (fn [vars]
               (update vars var-name merge
                       data
                       {:var-name var-name
                        :order    (count vars)}))))

(defn- add-arity [ctx var-name arity op]
  (letfn [(add [arity-map]
            (cond
              (nil? arity-map)
              (-> op
                  (dissoc :args)
                  (assoc :arglists [(:args op)]))

              (not= (:op arity-map) (:op op))
              (error ::op-conflict
                     (str "Conflicting operations: "
                          (:op arity-map) " and " (:op op)))

              (not= (:module-name arity-map) (:module-name op))
              (error ::module-confict
                     (str "Conflicting module: "
                          (:module-name arity-map) " and "
                          (:module-name op)))

              (not= (:path arity-map) (:path op))
              (error ::path-confict
                     (str "Conflicting paths: "
                          (:path arity-map) " and " (:path op)))

              :else
              (update arity-map :arglists conj (:args op))))
          (error [type msg]
            (throw (ex-info msg
                            {:type type
                             :ns (:ns-key ctx)
                             :var-name var-name
                             :arity arity
                             :op op})))]
    (update-ns ctx update-in [:vars var-name :arities arity] add)))

(defmulti adapt*
  (fn [_ctx node]
    (:kind node)))

(defmethod adapt* :default
  [ctx node]
  (println "Warning: No adapt* implementation for" (:kind node))
  ctx)

(defmethod adapt* :module
  [ctx {:keys [name exports] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (vals exports))
    (leave-namespace %)))

(defmethod adapt* :variable
  [ctx {:keys [name] :as node}]
  (let [var-name        (names/cljs-name name)
        [module & path] (:symbol-path ctx)]
    ;; TODO: If the type is callable, add arities to call?
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        ;; TODO: Arity 0 - fn? Or :init/:alias?
        (add-arity var-name 0 {:op          :module-get
                               :module-name module
                               :path        (conj (vec path) name)}))))

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

(defn- add-signatures [ctx var-name signatures arity-fn]
  (reduce (fn [ctx signature]
            ;; TODO: Check for variadic params
            ;; TODO: Consider param types and docstrings
            ;; TODO: Consider return value?
            (reduce (fn [ctx args]
                      (let [{:keys [args] :as arity} (arity-fn args)]
                        (add-arity ctx var-name (count args) arity)))
                    ctx
                    (signature-arglists signature)))
          ctx
          signatures))

(defmethod adapt* :function
  [ctx {:keys [name signatures] :as node}]
  (let [var-name (names/cljs-name name)
        [module & path] (:symbol-path ctx)
        path     (conj (vec path) name)
        ctx      (add-var ctx var-name (select-keys node [:doc]))]
    (add-signatures ctx var-name signatures
                    (fn [args]
                      {:op          :module-call
                       :module-name module
                       :path        path
                       :args        args}))))

(defn- adapt-interface [ctx {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (vals members))
    (leave-namespace %)))

(defmethod adapt* :class
  [ctx node]
  ;; TODO: Look for construct signatures?
  (adapt-interface ctx node))

(defmethod adapt* :interface
  [ctx node]
  ;; TODO: Add `invoke` var when the interface has call signatures?
  (adapt-interface ctx node))

(defmethod adapt* :enum
  [ctx {:keys [name members] :as node}]
  (as-> ctx %
    (enter-namespace % name (select-keys node [:doc]))
    (reduce adapt* % (vals members))
    (leave-namespace %)))

(defmethod adapt* :enum-member
  [ctx {:keys [name] :as node}]
  (let [var-name        (names/cljs-name name)
        [module & path] (:symbol-path ctx)]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-arity var-name 0 {:op          :module-get
                               :module-name module
                               :path        (conj (vec path) name)}))))

(defmethod adapt* :type-alias
  [ctx _node]
  ctx)

(defmethod adapt* :property
  [ctx {:keys [name] :as node}]
  ;; TODO: If the type is callable, add a function?
  (let [var-name  (names/cljs-name name)
        type-name (names/cljs-name (last (:symbol-path ctx)))]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-arity var-name 1 {:op   :arg-get
                               :path [name]
                               :args [type-name]}))))

;; TODO: :get-accessor
;; TODO: :set-accessor

(defmethod adapt* :method
  [ctx {:keys [name signatures] :as node}]
  (let [var-name  (names/cljs-name name)
        type-name (names/cljs-name (last (:symbol-path ctx)))
        path      [name]
        ctx       (add-var ctx var-name (select-keys node [:doc]))]
    (add-signatures ctx var-name signatures
                    (fn [args]
                      {:op   :arg-call
                       :path path
                       :args (into [type-name] args)}))))

;; TODO: After adapting the whole module:
;; - require referenced modules
;; - exclude cljs.core names that we use

(defn adapt [module _opts]
  ;; TODO: Support ns-prefix, e.g. "dots"
  (-> empty-ctx
      (assoc :module-name (:name module))
      (adapt* module)
      :namespaces))

(comment
  (adapt* empty-ctx {:kind :module}))
