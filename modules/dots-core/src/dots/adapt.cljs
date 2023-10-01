(ns dots.adapt
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

(def empty-ctx
  {:namespaces {}
   :ns-path []
   :symbol-path []})

;; TODO: Strip `get-`, `set-`, and `is-` prefixes for functions?
;; TODO: Add `?` suffix to booleans?
;; TODO: Prefix optionals with `?`?
(defn- cljs-name [symbol-name]
  (-> symbol-name
      (str/replace "\"" "")
      (str/replace #"\W+" "_")
      csk/->kebab-case-string))

(defn- update-ns-path [ctx f]
  (let [ns-path (f (:ns-path ctx))
        ns-key  (str/join "." (map name ns-path))]
    (-> ctx
        (assoc-in [:namespaces ns-key] {:ns-path ns-path})
        (assoc :ns-path ns-path :ns-key ns-key))))

(defn- update-ns [ctx f & args]
  (apply update-in ctx [:namespaces (:ns-key ctx)] f args))

(defn- enter-namespace [ctx symbol-name ns-data]
  (-> ctx
      ;; TODO We might want to separate "symbol path" and "namespace path"?
      ;; E.g. if we don't want to mirror the hierarchy?
      (update :symbol-path conj symbol-name)
      (update-ns-path #(conj % (cljs-name symbol-name)))
      (update-ns merge ns-data)))

(defn- leave-namespace [ctx]
  (-> ctx
      (update :symbol-path pop)
      (update-ns-path pop)))

;; TODO: Generate in a second pass, when all vars are determined?
;; Together with "excludes"
(defn- add-require [ctx]
  (let [{:keys [module-name ns-alias]} ctx]
    (update-ns ctx assoc-in [:requires module-name] ns-alias)))

(defn- add-var [ctx var-name data]
  ;; TODO: Keep order (add index key)
  ;; TODO: Handle name conflicts (rename?)
  (update-ns ctx update-in [:vars var-name] merge data {:var-name var-name}))

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

(defn- object-path [ctx]
  ;; The first symbol is the module that we ns-alias.
  ;; Return the rest: the path within the module.
  (next (:symbol-path ctx)))

;; variable:     ns/obj
;; property:     (.. ns/obj -a -b -c)
;; function:     (ns/func x y z)
;; method:       (.. ns/obj -a -b (method x y z))
;; arg-property: (.-prop arg)
;; arg-method:   (.method arg x y z)
(defn- get-form
  "Returns a form to access a property in the current context."
  [ctx property-name]
  (let [ns-alias (:ns-alias ctx)
        path     (object-path ctx)]
    ;; TODO Probably easier to recur and produce: (.-c (.-b ns/a))
    ;; instead of: (.. ns/a -b -c)
    ;; Need the same for call syntax: (.c (.-b ns/a) x y z)
    ;; Or de-sugar to (. obj -member) and (. obj f x y z)?
    ;; Or return a description and delegate code generation to the emitter?
    (if (seq path)
      (list* '..
             (symbol (str ns-alias "/" (first path)))
             (map #(symbol (str "-" %))
                  (concat (next path) (list property-name))))
      (symbol (str ns-alias "/" property-name)))))

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
  (let [var-name        (cljs-name name)
        [module & path] (:symbol-path ctx)]
    ;; TODO: If the type is callable, add arities to call?
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        ;; TODO: Arity 0 - fn? Or :init/:alias?
        (add-arity var-name 0 {:op          :module-get
                               :module-name module
                               :path        (conj (vec path) name)}))))

(defmethod adapt* :function
  [ctx {:keys [name signatures] :as node}]
  (let [var-name (cljs-name name)
        [module & path] (:symbol-path ctx)
        path     (conj (vec path) name)
        ctx      (add-var ctx var-name (select-keys node [:doc]))]
    (reduce (fn [ctx {:keys [params]}]
              ;; TODO: Check for optional params
              ;; TODO: Check for variadic params
              ;; TODO: Consider param types and docstrings
              ;; TODO: Consider return value?
              (let [args (mapv (comp cljs-name :name) params)]
                (add-arity ctx var-name (count args)
                           {:op          :module-call
                            :module-name module
                            :path        path
                            :args        args})))
            ctx
            signatures)))

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
  (let [var-name        (cljs-name name)
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
  (let [var-name  (cljs-name name)
        type-name (cljs-name (last (:symbol-path ctx)))]
    (-> ctx
        (add-var var-name (select-keys node [:doc]))
        (add-arity var-name 1 {:op   :arg-get
                               :path [name]
                               :args [type-name]}))))

;; TODO: :get-accessor
;; TODO: :set-accessor

(defmethod adapt* :method
  [ctx {:keys [name signatures] :as node}]
  (let [var-name  (cljs-name name)
        type-name (cljs-name (last (:symbol-path ctx)))
        path      [name]
        ctx       (add-var ctx var-name (select-keys node [:doc]))]
    (reduce (fn [ctx {:keys [params]}]
                ;; TODO: Check for optional params
                ;; TODO: Check for variadic params
                ;; TODO: Consider param types and docstrings
                ;; TODO: Consider return value?
              (let [args (into [type-name] (map (comp cljs-name :name)) params)]
                (add-arity ctx var-name (count args)
                           {:op   :arg-call
                            :path path
                            :args args})))
            ctx
            signatures)))

(defn adapt [module]
  (-> empty-ctx
      (assoc :module-name (:name module)
             :ns-alias (cljs-name (:name module)))
      (adapt* module)
      :namespaces))

(comment
  (adapt* empty-ctx {:kind :module})

  )
