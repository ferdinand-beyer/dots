(ns dots.adapt
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

(def empty-ctx
  {:namespaces {}
   :ns-path []
   :symbol-path []})

(defn- cljs-name [symbol-name]
  (-> symbol-name
      (str/replace "\"" "")
      (str/replace #"\W+" "_")
      csk/->kebab-case-symbol))

(defn- update-ns-path [ctx f]
  (let [ns-path (f (:ns-path ctx))
        ns-key  (symbol (str/join "." (map name ns-path)))]
    (assoc ctx :ns-path ns-path :ns-key ns-key)))

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

(defn- add-require [ctx]
  (let [{:keys [module-name ns-alias]} ctx]
    (update-ns ctx assoc-in [:requires module-name] ns-alias)))

(defn- add-var [ctx var-name data]
  ;; TODO: Keep order
  ;; TODO: Handle name conflicts (rename?)
  (update-ns ctx update-in [:vars var-name] merge data))

(defn- object-path [ctx]
  ;; The first symbol is the module that we ns-alias.
  ;; Return the rest: the path within the module.
  (next (:symbol-path ctx)))

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
  (let [var-name (cljs-name name)
        var-data (merge (select-keys node [:doc])
                        {:kind :def
                         :init-form (get-form ctx name)})]
    (-> ctx
        (add-require)
        (add-var var-name var-data))))

(defmethod adapt* :function
  [ctx node]
  ;; TODO: Add a require
  ;; TODO: Add a defn
  ;; Just create arglists for each signature,
  ;; and emit code to apply max number of args?
  ctx)

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
  (let [var-name (cljs-name name)
        var-data (merge (select-keys node [:doc])
                        {:kind :def
                         :init-form (get-form ctx name)})]
    (-> ctx
        (add-require)
        (add-var var-name var-data))))

(defmethod adapt* :type-alias
  [ctx _node]
  ctx)

(defmethod adapt* :property
  [ctx node]
  ;; TODO: If the type is callable, add a function?
  ctx)

(defmethod adapt* :method
  [ctx node]
  ctx)

(defn adapt [module]
  (-> empty-ctx
      (assoc :module-name (:name module)
             :ns-alias (cljs-name (:name module)))
      (adapt* module)
      :namespaces))

(comment
  (adapt* empty-ctx {:kind :module})

  )
