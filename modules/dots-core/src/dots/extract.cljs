(ns dots.extract
  "Extract module exports using the TypeScript compiler."
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-clause :as import-clause]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.module-kind :as module-kind]
            [dots.typescript.namespace-import :as namespace-import]
            [dots.typescript.program :as program]
            [dots.typescript.script-kind :as script-kind]
            [dots.typescript.script-target :as script-target]
            [dots.typescript.signature :as signature]
            [dots.typescript.signature-kind :as signature-kind]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.symbol-flags :as symbol-flags]
            [dots.typescript.type :as type]
            [dots.typescript.type-checker :as type-checker]
            [dots.typescript.type-flags :as type-flags]
            [dots.util.names :as names]
            [dots.util.table :as table]))

(def ^:dynamic *debug?* false)

(def ^:private proxy-file-name "dots$proxy.ts")

(defn- proxy-file-name? [file-name]
  (= file-name proxy-file-name))

(defn- proxy-source-text
  "Creates source code for a proxy TypeScript source file."
  [module-name {:keys [default?]}]
  (str/join (list "import "
                  (if default? "" "* as ")
                  "dots$imported from \""
                  module-name
                  "\";\n")))

(defn- proxy-compiler-host
  "Creates a CompilerHost that resolves our special proxy file."
  [compiler-opts module-name opts]
  (let [parent-nodes? false
        script-kind script-kind/ts
        host        (ts/create-compiler-host compiler-opts parent-nodes?)
        source-text (proxy-source-text module-name opts)]
    (letfn [(file-exists [file-name]
              (or (proxy-file-name? file-name)
                  (.fileExists host file-name)))
            (get-source-file [file-name target-or-opts on-error create?]
              (if (proxy-file-name? file-name)
                (ts/create-source-file proxy-file-name source-text target-or-opts parent-nodes? script-kind)
                (.getSourceFile host file-name target-or-opts on-error create?)))]
      (.assign js/Object #js {} host #js {:fileExists    file-exists
                                          :getSourceFile get-source-file}))))

(def compiler-opts
  (.assign js/Object
           #js {}
           (ts/default-compiler-options)
           #js {:allowJs true
                :strict true
                :module module-kind/es-2015
                :target script-target/es-next}))

(defn- create-program
  [module-name opts]
  (let [host (proxy-compiler-host compiler-opts module-name opts)]
    (ts/create-program #js [proxy-file-name] compiler-opts host)))

(defn- import-identifier
  "Returns the identifier node representing the module import."
  [program]
  (let [source-file   (program/get-source-file program proxy-file-name)
        import-clause (import-declaration/import-clause
                       (first (source-file/statements source-file)))]
    (or (import-clause/name import-clause)
        (namespace-import/name
         (import-clause/named-bindings import-clause)))))

(defn- imported-module-symbol [program]
  (let [checker (program/get-type-checker program)]
    (->> (import-identifier program)
         (type-checker/symbol-at-location checker)
         (type-checker/aliased-symbol checker))))

;; FLAGS

(defn- has? [flags test]
  (not= 0 (bit-and flags test)))

;; TYPE

(defn- debug-type [{:keys [type-checker]} type]
  (let [sym (type/symbol type)
        fqn (some->> (type/symbol type)
                     (type-checker/fully-qualified-name type-checker))]
    (cond-> {:str   (type-checker/type-to-string type-checker type)
             :flags (type/flags type)
             :symbol (when sym
                       {:name (symbol/name sym)
                        :flags (symbol/flags sym)})}
      (some? fqn) (assoc :fqn fqn)
      (type/literal? type) (assoc :value (type/value type))
      (type/class-or-interface? type) (assoc :object-flags (type/object-flags type)))))

(defn- extract-type [env type]
  (let [checker (:type-checker env)
        flags   (type/flags type)]
    (cond-> {:str (type-checker/type-to-string checker type)}
      (has? flags type-flags/any) (assoc :any? true)
      (has? flags type-flags/string) (assoc :primitive :string)
      (has? flags type-flags/number) (assoc :primitive :number)
      (has? flags type-flags/boolean) (assoc :primitive :boolean)
      (has? flags type-flags/undefined) (assoc :primitive :undefined)
      *debug?* (assoc :debug/type-flags flags))))

(defn- extract-symbol-type [env sym]
  (let [checker (:type-checker env)]
    (extract-type env (type-checker/type-of-symbol checker sym))))

;; SYMBOL

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(declare extract-symbol)

(defn- add-members [data k env syms]
  (cond-> data
    (seq syms)
    (update k (fn [table]
                (reduce (fn [table sym]
                          (let [data (extract-symbol env sym)]
                            (table/tassoc table (:name data) data)))
                        table
                        syms)))))

(defn- add-type-members [data env type]
  (let [checker (:type-checker env)
        syms    (type-checker/properties-of-type checker type)]
    (add-members data :members env syms)))

;; TODO: Represent types as Clojure data (hiccup)?
;; At least understand some basics:
;; - primitives (type flags)
;; - nullable types - same as (X | undefined)?
;; - union types (A | B) - "either or"
;; - intersection types (A & B) - "all of"
;; - references to types defined here
;;   (class, interface, type alias, alias (?))
(defn- debug-types [{:keys [type-checker] :as env} sym]
  (-> {:type          (type-checker/type-of-symbol type-checker sym)
       :declared-type (type-checker/declared-type-of-symbol type-checker sym)}
      (update-vals #(extract-type env %))))

(defn- extract-parameter [env sym]
  (let [checker (:type-checker env)
        decl    (symbol/value-declaration sym)]
    (-> (extract-symbol env sym)
        (assoc :type (extract-symbol-type env sym))
        (cond->
         (type-checker/optional-parameter? checker decl) (assoc :optional? true)
         (ts/rest-parameter? decl) (assoc :rest? true)))))

(defn- extract-signature [env sig]
  (let [checker     (:type-checker env)
        return-type (type-checker/return-type-of-signature checker sig)]
    (cond-> {:params      (map #(extract-parameter env %) (signature/parameters sig))
             :return-type (extract-type env return-type)})))

(defn- extract-signatures
  ([env type]
   (extract-signatures env type signature-kind/call))
  ([env type kind]
   (let [checker (:type-checker env)]
     (for [sig (type-checker/signatures-of-type checker type kind)]
       (extract-signature env sig)))))

(defn- add-signatures [data env type]
  (update data :signatures into (extract-signatures env type)))

(defn- extract-property [data env sym]
  (-> data
      (update :traits conj :property)
      (assoc :type (extract-symbol-type env sym))
      (cond-> (has? (symbol/flags sym) symbol-flags/optional)
        (assoc :optional? true))))

(defn- extract-method [data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> data
        (update :traits conj :method)
        (add-signatures env type))))

(defn- extract-get-accessor [data _env _sym]
  (update data :traits conj :get-accessor))

(defn- extract-set-accessor [data _env _sym]
  (update data :traits conj :set-accessor))

(defn- extract-variable [data env sym]
  (-> data
      (update :traits conj :variable)
      (assoc :type (extract-symbol-type env sym))))

(defn- extract-function [data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> data
        (update :traits conj :function)
        (add-signatures env type))))

(defn- extract-class-or-interface
  [trait data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/declared-type-of-symbol checker sym)]
    (-> data
        (update :traits conj trait)
        (add-type-members env type)
        (add-signatures env type))))

(defn- extract-class [data env sym]
  (let [table (symbol/exports sym)]
    (-> (extract-class-or-interface :class data env sym)
        ;; Static members
        (add-members :exports env (.values ^js table)))))

(defn- extract-interface [data env sym]
  (extract-class-or-interface :interface data env sym))

(defn- extract-enum-member [data _env _sym]
  (update data :traits conj :enum-member))

(defn- extract-enum [data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> data
        (update :traits conj :enum)
        (add-type-members env type))))

(defn- extract-type-alias [data _env _sym]
  (update data :traits conj :type-alias))

(defn- extract-alias [data _env _sym]
  (update data :traits conj :alias))

(defn- extract-module [data env sym]
  (let [checker (:type-checker env)
        syms    (type-checker/exports-of-module checker sym)]
    (-> data
        (update :traits conj :module)
        (add-members :exports env syms))))

(defn- symbol-common [env sym]
  (let [checker (:type-checker env)
        doc-str (doc-string sym)
        sym-name (symbol/name sym)]
    (cond-> {:name   (type-checker/symbol-to-string checker sym)
             :traits #{}
             ;; TODO: Only for types, so that we can reference them?
             ;; TODO: Register in the environment? Allow to resolve types
             :fqn    (type-checker/fully-qualified-name checker sym)}
      (names/internal? sym-name) (assoc :internal? true)
      doc-str  (assoc :doc doc-str)
      *debug?* (assoc :debug/name  sym-name
                      :debug/flags (symbol/flags sym)
                      :debug/types (debug-types env sym)))))

(defn- amend-symbol [data env sym]
  (let [flags (symbol/flags sym)]
    (cond-> data
      (has? flags symbol-flags/variable) (extract-variable env sym)
      (has? flags symbol-flags/function) (extract-function env sym)
      (has? flags symbol-flags/class) (extract-class env sym)
      (has? flags symbol-flags/interface) (extract-interface env sym)
      (has? flags symbol-flags/enum) (extract-enum env sym)
      (has? flags symbol-flags/enum-member) (extract-enum-member env sym)
      (has? flags symbol-flags/module) (extract-module env sym)
      (has? flags symbol-flags/alias) (extract-alias env sym)
      (has? flags symbol-flags/type-alias) (extract-type-alias env sym)
      (has? flags symbol-flags/property) (extract-property env sym)
      (has? flags symbol-flags/method) (extract-method env sym)
      (has? flags symbol-flags/get-accessor) (extract-get-accessor env sym)
      (has? flags symbol-flags/set-accessor) (extract-set-accessor env sym))))

(defn- extract-symbol [env sym]
  (-> (symbol-common env sym)
      (amend-symbol env sym)))

(defn extract [module-name opts]
  (let [program (create-program module-name opts)
        symbol  (imported-module-symbol program)
        env     {:type-checker (program/get-type-checker program)}]
    ;; TODO: The imported symbol is a variable => extract its type
    ;; For example, the "path" module exports the `PlatformPath`
    ;; interface
    (-> (extract-symbol env symbol)
        ;; TODO Hack to set requires correctly
        ;; We should keep track of how we imported the module,
        ;; to emit correct :require code.  However, we should
        ;; not change module names, because we could use them to
        ;; resolve fully-qualified names
        (assoc :name (str "\"" module-name "\"")))))
