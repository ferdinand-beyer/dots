(ns dots.extract
  "Extract module exports using the TypeScript compiler."
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-clause :as import-clause]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.modifier-flags :as modifier-flags]
            [dots.typescript.namespace-import :as namespace-import]
            [dots.typescript.object-flags :as object-flags]
            [dots.typescript.program :as program]
            [dots.typescript.script-kind :as script-kind]
            [dots.typescript.signature :as signature]
            [dots.typescript.signature-kind :as signature-kind]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.symbol-flags :as symbol-flags]
            [dots.typescript.type :as type]
            [dots.typescript.union-or-intersection-type :as union-or-intersection-type]
            [dots.typescript.literal-type :as literal-type]
            [dots.typescript.type-checker :as type-checker]
            [dots.typescript.type-flags :as type-flags]
            [dots.typescript.object-type :as object-type]
            [dots.typescript.type-reference :as type-reference]
            [dots.util.names :as names]
            [dots.util.table :as table]))

(def ^:dynamic *debug?* false)

(def ^:private proxy-file-name "dots$proxy.ts")

(defn- proxy-file-name? [file-name]
  (= file-name proxy-file-name))

;; import defaultExport, * as name from "module-name";
;; [
;;   factory.createImportDeclaration(
;;     undefined,
;;     factory.createImportClause(
;;       false,
;;       factory.createIdentifier("defaultExport"),
;;       factory.createNamespaceImport(
;;         factory.createIdentifier("name")
;;       )
;;     ),
;;     factory.createStringLiteral("module-name"),
;;     undefined
;;   )
;; ];

(defn- proxy-source-text
  "Creates source code for a proxy TypeScript source file."
  [module-name {:keys [default?]}]
  ;; Should we support the default export?
  ;; ts:   import defaultExport, * as name from "module-name";
  ;; cljs: (:require ["module-name" :as name] ["module-name$default" :as defaultExport])
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
                :strict  true}))

(defn- create-program
  [module-name opts]
  (let [host (proxy-compiler-host compiler-opts module-name opts)]
    (ts/create-program #js [proxy-file-name] compiler-opts host)))

(defn- import-identifier
  "Returns the identifier node representing the module import."
  [program]
  (let [source-file   (program/source-file program proxy-file-name)
        import-clause (import-declaration/import-clause
                       (first (source-file/statements source-file)))]
    (or (import-clause/name import-clause)
        (namespace-import/name
         (import-clause/named-bindings import-clause)))))

(defn- imported-module-symbol [program]
  (let [checker (program/type-checker program)]
    (->> (import-identifier program)
         (type-checker/symbol-at-location checker)
         (type-checker/aliased-symbol checker))))

(defn- has? [flags test]
  (not= 0 (bit-and flags test)))

;;-------------------------------------------------------------------------------------------------
;; Type

(declare extract-type)

(defn- fqn [env sym]
  (-> (:type-checker env)
      (type-checker/fully-qualified-name sym)
      (names/split-fqn)))

(defn- extract-type-reference [props env type]
  (let [target  (type-reference/target type)]
    (if (identical? type target)
      props
      (let [checker (:type-checker env)
            args    (type-checker/type-arguments checker type)]
        (cond-> (assoc props :reference (extract-type env target))
          (seq args) (assoc :args (mapv #(extract-type env %) args)))))))

;; TODO: Unify with extract-class-or-interface?
(defn- extract-class-or-interface-type [props env type]
  (let [sym (type/symbol type)]
    (-> props
        (assoc :class-or-interface? true)
        (assoc (if (type/class? type) :class? :interface?) true)
        (cond->
         sym      (assoc :fqn (fqn env sym))
         *debug?* (assoc :debug/object-flags (object-type/object-flags type))))))

(defn- extract-object-type [props env type]
  (let [flags (object-type/object-flags type)]
    (-> props
        (assoc :object? true)
        (cond->
         (type/class-or-interface? type) (extract-class-or-interface-type env type)
         (has? flags object-flags/reference) (extract-type-reference env type)
         (type-checker/array-type? (:type-checker env) type) (assoc :array? true)))))

(def ^:private type-flag-keys
  [:any? :string? :number? :boolean? :enum? :void? :undefined? :null? :object? :array?])

(defn- merge-union-type [props types]
  (apply merge props (map #(select-keys % type-flag-keys) types)))

(defn- maybe-boolean-union [props component-types]
  ;; `boolean | undefined` is (sometimes?) reported as `undefined | true | false`
  (let [literals (into #{} (keep :literal) component-types)]
    (cond-> props
      (and (contains? literals true)
           (contains? literals false))
      (assoc :boolean? true))))

(defn- extract-union-or-intersection-type [props env type]
  (let [component-types (mapv #(extract-type env %) (union-or-intersection-type/types type))]
    (if (type/union? type)
      (-> props
          (merge-union-type component-types)
          (maybe-boolean-union component-types)
          (assoc :union component-types))
      (assoc props :intersection component-types))))

(defn- extract-enum-type [props env type]
  (let [sym (type/symbol type)]
    ;; TODO: What's the difference between `enum` and `enum-literal`?
    ;; A enum type gets `enum-literal` + `union` flags...
    (cond-> (assoc props :enum? true)
      sym (assoc :fqn (fqn env sym)))))

(defn- literal-value [type flags]
  (let [value (literal-type/value type)]
    (if (has? flags type-flags/boolean-literal)
      (parse-boolean value)
      value)))

(defn- extract-type* [env type]
  (let [checker (:type-checker env)
        flags   (type/flags type)]
    (cond-> {:str (type-checker/type-to-string checker type)}
      ;; TypeScript AST viewer reports that there is a Primitive flag, but the d.ts does not contain it?
      ;; Some of these flags are mutually exclusive and could be checked using a mask and =?
      (has? flags type-flags/any) (assoc :any? true)
      (has? flags type-flags/unknown) (assoc :unknown? true)
      (has? flags type-flags/string) (assoc :primitive :string, :string? true)
      (has? flags type-flags/number) (assoc :primitive :number, :number? true)
      (has? flags type-flags/boolean) (assoc :primitive :boolean, :boolean? true)
      ;; TODO: BooleanLiteral is not considered type/literal?
      (has? flags type-flags/boolean-literal) (as-> % (assoc % :literal (parse-boolean (:str %))))
      (has? flags type-flags/enum-like) (extract-enum-type env type)
      (has? flags type-flags/void) (assoc :primitive :void, :void? true)
      (has? flags type-flags/undefined) (assoc :primitive :undefined, :undefined? true)
      (has? flags type-flags/null) (assoc :null? true)
      (has? flags type-flags/never) (assoc :never? true)
      (has? flags type-flags/type-parameter) (assoc :param? true)
      (has? flags type-flags/object) (extract-object-type env type)
      (type/literal? type) (assoc :literal (literal-value type flags))
      (type/union-or-intersection? type) (extract-union-or-intersection-type env type)
      *debug?* (assoc :debug/type-flags flags))))

(defn- extract-type [env type]
  (let [cache (:types* env)]
    (if-let [data (get @cache type)]
      (if (keyword-identical? ::pending data)
        (let [sym (type/symbol type)]
          (cond-> {:circular? true
                   :str (type-checker/type-to-string (:type-checker env) type)}
            sym (assoc :fqn (fqn env sym))))
        data)
      (do
        (swap! cache assoc type ::pending)
        (let [data (extract-type* env type)]
          (swap! cache assoc type data)
          data)))))

(defn- extract-symbol-type [env sym]
  (let [checker (:type-checker env)]
    (extract-type env (type-checker/type-of-symbol checker sym))))

;;-------------------------------------------------------------------------------------------------
;; Symbol

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(defn- modifier-flags [sym]
  (ts/combined-modifier-flags (symbol/value-declaration sym)))

(declare extract-symbol)

(defn- add-members [data k env syms]
  (cond-> data
    (seq syms) (update k (fn [table]
                           (reduce (fn [table sym]
                                     (let [data (extract-symbol env sym)]
                                       (table/tassoc table (:name data) data)))
                                   table
                                   syms)))))

(defn- add-type-properties [data env type]
  (let [checker (:type-checker env)
        syms    (type-checker/properties-of-type checker type)]
    (add-members data :members env syms)))

(defn- extract-parameter [env sym]
  (let [checker (:type-checker env)
        decl    (symbol/value-declaration sym)
        type    (extract-symbol-type env sym)]
    (-> (extract-symbol env sym)
        (assoc :type type)
        (cond->
         (or (:undefined? type)
             (type-checker/optional-parameter? checker decl)) (assoc :optional? true)
         (ts/rest-parameter? decl) (assoc :rest? true)))))

(defn- extract-signature [env sig]
  (let [checker     (:type-checker env)
        return-type (type-checker/return-type-of-signature checker sig)]
    (cond-> {:params      (map #(extract-parameter env %) (signature/parameters sig))
             :return-type (extract-type env return-type)})))

(defn- extract-signatures [env type kind]
  (let [checker (:type-checker env)]
    (for [sig (type-checker/signatures-of-type checker type kind)]
      (extract-signature env sig))))

(defn- add-call-signatures [data env type]
  (let [signatures (extract-signatures env type signature-kind/call)]
    (cond-> data
      (seq signatures) (update :signatures (fnil into []) signatures))))

(defn- add-construct-signatures [data env type]
  (let [signatures (extract-signatures env type signature-kind/construct)]
    (cond-> data
      (seq signatures) (update :construct-signatures (fnil into []) signatures))))

;; TODO: Find out if properties and variables are readonly.
;; The TypeChecker has a `isReadonlySymbol` function but unfortunately, it is private.
;; We might be able to walk the AST to find:
;; - Readonly modifier (getCombinedModifierFlags)
;; - Const keyword (a bit trickier, because this is outside of the valueDeclaration)
;; We should generate setters for non-readonly properties.  We can handle get-accessors
;; and set-accessors in classes similar to properties in interfaces.
(defn- extract-property [data env sym]
  (-> data
      (update :traits conj :property)
      (assoc :type (extract-symbol-type env sym))
      (cond->
       (has? (symbol/flags sym) symbol-flags/optional)     (assoc :optional? true)
       (has? (modifier-flags sym) modifier-flags/readonly) (assoc :const? true))))

(defn- extract-method [data env sym]
  (extract-property data env sym)
  (let [checker (:type-checker env)
        type    (->> (type-checker/type-of-symbol checker sym)
                     ;; Methods can be optional, too
                     (type-checker/non-nullable-type checker))]
    (-> data
        (update :traits conj (if (has? (modifier-flags sym) modifier-flags/static)
                               ;; Treat static class members as module functions
                               :function
                               :method))
        (add-call-signatures env type)
        (cond->
         (has? (symbol/flags sym) symbol-flags/optional) (assoc :optional? true)))))

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
        (add-call-signatures env type))))

(defn- extract-class-or-interface
  [trait data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/declared-type-of-symbol checker sym)]
    (-> data
        (update :traits conj trait)
        (add-type-properties env type)
        (add-call-signatures env type))))

(defn- extract-class [data env sym]
  (let [checker     (:type-checker env)
        type        (type-checker/type-of-symbol checker sym)
        class-fqn   (fqn env sym)
        static-syms (->> (type-checker/properties-of-type checker type)
                         ;; Every class has a prototype property.
                         (remove #(has? (symbol/flags %) symbol-flags/prototype))
                         ;; No inherited exports.
                         (filter #(= class-fqn (take (count class-fqn) (fqn env %)))))]
    (-> (extract-class-or-interface :class data env sym)
        (add-construct-signatures env type)
        (cond-> (seq static-syms)
          ;; Treat statics as module
          (-> (update :traits conj :module)
              (add-members :exports env static-syms))))))

(defn- extract-interface [data env sym]
  (extract-class-or-interface :interface data env sym))

(defn- extract-enum-member [data env sym]
  (-> data
      (update :traits conj :enum-member)
      (assoc :type (extract-symbol-type env sym))))

(defn- extract-enum [data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> data
        (update :traits conj :enum)
        (add-type-properties env type))))

(defn- extract-type-alias [data env sym]
  (let [checker (:type-checker env)]
    (merge (extract-type env (type-checker/declared-type-of-symbol checker sym))
           (update data :traits conj :type-alias))))

(defn- extract-alias [data _env _sym]
  (update data :traits conj :alias))

(defn- extract-module [data env sym]
  (let [checker (:type-checker env)
        syms    (type-checker/exports-of-module checker sym)]
    (-> data
        (update :traits conj :module)
        (add-members :exports env syms))))

(defn- symbol-common [env sym]
  (let [checker  (:type-checker env)
        doc-str  (doc-string sym)
        sym-name (symbol/name sym)]
    (cond-> {:name   (type-checker/symbol-to-string checker sym)
             :traits #{}}
      (names/internal? sym-name) (assoc :internal? true)
      doc-str  (assoc :doc doc-str)
      *debug?* (assoc :debug/name  sym-name
                      :debug/flags (symbol/flags sym)))))

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

(defn- extract-symbol* [env sym]
  (-> (symbol-common env sym)
      (amend-symbol env sym)))

(defn- extract-symbol [env sym]
  (let [cache (:symbols* env)]
    (if-let [data (get @cache sym)]
      (if (keyword-identical? ::pending data)
        (let [fqn (fqn env sym)]
          (println "Warning: Circular symbol dependency" fqn)
          {:fqn       fqn
           :circular? true})
        data)
      (do
        (swap! cache assoc sym ::pending)
        (let [data (extract-symbol* env sym)]
          (swap! cache assoc sym data)
          data)))))

(defn extract [module-name opts]
  (let [program (create-program module-name opts)
        symbol  (imported-module-symbol program)
        env     {:type-checker (program/type-checker program)
                 :symbols*     (atom {})
                 :types*       (atom {})}]
    ;; TODO: The imported symbol is a variable => extract its type
    ;; For example, the "path" module exports the `PlatformPath`
    ;; interface
    (-> (extract-symbol env symbol)
        (assoc :import-name module-name))))
