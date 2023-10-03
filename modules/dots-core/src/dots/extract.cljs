(ns dots.extract
  "Extract module exports using the TypeScript compiler."
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-clause :as import-clause]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.namespace-import :as namespace-import]
            [dots.typescript.program :as program]
            [dots.typescript.signature :as signature]
            [dots.typescript.signature-kind :as signature-kind]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.symbol-flags :as symbol-flags]
            [dots.typescript.type :as type]
            [dots.typescript.type-checker :as type-checker]
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
  (let [host        (ts/create-compiler-host compiler-opts true)
        source-text (proxy-source-text module-name opts)]
    (letfn [(file-exists [file-name]
              (or (proxy-file-name? file-name)
                  (.fileExists host file-name)))
            (get-source-file [file-name target-or-opts on-error create?]
              (if (proxy-file-name? file-name)
                (ts/create-source-file proxy-file-name source-text target-or-opts true)
                (.getSourceFile host file-name target-or-opts on-error create?)))]
      (.assign js/Object #js {} host #js {:fileExists    file-exists
                                          :getSourceFile get-source-file}))))

(defn- create-program
  [module-name {:keys [compiler-opts] :as opts}]
  (let [compiler-opts (or compiler-opts
                          (ts/default-compiler-options))
        host (proxy-compiler-host compiler-opts module-name opts)]
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

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(declare extract-symbol)

(defn- add-members [data env syms]
  (cond-> data
    (seq syms) (update :members (fn [members]
                                  (reduce (fn [members sym]
                                            (let [d (extract-symbol env sym)]
                                              (table/tassoc members (:name d) d)))
                                          members
                                          syms)))))

(defn- debug-type [{:keys [type-checker]} type]
  (let [fqn (some->> (type/symbol type)
                     (type-checker/fully-qualified-name type-checker))]
    (cond-> {:str   (type-checker/type-to-string type-checker type)
             :flags (type/flags type)}
      (some? fqn) (assoc :fqn fqn)
      (type/literal? type) (assoc :value (type/value type))
      (type/class-or-interface? type) (assoc :object-flags (type/object-flags type)))))

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
      (update-vals #(debug-type env %))))

(defn- extract-parameter [env sym]
  (let [checker (:type-checker env)
        decl    (symbol/value-declaration sym)]
    (-> (extract-symbol env sym)
        (assoc :optional? (type-checker/optional-parameter? checker decl)))))

(defn- extract-signature [env sig]
  (let [checker     (:type-checker env)
        return-type (type-checker/return-type-of-signature checker sig)]
    (cond-> {:params (map #(extract-parameter env %) (signature/parameters sig))}
      *debug?* (assoc :debug/return-type (debug-type env return-type)))))

(defn- extract-signatures
  ([env type]
   (extract-signatures env type signature-kind/call))
  ([env type kind]
   (let [checker (:type-checker env)]
     (for [sig (type-checker/signatures-of-type checker type kind)]
       (extract-signature env sig)))))

(defn- add-signatures [data env type]
  (update data :signatures into (extract-signatures env type)))

(defn- extract-property [data _env _sym]
  (update data :traits conj :property))

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

(defn- extract-variable [data _env _sym]
  (update data :traits conj :variable))

(defn- extract-function [data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> data
        (update :traits conj :function)
        (add-signatures env type))))

(defn- add-type-members [data env type]
  (let [checker (:type-checker env)
        syms    (type-checker/properties-of-type checker type)]
    (add-members data env syms)))

(defn- extract-class-or-interface
  [trait data env sym]
  (let [checker (:type-checker env)
        type    (type-checker/declared-type-of-symbol checker sym)]
    (-> data
        (update :traits conj trait)
        (add-type-members env type)
        (add-signatures env type))))

(defn- extract-class [data env sym]
  (extract-class-or-interface :class data env sym))

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
        (add-members env syms))))

(defn- common [env sym]
  (let [checker (:type-checker env)
        doc-str (doc-string sym)]
    (cond-> {:name (symbol/name sym)
             :traits #{}
             ;; TODO: Only for types, so that we can reference them?
             ;; TODO: Register in the environment? Allow to resolve types
             :fqn  (type-checker/fully-qualified-name checker sym)}
      doc-str  (assoc :doc doc-str)
      *debug?* (assoc :debug/types (debug-types env sym)
                      :debug/flags (symbol/flags sym)))))

(defn- has? [flags test]
  (not= 0 (bit-and flags test)))

(defn- extract-symbol [env sym]
  (let [flags (symbol/flags sym)]
    (cond-> (common env sym)
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

(defn extract [module-name opts]
  (let [program (create-program module-name opts)
        symbol  (imported-module-symbol program)
        env     {:type-checker (program/get-type-checker program)}]
    ;; TODO: The imported symbol is a variable => extract its type
    ;; For example, the "path" module exports the `PlatformPath`
    ;; interface
    (-> (extract-symbol env symbol)
        ;; TODO Hack to set requires correctly
        (assoc :name (str "\"" module-name "\"")))))
