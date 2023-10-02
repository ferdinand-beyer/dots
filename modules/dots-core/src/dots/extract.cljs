(ns dots.extract
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.modifier-flags :as modifier-flags]
            [dots.typescript.program :as program]
            [dots.typescript.signature :as signature]
            [dots.typescript.signature-kind :as signature-kind]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.symbol-flags :as symbol-flags]
            [dots.typescript.type :as type]
            [dots.typescript.type-checker :as type-checker]))

(def ^:dynamic *debug?* false)

(def default-compiler-opts #js {})

(def ^:private proxy-file-name "dots$proxy.d.ts")

(defn- proxy-file-name? [file-name]
  (= file-name proxy-file-name))

(defn- proxy-source-text [module-names]
  (->> module-names
       (mapcat (fn [module-name]
                 ;; TODO: Support default exports
                 ;; export * as <name> from?
                 (list "export * from \"" module-name "\";\n")))
       str/join))

;; TODO: Instead of the proxy, can we just resolve modules and find them in the
;; type checker's ambient modules?
(defn- proxy-compiler-host
  "Creates a CompilerHost that resolves our special proxy file."
  [compiler-opts module-names]
  (let [host        (ts/create-compiler-host compiler-opts true)
        source-text (proxy-source-text module-names)]
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
  ([module-names]
   (create-program module-names nil))
  ([module-names compiler-opts]
   (let [compiler-opts (or compiler-opts default-compiler-opts)
         host (proxy-compiler-host compiler-opts module-names)]
     (ts/create-program #js [proxy-file-name] compiler-opts host))))

(defn- module-symbols [program]
  (let [checker     (program/get-type-checker program)
        source-file (program/get-source-file program proxy-file-name)]
    (->> (source-file/statements source-file)
         (map import-declaration/module-specifier)
         (map #(type-checker/symbol-at-location checker %)))))

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(defn- modifier-flag? [decl flag]
  (not (zero? (bit-and (ts/get-combined-modifier-flags decl) flag))))

(defn- symbol-table [env symbols extract-fn]
  (->> symbols
       (map #(extract-fn env %))
       (mapcat (juxt :name identity))
       (apply array-map)))

(defn- mask-flags [sym mask]
  (bit-and mask (symbol/flags sym)))

(defn- debug-type [{:keys [type-checker]} type]
  (let [fqn (some->> (type/symbol type)
                     (type-checker/fully-qualified-name type-checker))]
    (cond-> {:str (type-checker/type-to-string type-checker type)}
      (some? fqn) (assoc :fqn fqn))))

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
      (update-vals (partial debug-type env))))

(defn- extract-symbol-common [env sym kind]
  (let [checker (:type-checker env)
        doc-str (doc-string sym)]
    (cond-> {:kind kind
             :name (symbol/name sym)
             ;; TODO: Only for types, so that we can reference them?
             ;; TODO: Register in the environment? Allow to resolve types
             :fqn  (type-checker/fully-qualified-name checker sym)}
      doc-str      (assoc :doc doc-str)
      *debug?* (assoc :debug/types (debug-types env sym)))))

(defn- extract-parameter [env sym]
  (extract-symbol-common env sym :parameter))

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

(defn- extract-property [env sym]
  (extract-symbol-common env sym :property))

(defn- extract-method [env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> (extract-symbol-common env sym :method)
        (assoc :signatures (extract-signatures env type)))))

(defn- extract-get-accessor [env sym]
  (extract-symbol-common env sym :get-accessor))

(defn- extract-set-accessor [env sym]
  (extract-symbol-common env sym :set-accessor))

(defn- extract-class-member [env sym]
  (condp = (mask-flags sym symbol-flags/class-member)
    symbol-flags/property     (extract-property env sym)
    symbol-flags/method       (extract-method env sym)
    symbol-flags/get-accessor (extract-get-accessor env sym)
    symbol-flags/set-accessor (extract-set-accessor env sym)))

(defn- extract-variable [env sym]
  (-> (extract-symbol-common env sym :variable)
      (assoc
       ;; The "const" keyword seems to be eaten by TypeScript.
       ;; Assume "export let" is not allowed and "export" is
       ;; always "const"?!
       ;; TODO: Check differences in generated AST symbol for
       ;; let and const!
       :const? (modifier-flag?
                (symbol/value-declaration sym)
                (bit-or modifier-flags/const
                        modifier-flags/export)))))

(defn- extract-function [env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)]
    (-> (extract-symbol-common env sym :function)
        (assoc :signatures (extract-signatures env type)))))

(defn- extract-class-members
  [{:keys [type-checker] :as env} sym]
  (let [type  (type-checker/declared-type-of-symbol type-checker sym)
        props (type-checker/properties-of-type type-checker type)]
    (symbol-table env props extract-class-member)))

(defn- extract-class [env sym]
  (-> (extract-symbol-common env sym :class)
      (assoc :members (extract-class-members env sym))))

(defn- extract-interface [env sym]
  (let [checker (:type-checker env)
        type (type-checker/declared-type-of-symbol checker sym)
        sigs (extract-signatures env type)]
    (-> (extract-symbol-common env sym :interface)
        (assoc :members (extract-class-members env sym))
        (cond->
         (seq sigs) (assoc :signatures sigs)))))

(defn- extract-enum-member [env sym]
  (extract-symbol-common env sym :enum-member))

(defn- extract-enum-members [env sym]
  (let [checker (:type-checker env)
        type    (type-checker/type-of-symbol checker sym)
        props   (type-checker/properties-of-type checker type)]
    (symbol-table env props extract-enum-member)))

(defn- extract-enum [env sym]
  (-> (extract-symbol-common env sym :enum)
      (assoc :members (extract-enum-members env sym))))

(defn- extract-type-alias [env sym]
  (extract-symbol-common env sym :type-alias))

(defn- extract-alias [env sym]
  (extract-symbol-common env sym :alias))

(declare extract-module)

;; TODO: Use multimethod?
(defn- extract-module-member [env sym]
  (condp = (mask-flags sym symbol-flags/module-member)
    symbol-flags/function-scoped-variable (extract-variable env sym)
    symbol-flags/block-scoped-variable    (extract-variable env sym)
    symbol-flags/function                 (extract-function env sym)
    symbol-flags/class                    (extract-class env sym)
    symbol-flags/interface                (extract-interface env sym)
    symbol-flags/const-enum               (extract-enum env sym)
    symbol-flags/regular-enum             (extract-enum env sym)
    symbol-flags/value-module             (extract-module env sym)
    symbol-flags/namespace-module         (extract-module env sym)
    symbol-flags/type-alias               (extract-type-alias env sym)
    symbol-flags/alias                    (extract-alias env sym)))

(defn- module-exports [{:keys [type-checker]} sym]
  (type-checker/exports-of-module type-checker sym))

(defn- extract-module [env sym]
  (-> (extract-symbol-common env sym :module)
      (assoc :exports (symbol-table env (module-exports env sym) extract-module-member))))

(defn extract [module-name]
  (let [program (create-program [module-name])
        env     {:type-checker (program/get-type-checker program)}
        symbol  (first (module-symbols program))]
    (extract-module env symbol)))
