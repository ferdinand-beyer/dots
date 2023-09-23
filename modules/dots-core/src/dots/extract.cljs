(ns dots.extract
  (:refer-clojure :exclude [type])
  (:require [dots.typescript :as ts]
            [dots.typescript.modifier-flags :as modifier-flags]
            [dots.typescript.signature-kind :as signature-kind]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.symbol-flags :as symbol-flags]
            [dots.typescript.type :as type]
            [dots.typescript.signature :as signature]
            [dots.typescript.type-checker :as type-checker]))

(def ^:dynamic *debug?* true)

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(defn- modifier-flag? [decl flag]
  (not (zero? (bit-and (ts/get-combined-modifier-flags decl) flag))))

(defn- symbol-table [env symbols extract-fn]
  (->> symbols
       (map #(extract-fn env %))
       (map (juxt :name identity))
       (into {})))

(defn- mask-flags [sym mask]
  (bit-and mask (symbol/flags sym)))

(defn- type-at-decl [env sym]
  (type-checker/type-of-symbol-at-location
   (:type-checker env) sym (symbol/value-declaration sym)))

(defn- debug-type [{:keys [type-checker]} type]
  (let [fqn (some->> (type/symbol type)
                     (type-checker/fully-qualified-name type-checker))]
    (cond-> {:str (type-checker/type-to-string type-checker type)}
      (some? fqn) (assoc :fqn fqn))))

;; Types of symbol
;;
;; (1) get-type-of-symbol
;; (2) get-declared-type-of-symbol
;; (3) get-type-of-symbol-at-location *value-declaration*
;;
;; For vars, we want (3)
;;
;; For classes:
;; (1) any
;; (2) Class
;; (3) typeof Class
(defn- debug-types [{:keys [type-checker] :as env} sym]
  (-> {:type             (type-checker/type-of-symbol type-checker sym)
       :declared-type    (type-checker/declared-type-of-symbol type-checker sym)
       :type-at-location (type-at-decl env sym)}
      (update-vals (partial debug-type env))))

(defn- extract-symbol-common [env sym kind]
  (cond-> {:kind kind
           :name (symbol/name sym)
           :fqn  (type-checker/fully-qualified-name (:type-checker env) sym)
           :doc  (doc-string sym)}
    *debug?* (assoc :debug/types (debug-types env sym))))

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
  (-> (extract-symbol-common env sym :method)
      (assoc :signatures (extract-signatures env (type-at-decl env sym)))))

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
  (-> (extract-symbol-common env sym :function)
      (assoc :signatures (extract-signatures env (type-at-decl env sym)))))

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

(defn- extract-enum [env sym]
  (extract-symbol-common env sym :enum))

(defn- extract-type-alias [env sym]
  (extract-symbol-common env sym :type-alias))

(defn- extract-alias [env sym]
  (extract-symbol-common env sym :alias))

(declare extract-module)

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

(defn extract-module [env sym]
  (-> (extract-symbol-common env sym :module)
      (assoc :exports (symbol-table env (module-exports env sym) extract-module-member))))
