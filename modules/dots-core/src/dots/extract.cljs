(ns dots.extract
  (:require [dots.typescript :as ts]
            [dots.typescript.modifier-flags :as modifier-flags]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.type-checker :as type-checker]))

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(defn modifier-flag? [decl flag]
  (not (zero? (bit-and (ts/get-combined-modifier-flags decl) flag))))

(defn- module-exports [{:keys [type-checker]} sym]
  (type-checker/get-exports-of-module type-checker sym))

(declare extract-symbol)

(defn- symbol-table [env symbols]
  (->> symbols
       (map #(extract-symbol env %))
       (map (juxt :name identity))
       (into {})))

(defn- extract-module [env sym]
  {:kind :module
   :name (symbol/name sym)
   :doc (doc-string sym)
   :exports (symbol-table env (module-exports env sym))})

(defn- extract-variable [_env sym]
  {:kind :variable
   :name (symbol/name sym)
   :doc (doc-string sym)
   ;; The "const" keyword seems to be eaten by TypeScript.
   ;; Assume "export let" is not allowed and "export" is
   ;; always "const"?!
   ;; TODO: Check differences in generated AST symbol for
   ;; let and const!
   :const? (modifier-flag?
            (symbol/value-declaration sym)
            (bit-or modifier-flags/const
                    modifier-flags/export))})

(defn- extract-function [_env sym]
  {:kind :function
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-properties [{:keys [type-checker] :as env} sym]
  (let [type  (type-checker/get-declared-type-of-symbol type-checker sym)
        props (type-checker/get-properties-of-type type-checker type)]
    (symbol-table env props)))

(defn- extract-class [env sym]
  {:kind :class
   :name (symbol/name sym)
   :doc (doc-string sym)
   :properties (extract-properties env sym)})

(defn- extract-interface [env sym]
  {:kind :interface
   :name (symbol/name sym)
   :doc (doc-string sym)
   :properties (extract-properties env sym)})

(defn- extract-enum [_env sym]
  {:kind :enum
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-type-alias [_env sym]
  {:kind :type-alias
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-alias [_env sym]
  {:kind :alias
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-property [_env sym]
  {:kind :property
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-method [_env sym]
  {:kind :method
   :name (symbol/name sym)
   :doc (doc-string sym)})

(defn- extract-symbol [env sym]
  (cond
    ;; module members
    (symbol/variable? sym)   (extract-variable env sym)
    (symbol/function? sym)   (extract-function env sym)
    (symbol/class? sym)      (extract-class env sym)
    (symbol/interface? sym)  (extract-interface env sym)
    (symbol/enum? sym)       (extract-enum env sym)
    (symbol/module? sym)     (extract-module env sym)
    (symbol/type-alias? sym) (extract-type-alias env sym)
    (symbol/alias? sym)      (extract-alias env sym)
    ;; properties
    (symbol/property? sym)   (extract-property env sym)
    (symbol/method? sym)     (extract-method env sym)
    ;; Unhandled
    :else (println "Warning: Unexpected exported symbol"
                   (symbol/name sym) (symbol/flags sym))))

(defn extract [env sym]
  (extract-symbol env sym))
