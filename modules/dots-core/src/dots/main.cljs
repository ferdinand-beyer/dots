(ns dots.main
  (:require [applied-science.js-interop :as j]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.modifier-flags :as modifier-flags]
            [dots.typescript.program :as program]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.symbol :as symbol]
            [dots.typescript.type-checker :as type-checker]))

(def proxy-file-name "dots$proxy.d.ts")

(defn- proxy-file-name? [file-name]
  (= file-name proxy-file-name))

(defn- proxy-source-text [module-name references]
  (str (str/join
        (for [path references]
          (str "/// <reference path=\"" path "\" />\n")))
       "export * from \"" module-name "\";\n"))

;; ? Should we construct the SourceFile directly instead or parsing?
(defn- create-proxy-source-file [module-name references target-or-opts]
  (ts/create-source-file proxy-file-name
                         (proxy-source-text module-name references)
                         target-or-opts
                         true))

(defn- proxy-compiler-host
  "Creates a CompilerHost that resolves our special proxy file."
  ([compiler-opts module-name]
   (proxy-compiler-host compiler-opts module-name nil))
  ([compiler-opts module-name references]
   (let [host (ts/create-compiler-host compiler-opts true)]
     (j/call js/Object :assign #js {} host
             #js {:fileExists (fn [file-name]
                                (or (proxy-file-name? file-name)
                                    (j/call host :fileExists file-name)))
                  :getSourceFile
                  (fn [file-name target-or-opts on-error create?]
                    (if (proxy-file-name? file-name)
                      (create-proxy-source-file module-name references target-or-opts)
                      (j/call host :getSourceFile file-name target-or-opts on-error create?)))}))))

(defn- create-program
  "Creates a program that "
  [module-name {:keys [compiler-opts references]
                :or   {compiler-opts #js {}}}]
  (let [host (proxy-compiler-host compiler-opts module-name references)]
    (ts/create-program #js [proxy-file-name] compiler-opts host)))

(defn- module-symbol [program checker]
  (let [node (-> program
                 (program/get-source-file proxy-file-name)
                 source-file/statements
                 first
                 import-declaration/module-specifier)]
    (type-checker/get-symbol-at-location checker node)))

(defn- doc-string [sym]
  (let [parts (symbol/documentation-comment sym)]
    (when (seq parts)
      (ts/display-parts-to-string parts))))

(defn- module-ns-name [sym]
  (let [n (symbol/name sym)]
    (-> n
        (subs 1 (dec (count n)))
        (str/replace "/" ".")
        (str/replace #"\W+" "$")
        (csk/->kebab-case-string))))

(defn- module-exports [{:keys [checker]} sym]
  (type-checker/get-exports-of-module checker sym))

(defn- symbol-type [{:keys [checker]} sym]
  (when-let [decl (symbol/value-declaration sym)]
    (type-checker/get-type-of-symbol-at-location checker sym decl)))

(declare adapt-symbol)

(defn- adapt-module [ctx sym]
  (let [n   (module-ns-name sym)
        ns  {:name n
             :symbol sym
             :doc (doc-string sym)
             :vars {}
             :var-order []}
        ctx (-> ctx
                (assoc-in [:namespaces n] ns)
                (update :ns-stack conj n))]
    (-> (reduce adapt-symbol ctx (module-exports ctx sym))
        (update :ns-stack pop))))

;; ? The module-name is a 'import name'.  We can get the canonical name
;;   to the resolved module from the symbol.  We also need to decide on
;;   a Clojure alias for it, to generate the namespace form like:
;;   `(:require ["ts-name" :as clj-alias])`
(defn- adapt [module-name opts]
  (let [program    (create-program module-name opts)
        checker    (program/get-type-checker program)
        module-sym (module-symbol program checker)
        ctx        {:module-name module-name
                    :program program
                    :checker checker
                    :module-sym module-sym
                    :namespaces {}
                    :ns-stack []}]
    (adapt-module ctx module-sym)))

(defn modifier-flag? [decl flag]
  (not (zero? (bit-and (ts/get-combined-modifier-flags decl) flag))))

(defn- add-var-to-namespace-map [ns-map var-map]
  (let [var-name (:name var-map)]
    (-> ns-map
        (assoc-in [:vars var-name] var-map)
        (update :var-order conj var-name))))

;; TODO: Need to ensure not to overwrite existing vars.  Maybe provide
;; a conflict resolution strategy?
(defn- add-var [ctx var-map]
  (update-in ctx [:namespaces (peek (:ns-stack ctx))]
             add-var-to-namespace-map var-map))

(defn- adapt-variable [ctx sym]
  (add-var ctx {:name (symbol/name sym)
                :symbol sym
                :doc (doc-string sym)
                :kind :variable
                ;; The "const" keyword seems to be eaten by TypeScript.
                ;; Assume "export let" is not allowed and "export" is
                ;; always "const"?!
                ;; TODO: Check differences in generated AST symbol for
                ;; let and const!
                :const? (modifier-flag?
                         (symbol/value-declaration sym)
                         (bit-or modifier-flags/const
                                 modifier-flags/export))}))

(defn- adapt-function [ctx sym]
  ;; TODO: Create :arglists meta
  (add-var ctx {:name (symbol/name sym)
                :symbol sym
                :doc (doc-string sym)
                :kind :function}))

(defn- adapt-symbol [ctx sym]
  (println "Found:" (symbol/name sym))
  ;; Dispatch on `SymbolFlags.ModuleMember` bit mask.
  (cond
    (symbol/variable? sym) (adapt-variable ctx sym)
    (symbol/function? sym) (adapt-function ctx sym)
    (symbol/class? sym) ctx
    (symbol/interface? sym) ctx
    (symbol/enum? sym) ctx
    (symbol/module? sym) ctx
    (symbol/type-alias? sym) ctx
    (symbol/alias? sym) ctx
    ;; Add :diagnostics to ctx?
    :else (do (println "Warning: Unexpected exported symbol"
                       (symbol/name sym) (symbol/flags sym))
              ctx)))

(defn ^:export main []
  (println "Hello, Dots."))

(when goog/DEBUG
  (enable-console-print!))

(comment
  (println)
  (:namespaces (adapt "vscode" nil))
  )
