(ns dots.main
  (:require ["typescript" :as ts]
            [applied-science.js-interop :as j]
            [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

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
  (ts/createSourceFile proxy-file-name
                       (proxy-source-text module-name references)
                       target-or-opts
                       true))

(defn- proxy-compiler-host
  ([compiler-opts module-name]
   (proxy-compiler-host compiler-opts module-name nil))
  ([compiler-opts module-name references]
   (let [host (ts/createCompilerHost compiler-opts true)]
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
  [module-name {:keys [compiler-opts references]
                :or   {compiler-opts #js {}}}]
  (let [host (proxy-compiler-host compiler-opts module-name references)]
    (ts/createProgram #js [proxy-file-name] compiler-opts host)))

(defn- module-symbol [program checker]
  (let [node (-> program
                 (j/call :getSourceFile proxy-file-name)
                 (j/get :statements)
                 first
                 (j/get :moduleSpecifier))]
    (j/call checker :getSymbolAtLocation node)))

(defn- symbol-name [sym]
  (j/call sym :getName))

(defn- symbol-flags [sym]
  (j/call sym :getFlags))

(defn- symbol-decl [sym]
  (j/get sym :valueDeclaration))

(defn- doc-string [sym]
  (let [parts (j/call sym :getDocumentationComment)]
    (when (seq parts)
      (ts/displayPartsToString parts))))

(defn- module-ns-name [sym]
  (let [n (symbol-name sym)]
    (-> (symbol-name sym)
        (subs 1 (dec (count n)))
        (str/replace "/" ".")
        (str/replace #"\W+" "$")
        (csk/->kebab-case-string))))

(defn- module-exports [{:keys [checker]} sym]
  (j/call checker :getExportsOfModule sym))

(defn- symbol-type [{:keys [checker]} sym]
  (when-let [decl (symbol-decl sym)]
    (j/call checker :getTypeOfSymbolAtLocation sym decl)))

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
        checker    (j/call program :getTypeChecker)
        module-sym (module-symbol program checker)
        ctx        {:module-name module-name
                    :program program
                    :checker checker
                    :module-sym module-sym
                    :namespaces {}
                    :ns-stack []}]
    (adapt-module ctx module-sym)))

(defn- flag? [flags test]
  (not= 0 (bit-and flags test)))

(defn- symbol-flag? [sym test]
  (flag? (symbol-flags sym) test))

(defn- variable? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Variable)))

(defn- function? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Function)))

(defn- class? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Class)))

(defn- interface? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Interface)))

(defn- enum? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Enum)))

(defn- module? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Module)))

(defn- type-alias? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :TypeAlias)))

(defn- alias? [sym]
  (symbol-flag? sym (j/get ts/SymbolFlags :Alias)))

(defn modifier-flag? [decl flag]
  (flag? (ts/getCombinedModifierFlags decl) flag))

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
  (add-var ctx {:name (symbol-name sym)
                :symbol sym
                :doc (doc-string sym)
                :kind :variable
                ;; The "const" keyword seems to be eaten by TypeScript.
                ;; Assume "export let" is not allowed and "export" is
                ;; always "const"?!
                ;; TODO: Check differences in generated AST symbol for
                ;; let and const!
                :const? (modifier-flag?
                         (symbol-decl sym)
                         (bit-or ts/ModifierFlags.Const
                                 ts/ModifierFlags.Export))}))

(defn- adapt-function [ctx sym]
  ;; TODO: Create :arglists meta
  (add-var ctx {:name (symbol-name sym)
                :symbol sym
                :doc (doc-string sym)
                :kind :function}))

(defn- adapt-symbol [ctx sym]
  ;; Dispatch on `SymbolFlags.ModuleMember` bit mask.
  (cond
    (variable? sym) (adapt-variable ctx sym)
    (function? sym) (adapt-function ctx sym)
    (class? sym) ctx
    (interface? sym) ctx
    (enum? sym) ctx
    (module? sym) ctx
    (type-alias? sym) ctx
    (alias? sym) ctx
    ;; Add :diagnostics to ctx?
    :else (do (println "Warning: Unexpected exported symbol"
                       (symbol-name sym) (symbol-flags sym))
              ctx)))

(defn ^:export main []
  (println "Hello, Dots."))

(when goog/DEBUG
  (enable-console-print!))

(comment
  (println)

  (:namespaces (adapt "vscode" nil))
  )
