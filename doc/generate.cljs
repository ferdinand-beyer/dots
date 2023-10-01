(ns doc.generate
  (:require ["js-module" :as module-alias]))

;; module-alias/first
;; (.. module-alias/first -second -third)
(def module-get nil)

;; (set! module-alias/name x)
;; (set! (.. module-alias/first -second -third) x)
(def module-set nil)

;; (module-alias/first ,,,)
;; (.. module-alias/first -second (third ,,,))
(def module-call nil)

;; (.-property-name arg)
(def arg-get nil)

;; (set! (.-property-name arg) val)
(def arg-set nil)

;; (.method-name arg ,,,)
(def arg-call nil)

(def ^{:arglists '([args...])} ^js function-alias
  "doc-string"
  ;; This is a "get-form"
  #_module-alias/function-name
  (.. module-alias/namespace-name -function-name))

(defn function-call
  "doc-string"
  {:arglists '([args...])}
  ^js [req1 req2 & [opt1 opt2]]
  #_(_module-alias/function-name req1 req2 opt1 opt2)
  ;; Takes advantage of passing js/undefined args
  ;; This is a "call-form"
  (.. module-alias/namespace-name (function-name req1 req2 opt1 opt2)))

(defn function-call2
  "doc-string"
  {:arglists '([args...])}
  ;; So that cljs tooling can detect argument count
  ^js [_ _ & [_ _]]
  ;; Calling apply on a get-form
  (.apply ^js (.. module-alias/namespace-name -function-name) (js-arguments)))

(def ^js var-alias
  ;; same as function-alias
  nil)
;; conflicts if we also want to generate defns

(defn var-call
  "doc-string"
  []
  ;; same alias-form
  ;; can be used together with call forms, e.g. vscode event properties
  )

;; Var descriptors

{:op :module-get
 :module-name "vscode"
 :path ["version"]}

{:op :module-set
 :module-name "vscode"
 :path ["version"]}

{:op :module-call
 :module-name "typescript"
 :path ["createSourceFile"]
 :args [[{:arg-name "file-name"}
         {:arg-name "source-text"}
         {:arg-name "language-version-or-options"
          :type [:union
                 [:fqn "ScriptTarget"]
                 [:fqn "CreateSourceFileOptions"]]}]
        [{:arg-name "file-name"}
         {:arg-name "source-text"}
         {:arg-name "language-version-or-options"
          :type [:union
                 [:fqn "ScriptTarget"]
                 [:fqn "CreateSourceFileOptions"]]}
         {:arg-name "set-parent-nodes?"}]
        [{:arg-name "file-name"}
         {:arg-name "source-text"}
         {:arg-name "language-version-or-options"
          :type [:union
                 [:fqn "ScriptTarget"]
                 [:fqn "CreateSourceFileOptions"]]}
         {:arg-name "set-parent-nodes?"}
         {:arg-name "script-kind"}]]}

{:op :arg-get
 :path ["objectFlags"]}

{:op :arg-set
 :path ["objectFlags"]}

{:op :arg-call
 :path ["isUnionOrIntersection"]
 :args [[]]}

;; Var descriptor

{:var-name "cljs-name"
 :doc "doc-string"
 :ops [{:op :arg-get}]}

;; Var args -- variadic / "rest" params

{:var-name "join"
 :ops [{:op :module-call
        :module-name "path"
        :path ["join"]
        :arities [{:arg-name "paths"
                   :variadic? true}]}]}
