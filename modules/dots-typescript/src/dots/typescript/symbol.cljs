(ns dots.typescript.symbol
  (:refer-clojure :exclude [name])
  (:require [dots.typescript.symbol-flags :as symbol-flags]))

;; export interface Symbol {
;;     flags: SymbolFlags;
;;     escapedName: __String;
;;     declarations?: Declaration[];
;;     valueDeclaration?: Declaration;
;;     members?: SymbolTable;
;;     exports?: SymbolTable;
;;     globalExports?: SymbolTable;
;; }
;;
;; interface Symbol {
;;     readonly name: string;
;;     getFlags(): SymbolFlags;
;;     getEscapedName(): __String;
;;     getName(): string;
;;     getDeclarations(): Declaration[] | undefined;
;;     getDocumentationComment(typeChecker: TypeChecker | undefined): SymbolDisplayPart[];
;;     getJsDocTags(checker?: TypeChecker): JSDocTagInfo[];
;; }

(defn flags [^Symbol sym]
  (.getFlags sym))

(defn name [^Symbol sym]
  (.getName sym))

(defn declarations [^Symbol sym]
  (.getDeclarations sym))

(defn value-declaration [^Symbol sym]
  (.-valueDeclaration sym))

(defn documentation-comment
  (^"SymbolDisplayPart[]" [^Symbol sym]
   (.getDocumentationComment sym))
  (^"SymbolDisplayPart[]" [^Symbol sym ^TypeChecker type-checker]
   (.getDocumentationComment sym type-checker)))

;; TODO: Code below is an extension to the API -- move to dots.*
;; TODO: Map flags to keywords / sets of keywords?

(defn flag? [sym test]
  (not (zero? (bit-and (flags sym) test))))

(defn variable? [sym]
  (flag? sym symbol-flags/variable))

(defn function? [sym]
  (flag? sym symbol-flags/function))

(defn class? [sym]
  (flag? sym symbol-flags/class))

(defn interface? [sym]
  (flag? sym symbol-flags/interface))

(defn enum? [sym]
  (flag? sym symbol-flags/enum))

(defn module? [sym]
  (flag? sym symbol-flags/module))

(defn type-alias? [sym]
  (flag? sym symbol-flags/type-alias))

(defn alias? [sym]
  (flag? sym symbol-flags/alias))

(defn property? [sym]
  (flag? sym symbol-flags/property))

(defn method? [sym]
  (flag? sym symbol-flags/method))
