(ns dots.typescript.symbol
  (:refer-clojure :exclude [name]))

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

(defn flags [^Symbol symbol]
  (.getFlags symbol))

(defn name [^Symbol symbol]
  (.getName symbol))

(defn declarations [^Symbol symbol]
  (.getDeclarations symbol))

(defn value-declaration [^Symbol symbol]
  (.-valueDeclaration symbol))

(defn documentation-comment
  (^"SymbolDisplayPart[]" [^Symbol symbol]
   (.getDocumentationComment symbol))
  (^"SymbolDisplayPart[]" [^Symbol symbol ^TypeChecker type-checker]
   (.getDocumentationComment symbol type-checker)))
