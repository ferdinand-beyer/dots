(ns dots.typescript.signature)

;;export interface Signature {
;;    declaration?: SignatureDeclaration | JSDocSignature;
;;    typeParameters?: readonly TypeParameter[];
;;    parameters: readonly Symbol[];
;;}
;;
;;interface Signature {
;;    getDeclaration(): SignatureDeclaration;
;;    getTypeParameters(): TypeParameter[] | undefined;
;;    getParameters(): Symbol[];
;;    getTypeParameterAtPosition(pos: number): Type;
;;    getReturnType(): Type;
;;    getDocumentationComment(typeChecker: TypeChecker | undefined): SymbolDisplayPart[];
;;    getJsDocTags(): JSDocTagInfo[];
;;}

(defn declaration
  ^SignatureDeclaration [^Signature signature]
  (.getDeclaration signature))

(defn type-parameters
  ^"TypeParameter[] | undefined" [^Signature signature]
  (.getTypeParameters signature))

(defn parameters
  ^"Symbol[]" [^Signature signature]
  (.getParameters signature))

(defn documentation-comment
  (^"SymbolDisplayPart[]" [^Symbol symbol]
   (.getDocumentationComment symbol))
  (^"SymbolDisplayPart[]" [^Symbol symbol ^TypeChecker type-checker]
   (.getDocumentationComment symbol type-checker)))
