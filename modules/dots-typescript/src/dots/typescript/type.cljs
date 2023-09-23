(ns dots.typescript.type
  (:refer-clojure :exclude [symbol]))

;;export interface Type {
;;    flags: TypeFlags;
;;    symbol: Symbol;
;;    pattern?: DestructuringPattern;
;;    aliasSymbol?: Symbol;
;;    aliasTypeArguments?: readonly Type[];
;;}

;;interface Type {
;;    getFlags(): TypeFlags;
;;    getSymbol(): Symbol | undefined;
;;    getProperties(): Symbol[];
;;    getProperty(propertyName: string): Symbol | undefined;
;;    getApparentProperties(): Symbol[];
;;    getCallSignatures(): readonly Signature[];
;;    getConstructSignatures(): readonly Signature[];
;;    getStringIndexType(): Type | undefined;
;;    getNumberIndexType(): Type | undefined;
;;    getBaseTypes(): BaseType[] | undefined;
;;    getNonNullableType(): Type;
;;    getConstraint(): Type | undefined;
;;    getDefault(): Type | undefined;
;;    isUnion(): this is UnionType;
;;    isIntersection(): this is IntersectionType;
;;    isUnionOrIntersection(): this is UnionOrIntersectionType;
;;    isLiteral(): this is LiteralType;
;;    isStringLiteral(): this is StringLiteralType;
;;    isNumberLiteral(): this is NumberLiteralType;
;;    isTypeParameter(): this is TypeParameter;
;;    isClassOrInterface(): this is InterfaceType;
;;    isClass(): this is InterfaceType;
;;    isIndexType(): this is IndexType;
;;}

(defn flags [^Type type]
  (.getFlags type))

(defn symbol ^Symbol [^Type type]
  (.getSymbol type))

(defn properties ^"Symbol[]" [^Type type]
  (.getProperties type))

(defn call-signatures ^"readonly Signature[]" [^Type type]
  (.getCallSignatures type))

(defn construct-signatures ^"readonly Signature[]" [^Type type]
  (.getConstructSignatures type))

(defn base-types ^"BaseType[] | undefined" [^Type type]
  (.getBaseTypes type))

(defn non-nullable-type ^Type [^Type type]
  (.getNonNullableType type))

(defn constraint ^"Type | undefined" [^Type type]
  (.getConstraint type))

(defn default ^"Type | undefined" [^Type type]
  (.getDefault type))

;; UnionType
(defn union? [^Type type]
  (.isUnion type))

;; IntersectionType
(defn intersection? [^Type type]
  (.isIntersection type))

;; UnionOrIntersectionType
(defn union-or-intersection? [^Type type]
  (.isUnionOrIntersection type))

;; LiteralType
(defn literal? [^Type type]
  (.isLiteral type))

;; StringLiteralType
(defn string-literal? [^Type type]
  (.isStringLiteral type))

;; NumberLiteralType
(defn number-literal? [^Type type]
  (.isNumberLiteral type))

;; TypeParameter
(defn type-parameter? [^Type type]
  (.isTypeParameter type))

;; InterfaceType
(defn class-or-interface? [^Type type]
  (.isClassOrInterface type))

;; InterfaceType
(defn class? [^Type type]
  (.isClass type))

;; IndexType
(defn index-type? [^Type type]
  (.isIndexType type))
