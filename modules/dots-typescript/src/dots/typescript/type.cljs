(ns dots.typescript.type)

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
