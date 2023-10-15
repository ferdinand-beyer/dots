(ns dots.typescript.type-checker)

;;interface TypeChecker {
;;    getTypeOfSymbolAtLocation(symbol: Symbol, node: Node): Type;
;;    getTypeOfSymbol(symbol: Symbol): Type;
;;    getDeclaredTypeOfSymbol(symbol: Symbol): Type;
;;    getPropertiesOfType(type: Type): Symbol[];
;;    getPropertyOfType(type: Type, propertyName: string): Symbol | undefined;
;;    getPrivateIdentifierPropertyOfType(leftType: Type, name: string, location: Node): Symbol | undefined;
;;    getIndexInfoOfType(type: Type, kind: IndexKind): IndexInfo | undefined;
;;    getIndexInfosOfType(type: Type): readonly IndexInfo[];
;;    getIndexInfosOfIndexSymbol: (indexSymbol: Symbol) => IndexInfo[];
;;    getSignaturesOfType(type: Type, kind: SignatureKind): readonly Signature[];
;;    getIndexTypeOfType(type: Type, kind: IndexKind): Type | undefined;
;;    getBaseTypes(type: InterfaceType): BaseType[];
;;    getBaseTypeOfLiteralType(type: Type): Type;
;;    getWidenedType(type: Type): Type;
;;    getReturnTypeOfSignature(signature: Signature): Type;
;;    getNullableType(type: Type, flags: TypeFlags): Type;
;;    getNonNullableType(type: Type): Type;
;;    getTypeArguments(type: TypeReference): readonly Type[];
;;    /** Note that the resulting nodes cannot be checked. */
;;    typeToTypeNode(type: Type, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): TypeNode | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    signatureToSignatureDeclaration(signature: Signature, kind: SyntaxKind, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): SignatureDeclaration & {
;;        typeArguments?: NodeArray<TypeNode>;
;;    } | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    indexInfoToIndexSignatureDeclaration(indexInfo: IndexInfo, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): IndexSignatureDeclaration | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    symbolToEntityName(symbol: Symbol, meaning: SymbolFlags, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): EntityName | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    symbolToExpression(symbol: Symbol, meaning: SymbolFlags, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): Expression | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    symbolToTypeParameterDeclarations(symbol: Symbol, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): NodeArray<TypeParameterDeclaration> | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    symbolToParameterDeclaration(symbol: Symbol, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): ParameterDeclaration | undefined;
;;    /** Note that the resulting nodes cannot be checked. */
;;    typeParameterToDeclaration(parameter: TypeParameter, enclosingDeclaration: Node | undefined, flags: NodeBuilderFlags | undefined): TypeParameterDeclaration | undefined;
;;    getSymbolsInScope(location: Node, meaning: SymbolFlags): Symbol[];
;;    getSymbolAtLocation(node: Node): Symbol | undefined;
;;    getSymbolsOfParameterPropertyDeclaration(parameter: ParameterDeclaration, parameterName: string): Symbol[];
;;    /**
;;     * The function returns the value (local variable) symbol of an identifier in the short-hand property assignment.
;;     * This is necessary as an identifier in short-hand property assignment can contains two meaning: property name and property value.
;;     */
;;    getShorthandAssignmentValueSymbol(location: Node | undefined): Symbol | undefined;
;;    getExportSpecifierLocalTargetSymbol(location: ExportSpecifier | Identifier): Symbol | undefined;
;;    /**
;;     * If a symbol is a local symbol with an associated exported symbol, returns the exported symbol.
;;     * Otherwise returns its input.
;;     * For example, at `export type T = number;`:
;;     *     - `getSymbolAtLocation` at the location `T` will return the exported symbol for `T`.
;;     *     - But the result of `getSymbolsInScope` will contain the *local* symbol for `T`, not the exported symbol.
;;     *     - Calling `getExportSymbolOfSymbol` on that local symbol will return the exported symbol.
;;     */
;;    getExportSymbolOfSymbol(symbol: Symbol): Symbol;
;;    getPropertySymbolOfDestructuringAssignment(location: Identifier): Symbol | undefined;
;;    getTypeOfAssignmentPattern(pattern: AssignmentPattern): Type;
;;    getTypeAtLocation(node: Node): Type;
;;    getTypeFromTypeNode(node: TypeNode): Type;
;;    signatureToString(signature: Signature, enclosingDeclaration?: Node, flags?: TypeFormatFlags, kind?: SignatureKind): string;
;;    typeToString(type: Type, enclosingDeclaration?: Node, flags?: TypeFormatFlags): string;
;;    symbolToString(symbol: Symbol, enclosingDeclaration?: Node, meaning?: SymbolFlags, flags?: SymbolFormatFlags): string;
;;    typePredicateToString(predicate: TypePredicate, enclosingDeclaration?: Node, flags?: TypeFormatFlags): string;
;;    getFullyQualifiedName(symbol: Symbol): string;
;;    getAugmentedPropertiesOfType(type: Type): Symbol[];
;;    getRootSymbols(symbol: Symbol): readonly Symbol[];
;;    getSymbolOfExpando(node: Node, allowDeclaration: boolean): Symbol | undefined;
;;    getContextualType(node: Expression): Type | undefined;
;;    /**
;;     * returns unknownSignature in the case of an error.
;;     * returns undefined if the node is not valid.
;;     * @param argumentCount Apparent number of arguments, passed in case of a possibly incomplete call. This should come from an ArgumentListInfo. See `signatureHelp.ts`.
;;     */
;;    getResolvedSignature(node: CallLikeExpression, candidatesOutArray?: Signature[], argumentCount?: number): Signature | undefined;
;;    getSignatureFromDeclaration(declaration: SignatureDeclaration): Signature | undefined;
;;    isImplementationOfOverload(node: SignatureDeclaration): boolean | undefined;
;;    isUndefinedSymbol(symbol: Symbol): boolean;
;;    isArgumentsSymbol(symbol: Symbol): boolean;
;;    isUnknownSymbol(symbol: Symbol): boolean;
;;    getConstantValue(node: EnumMember | PropertyAccessExpression | ElementAccessExpression): string | number | undefined;
;;    isValidPropertyAccess(node: PropertyAccessExpression | QualifiedName | ImportTypeNode, propertyName: string): boolean;
;;    /** Follow all aliases to get the original symbol. */
;;    getAliasedSymbol(symbol: Symbol): Symbol;
;;    /** Follow a *single* alias to get the immediately aliased symbol. */
;;    getImmediateAliasedSymbol(symbol: Symbol): Symbol | undefined;
;;    getExportsOfModule(moduleSymbol: Symbol): Symbol[];
;;    getJsxIntrinsicTagNamesAt(location: Node): Symbol[];
;;    isOptionalParameter(node: ParameterDeclaration): boolean;
;;    getAmbientModules(): Symbol[];
;;    tryGetMemberInModuleExports(memberName: string, moduleSymbol: Symbol): Symbol | undefined;
;;    getApparentType(type: Type): Type;
;;    getBaseConstraintOfType(type: Type): Type | undefined;
;;    getDefaultFromTypeParameter(type: Type): Type | undefined;
;;    /**
;;     * Gets the intrinsic `any` type. There are multiple types that act as `any` used internally in the compiler,
;;     * so the type returned by this function should not be used in equality checks to determine if another type
;;     * is `any`. Instead, use `type.flags & TypeFlags.Any`.
;;     */
;;    getAnyType(): Type;
;;    getStringType(): Type;
;;    getStringLiteralType(value: string): StringLiteralType;
;;    getNumberType(): Type;
;;    getNumberLiteralType(value: number): NumberLiteralType;
;;    getBigIntType(): Type;
;;    getBooleanType(): Type;
;;    getFalseType(): Type;
;;    getTrueType(): Type;
;;    getVoidType(): Type;
;;    /**
;;     * Gets the intrinsic `undefined` type. There are multiple types that act as `undefined` used internally in the compiler
;;     * depending on compiler options, so the type returned by this function should not be used in equality checks to determine
;;     * if another type is `undefined`. Instead, use `type.flags & TypeFlags.Undefined`.
;;     */
;;    getUndefinedType(): Type;
;;    /**
;;     * Gets the intrinsic `null` type. There are multiple types that act as `null` used internally in the compiler,
;;     * so the type returned by this function should not be used in equality checks to determine if another type
;;     * is `null`. Instead, use `type.flags & TypeFlags.Null`.
;;     */
;;    getNullType(): Type;
;;    getESSymbolType(): Type;
;;    /**
;;     * Gets the intrinsic `never` type. There are multiple types that act as `never` used internally in the compiler,
;;     * so the type returned by this function should not be used in equality checks to determine if another type
;;     * is `never`. Instead, use `type.flags & TypeFlags.Never`.
;;     */
;;    getNeverType(): Type;
;;    /**
;;     * True if this type is the `Array` or `ReadonlyArray` type from lib.d.ts.
;;     * This function will _not_ return true if passed a type which
;;     * extends `Array` (for example, the TypeScript AST's `NodeArray` type).
;;     */
;;    isArrayType(type: Type): boolean;
;;    /**
;;     * True if this type is a tuple type. This function will _not_ return true if
;;     * passed a type which extends from a tuple.
;;     */
;;    isTupleType(type: Type): boolean;
;;    /**
;;     * True if this type is assignable to `ReadonlyArray<any>`.
;;     */
;;    isArrayLikeType(type: Type): boolean;
;;    getTypePredicateOfSignature(signature: Signature): TypePredicate | undefined;
;;    /**
;;     * Depending on the operation performed, it may be appropriate to throw away the checker
;;     * if the cancellation token is triggered. Typically, if it is used for error checking
;;     * and the operation is cancelled, then it should be discarded, otherwise it is safe to keep.
;;     */
;;    runWithCancellationToken<T>(token: CancellationToken, cb: (checker: TypeChecker) => T): T;
;;}

(defn type-of-symbol-at-location
  ^Type [^TypeChecker type-checker ^Symbol symbol ^Node node]
  (.getTypeOfSymbolAtLocation type-checker symbol node))

(defn type-of-symbol
  ^Type [^TypeChecker type-checker ^Symbol symbol]
  (.getTypeOfSymbol type-checker symbol))

(defn declared-type-of-symbol
  ^Type [^TypeChecker type-checker ^Symbol sym]
  (.getDeclaredTypeOfSymbol type-checker sym))

(defn properties-of-type
  ^"Symbol[]" [^TypeChecker type-checker ^Symbol sym]
  (.getPropertiesOfType type-checker sym))

(defn signatures-of-type
  ^"readonly Signature[]" [^TypeChecker type-checker ^Type type kind]
  (.getSignaturesOfType type-checker type kind))

(defn base-types ^"BaseType[]" [^TypeChecker type-checker ^InterfaceType type]
  (.getBaseTypes type-checker type))

(defn return-type-of-signature
  ^Type [^TypeChecker type-checker ^Signature signature]
  (.getReturnTypeOfSignature type-checker signature))

(defn symbol-at-location
  ^"Symbol | undefined" [^TypeChecker type-checker ^Node node]
  (.getSymbolAtLocation type-checker node))

(defn type-arguments
  ^"readonly Type[]" [^TypeChecker type-checker ^TypeReference type]
  (.getTypeArguments type-checker type))

(defn export-symbol-of-symbol
  "If a symbol is a local symbol with an associated exported symbol, returns the exported symbol.
   Otherwise returns its input.
   For example, at `export type T = number;`:
       - `getSymbolAtLocation` at the location `T` will return the exported symbol for `T`.
       - But the result of `getSymbolsInScope` will contain the *local* symbol for `T`, not the exported symbol.
       - Calling `getExportSymbolOfSymbol` on that local symbol will return the exported symbol."
  ^Symbol [^TypeChecker type-checker ^Symbol symbol]
  (.getExportSymbolOfSymbol type-checker symbol))

(defn signature-to-string
  ([^TypeChecker type-checker ^Signature signature]
   (.signatureToString type-checker signature))
  ([^TypeChecker type-checker ^Signature signature ^Node enclosing-declaration]
   (.signatureToString type-checker signature enclosing-declaration))
  ([^TypeChecker type-checker ^Signature signature ^Node enclosing-declaration flags]
   (.signatureToString type-checker signature enclosing-declaration flags))
  ([^TypeChecker type-checker ^Signature signature ^Node enclosing-declaration flags kind]
   (.signatureToString type-checker signature enclosing-declaration flags kind)))

(defn type-to-string
  ([^TypeChecker type-checker ^Type type]
   (.typeToString type-checker type))
  ([^TypeChecker type-checker ^Type type ^Node enclosing-declaration]
   (.typeToString type-checker type enclosing-declaration))
  ([^TypeChecker type-checker ^Type type ^Node enclosing-declaration flags]
   (.typeToString type-checker type enclosing-declaration flags)))

(defn symbol-to-string
  ([^TypeChecker type-checker symbol]
   (.symbolToString type-checker symbol))
  ([^TypeChecker type-checker symbol enclosing-declaration]
   (.symbolToString type-checker symbol enclosing-declaration))
  ([^TypeChecker type-checker symbol enclosing-declaration meaning]
   (.symbolToString type-checker symbol enclosing-declaration meaning))
  ([^TypeChecker type-checker symbol enclosing-declaration meaning flags]
   (.symbolToString type-checker symbol enclosing-declaration meaning flags)))

(defn fully-qualified-name [^TypeChecker type-checker ^Symbol sym]
  (.getFullyQualifiedName type-checker sym))

(defn signature-from-declaration
  ^"Signature | undefined" [^TypeChecker type-checker ^SignatureDeclaration declaration]
  (.getSignatureFromDeclaration type-checker declaration))

(defn aliased-symbol
  ^Symbol [^TypeChecker type-checker ^Symbol symbol]
  (.getAliasedSymbol type-checker symbol))

(defn exports-of-module
  ^"Symbol[]" [^TypeChecker type-checker ^Symbol module-symbol]
  (.getExportsOfModule type-checker module-symbol))

(defn ambient-modules
  ^"Symbol[]" [^TypeChecker type-checker]
  (.getAmbientModules type-checker))

(defn optional-parameter?
  [^TypeChecker type-checker ^ParameterDeclaration node]
  (.isOptionalParameter type-checker node))

(defn array-type?
  "True if this type is the `Array` or `ReadonlyArray` type from lib.d.ts.
   This function will _not_ return true if passed a type which
   extends `Array` (for example, the TypeScript AST's `NodeArray` type)."
  [^TypeChecker type-checker ^Type type]
  (.isArrayType type-checker type))
