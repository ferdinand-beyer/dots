(ns dots.typescript.import-declaration)

;;export interface ImportDeclaration extends Statement {
;;    readonly kind: SyntaxKind.ImportDeclaration;
;;    readonly parent: SourceFile | ModuleBlock;
;;    readonly modifiers?: NodeArray<Modifier>;
;;    readonly importClause?: ImportClause;
;;    /** If this is not a StringLiteral it will be a grammar error. */
;;    readonly moduleSpecifier: Expression;
;;    readonly assertClause?: AssertClause;
;;}

(defn import-clause
  ^ImportClause [^ImportDeclaration import-declaration]
  (.-importClause import-declaration))

(defn module-specifier
  ^Expression [^ImportDeclaration import-declaration]
  (.-moduleSpecifier import-declaration))
