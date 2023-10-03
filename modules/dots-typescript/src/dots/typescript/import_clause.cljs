(ns dots.typescript.import-clause
  (:refer-clojure :exclude [name]))

;;interface ImportClause extends NamedDeclaration {
;;    readonly kind: SyntaxKind.ImportClause;
;;    readonly parent: ImportDeclaration;
;;    readonly isTypeOnly: boolean;
;;    readonly name?: Identifier;
;;    readonly namedBindings?: NamedImportBindings;
;;}

(defn name
  ^Identifier [^ImportClause import-clause]
  (.-name import-clause))

(defn named-bindings
  ^NamedImportBindings [^ImportClause import-clause]
  (.-namedBindings import-clause))
