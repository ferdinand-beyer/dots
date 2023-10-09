(ns dots.typescript
  (:require ["typescript" :as ts]))

(defn default-compiler-options []
  (ts/getDefaultCompilerOptions))

(defn create-source-file
  ([file-name source-text ^"ScriptTarget | CreateSourceFileOptions" language-version-or-options]
   (ts/createSourceFile file-name source-text language-version-or-options))
  ([file-name source-text ^"ScriptTarget | CreateSourceFileOptions" language-version-or-options set-parent-nodes?]
   (ts/createSourceFile file-name source-text language-version-or-options set-parent-nodes?))
  ([file-name source-text ^"ScriptTarget | CreateSourceFileOptions" language-version-or-options set-parent-nodes? script-kind]
   (ts/createSourceFile file-name source-text language-version-or-options set-parent-nodes? script-kind)))

;; export function createCompilerHost(options: CompilerOptions, setParentNodes?: boolean): CompilerHost;
(defn create-compiler-host
  ^CompilerHost [^CompilerOptions options set-parent-nodes?]
  (ts/createCompilerHost options set-parent-nodes?))

(defn create-program
  "Create a new 'Program' instance. A Program is an immutable collection of 'SourceFile's and a 'CompilerOptions'
   that represent a compilation unit.

   Creating a program proceeds from a set of root files, expanding the set of inputs by following imports and
   triple-slash-reference-path directives transitively. '@types' and triple-slash-reference-types are also pulled in."
  ([^CreateProgramOptions create-program-options]
   (ts/createProgramm create-program-options))
  ([root-names ^CompilerOptions options ^CompilerHost host]
   (ts/createProgram root-names options host))
  ([root-names ^CompilerOptions options ^CompilerHost host ^Program old-program]
   (ts/createProgram root-names options host old-program))
  ([root-names ^CompilerOptions options ^CompilerHost host ^Program old-program ^"Diagnostic[]" config-file-parsing-diagnostics]
   (ts/createProgram root-names options host old-program config-file-parsing-diagnostics)))

(defn get-combined-modifier-flags [^Declaration node]
  (ts/getCombinedModifierFlags node))

(defn display-parts-to-string
  [^"SymbolDisplayPart[] | undefined" display-parts]
  (ts/displayPartsToString display-parts))

(defn resolve-module-name
  (^ResolvedModuleWithFailedLookupLocations [module-name containing-file ^CompilerOptions compiler-options ^ModuleResolutionHost host]
   (ts/resolveModuleName module-name containing-file compiler-options host))
  (^ResolvedModuleWithFailedLookupLocations [module-name containing-file ^CompilerOptions compiler-options ^ModuleResolutionHost host ^ModuleResolutionCache cache]
   (ts/resolveModuleName module-name containing-file compiler-options host cache))
  (^ResolvedModuleWithFailedLookupLocations [module-name containing-file ^CompilerOptions compiler-options ^ModuleResolutionHost host ^ModuleResolutionCache cache ^ResolvedProjectReference redirected-reference]
   (ts/resolveModuleName module-name containing-file compiler-options host cache redirected-reference))
  (^ResolvedModuleWithFailedLookupLocations [module-name containing-file ^CompilerOptions compiler-options ^ModuleResolutionHost host ^ModuleResolutionCache cache ^ResolvedProjectReference redirected-reference ^"ModuleKind.CommonJS | ModuleKind.ESNext" resolution-mode]
   (ts/resolveModuleName module-name containing-file compiler-options host cache redirected-reference resolution-mode)))

(defn get-pre-emit-diagnostics
  (^"readonly Diagnostic[]" [^Program program]
   (ts/getPreEmitDiagnostics program))
  (^"readonly Diagnostic[]" [^Program program ^SourceFile source-file]
   (ts/getPreEmitDiagnostics program source-file))
  (^"readonly Diagnostic[]" [^Program program ^SourceFile source-file ^CancellationToken cancellation-token]
   (ts/getPreEmitDiagnostics program source-file cancellation-token)))

(defn has-rest-parameter?
  [^SignatureDeclaration s]
  (ts/hasRestParameter s))

(defn rest-parameter?
  [^ParameterDeclaration node]
  (ts/isRestParameter node))
