(ns dots.typescript.compiler-host)

;;export interface ModuleResolutionHost {
;;    fileExists(fileName: string): boolean;
(defn file-exists? [^ModuleResolutionHost host file-name]
  (.fileExists host file-name))
;;    readFile(fileName: string): string | undefined;
;;    trace?(s: string): void;
;;    directoryExists?(directoryName: string): boolean;
;;    /**
;;     * Resolve a symbolic link.
;;     * @see https://nodejs.org/api/fs.html#fs_fs_realpathsync_path_options
;;     */
;;    realpath?(path: string): string;
;;    getCurrentDirectory?(): string;
;;    getDirectories?(path: string): string[];
;;    useCaseSensitiveFileNames?: boolean | (() => boolean) | undefined;
;;}
;;export interface CompilerHost extends ModuleResolutionHost {
;;    getSourceFile(fileName: string, languageVersionOrOptions: ScriptTarget | CreateSourceFileOptions, onError?: (message: string) => void, shouldCreateNewSourceFile?: boolean): SourceFile | undefined;
;;    getSourceFileByPath?(fileName: string, path: Path, languageVersionOrOptions: ScriptTarget | CreateSourceFileOptions, onError?: (message: string) => void, shouldCreateNewSourceFile?: boolean): SourceFile | undefined;
;;    getCancellationToken?(): CancellationToken;
;;    getDefaultLibFileName(options: CompilerOptions): string;
;;    getDefaultLibLocation?(): string;
;;    writeFile: WriteFileCallback;
;;    getCurrentDirectory(): string;
;;    getCanonicalFileName(fileName: string): string;
;;    useCaseSensitiveFileNames(): boolean;
;;    getNewLine(): string;
;;    readDirectory?(rootDir: string, extensions: readonly string[], excludes: readonly string[] | undefined, includes: readonly string[], depth?: number): string[];
;;    resolveModuleNames?(moduleNames: string[], containingFile: string, reusedNames: string[] | undefined, redirectedReference: ResolvedProjectReference | undefined, options: CompilerOptions, containingSourceFile?: SourceFile): (ResolvedModule | undefined)[];
;;    /**
;;     * Returns the module resolution cache used by a provided `resolveModuleNames` implementation so that any non-name module resolution operations (eg, package.json lookup) can reuse it
;;     */
;;    getModuleResolutionCache?(): ModuleResolutionCache | undefined;
;;    /**
;;     * This method is a companion for 'resolveModuleNames' and is used to resolve 'types' references to actual type declaration files
;;     */
;;    resolveTypeReferenceDirectives?(typeReferenceDirectiveNames: string[] | readonly FileReference[], containingFile: string, redirectedReference: ResolvedProjectReference | undefined, options: CompilerOptions, containingFileMode?: SourceFile["impliedNodeFormat"] | undefined): (ResolvedTypeReferenceDirective | undefined)[];
;;    getEnvironmentVariable?(name: string): string | undefined;
;;    createHash?(data: string): string;
;;    getParsedCommandLine?(fileName: string): ParsedCommandLine | undefined;
;;}
