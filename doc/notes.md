# Notes

- https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API
- https://ts-ast-viewer.com/

## Usage

- "analyse" typescript modules
- maybe: use this data in Clojure-LSP?
- emit ClojureScript wrapper code
- support some options how to emit:
  - separate namespaces for classes/interfaces/enums?
  - function for accessor + callable type
  - emit docstrings?
  - emit type hints?

steps:

1. create a TypeScript program
2. extract information from TypeScript
3. adapt: decide what code to emit
4. emit ClojureScript code
