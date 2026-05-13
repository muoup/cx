1. Add new syntax to treesitter grammar

2. Fix lisp-interpreter compile-time errors + organize implementation into smaller files -- ast.cx (ast types), parser.cx, interpreter.cx, main.cx

3. Add more small-scale project-scale examples, add at least one with an example of C interop

4. Look for easy LSP bugfixes and functionality wins; don't begin any incremental parsing

5. Expand test suite, add more tricky edge cases in tests/ as well as unit testing in the project itself.

6. Create base suite for mkdocs documentation + remove duplicate information from docs/ folder (human only)