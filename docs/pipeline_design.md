# Pipeline Design

## Stage 1: Lexing

Source text is tokenized.

- **Input**: source text
- **Output**: token stream

## Stage 2: Pre-parsing

The compiler collects type declarations, function signatures, templates, and imports before parsing function bodies. This resolves declaration-vs-expression ambiguities such as:

```c
a * b;
```

- **Input**: token stream
- **Output**: preparse data and import list

## Stage 3: Import Combining

Preparsed data from imported modules is merged into a combined symbol view for the current compilation unit.

- **Input**: local preparse data + imported preparse data
- **Output**: combined declaration environment

## Stage 4: Parsing

The parser builds the AST using the combined declaration environment from the preparse stages.

- **Input**: tokens + combined declaration environment
- **Output**: AST

## Stage 5: Type Checking and Template Realization

The typechecker resolves identifiers to concrete types, realizes templates, inserts implicit coercions, validates ownership rules, and constructs THIR. Staged expressions remain frozen, typechecked THIR fragments.

- **Input**: AST + declaration environment
- **Output**: THIR

## Stage 6: MIR Generation and Analysis

THIR is lowered once into semantic MIR. MIR owns interned semantic types, target-dependent size/alignment layouts, storage ownership metadata such as `@nodrop`, and source ranges for emitted instructions. Symbolic `sizeof` and `alignof` queries are resolved here.

MIR validation, liveness, and safe-function assertion analysis run after lowering, so the code-generation IR remains the semantic analysis boundary.

- **Input**: THIR
- **Output**: MIR + analysis data

## Stage 7: LMIR Generation

MIR is lowered to LMIR, the compiler’s flat SSA-style backend-facing IR.

- **Input**: MIR
- **Output**: LMIR

## Stage 8: Backend Code Generation

LMIR is translated to backend-specific code. The current backends are Cranelift and LLVM.

- **Input**: LMIR
- **Output**: object code or assembly

## Stage 9: Linking

Object files are linked into either an executable or a relocatable library object. Both Cranelift and LLVM backends emit per-function ELF sections (`.text.<function_name>`) to enable linker-level dead code elimination.

### Binary Linking

Binary targets are linked via `gcc` with `--gc-sections`, which strips any function sections not reachable from `main`.

- **Input**: object files
- **Output**: executable

### Library Linking

Library targets use `ld -r --gc-sections` to produce a single merged relocatable object file. Exported symbols (non-static, non-external functions from the entry file) are marked with `--undefined=<sym>` to prevent the linker from stripping them.

- **Input**: object files + exported symbol list
- **Output**: merged `.o` file

### C Header Generation

After library linking, a C header is generated from the entry file's LMIR unit. The header contains type definitions and function declarations for all exported symbols, wrapped in `extern "C"` guards. See [build_system.md](build_system.md) for the full type mapping and header structure.

## IR Roles

- **AST**: parsed source structure
- **MIR**: typed, semantically resolved frontend IR
- **LMIR**: lowered SSA-style IR for code generation
