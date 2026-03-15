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

The typechecker resolves identifiers to concrete types, realizes templates, inserts implicit coercions, validates ownership rules, and constructs MIR.

This is the last stage which may hit user errors. After this point, any failures are considered compiler bugs and should be reported.

- **Input**: AST + declaration environment
- **Output**: MIR

## Stage 5.5: Optional Safe-Function Analysis

If the compiler is run with `--analysis`, `safe` functions are lowered from MIR to FMIR and analyzed by `cx-safe-analyzer`.

FMIR is a functional IR used only for analysis. It is not the canonical lowering path for code generation.

Current behavior:

- analysis runs only when `--analysis` is present
- MIR is lowered to FMIR for analysis
- verification diagnostics may be emitted here
- if analysis succeeds, the pipeline returns to MIR and continues to LMIR

- **Input**: MIR for `safe` functions and their contracts
- **Output**: optional verification diagnostics

## Stage 6: LMIR Generation

MIR is lowered to LMIR, the compiler’s flat SSA-style backend-facing IR.

- **Input**: MIR
- **Output**: LMIR

## Stage 7: Backend Code Generation

LMIR is translated to backend-specific code. The current backends are Cranelift and LLVM.

- **Input**: LMIR
- **Output**: object code or assembly

## Stage 8: Linking

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

After library linking, a C header is generated from the entry file's LMIR unit. The header contains type definitions and function declarations for all exported symbols, wrapped in `extern "C"` guards. See [docs/build_system.md](build_system.md) for the full type mapping and header structure.

## IR Roles

- **AST**: parsed source structure
- **MIR**: typed, semantically resolved frontend IR
- **FMIR**: optional analysis IR for `safe` verification
- **LMIR**: lowered SSA-style IR for code generation
