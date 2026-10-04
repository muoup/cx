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

The parser builds the AST, represented as HIR, using the combined declaration environment from the preparse stages.

- **Input**: tokens + combined declaration environment
- **Output**: HIR

## Stage 5: HMIR Generation

`cx-hir-lowering` resolves names and constructs HMIR from HIR and the declaration environment. HMIR retains type expressions, comptime parameters and bodies, and staged quotes for evaluation during lowering.

- **Input**: HIR + declaration environment
- **Output**: HMIR

## Stage 6: MIR Generation and Analysis

`cx-hmir-lowering` evaluates comptime expressions, specializes functions, resolves types, inserts conversions, and splices staged quotes while lowering HMIR into MIR. Comptime parameters are bound in the evaluator and omitted from runtime signatures. Type and quote values cannot escape into runtime MIR values.

MIR contains concrete runtime instructions, types, and constants. It owns interned semantic types, target-dependent size/alignment layouts, storage ownership metadata such as `@nodrop`, and source ranges for emitted instructions. Symbolic `sizeof` and `alignof` queries are resolved during HMIR lowering. MIR has no comptime register bank, comptime instruction stream, or staged-expression values.

MIR validation, liveness, and safe-function assertion analysis run after lowering, so the code-generation IR remains the semantic analysis boundary.

- **Input**: HMIR
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

- **HIR (AST)**: parsed declarations and bodies used for symbol lookup and HMIR generation
- **HMIR**: resolved names with type expressions and staging constructs
- **MIR**: typed, semantically resolved frontend IR
- **LMIR**: lowered SSA-style IR for code generation

The legacy THIR, typechecker, THIR lowering, and MIR comptime evaluator sources are retained for parity comparisons but are not part of the active workspace. THIR-specific MIR definitions are archived in [the legacy MIR reference](../compiler/cx-mir-comptime/legacy-mir/README.md).
