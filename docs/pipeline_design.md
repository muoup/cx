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

Generated object files are linked into the final executable.

- **Input**: object files
- **Output**: executable

## IR Roles

- **AST**: parsed source structure
- **MIR**: typed, semantically resolved frontend IR
- **FMIR**: optional analysis IR for `safe` verification
- **LMIR**: lowered SSA-style IR for code generation
