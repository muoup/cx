# CX Compiler
CX is a statically typed C-superset programming language intended to be a modern
toy-like experimentation with both opt-in complexity and ease of use. The compiler
is currently written in Rust and uses a multi-stage module-based compilation pipeline
to transform CX code into machine code.

## Language Philosophy
CX was originally designed under the philosophy of creating a language built off of C with enough
modern features for me to self-host its compiler. This is currently not the case, however this 
starting point has proven to be a good base to build on. The existence of basic templates, member
functions, and a module system allows for a more beginner-friendly experience, while not necessarily
forcing the use of these features. On the flip side, I also intend to implement in the future a more
expressive type system, allowing better intrinsically communicated intent through types, as well as
more complex types such as algebraic data types and dependent types to allow for an opt-in means
to request for compiler guarantees, both for debugging and optimization.

## Getting Started

### Prerequisites

*   [Rust](https://www.rust-lang.org/tools/install)

### Installation

1.  Clone the repository:
    ```bash
    git clone https://github.com/your-username/cx.git
    ```
2.  Build the project:
    ```bash
    cargo build --release
    ```

### Optional Feature: LLVM Backend


## Usage

To compile a CX file, use the following command:

```bash
cargo run --release -- [-O0, -O1, -O2, -O3, -Ofast, -Osize] [-o <output_file>] <file_name>
```

### Options

*   `-O(0-3)`: Set the optimization level (0-3).
*   `-o <output_file>`: Specify the output file name.
*   `<file_name>`: The name of the CX file to compile.

## Compiler Pipeline

The CX compiler uses a multi-stage compilation pipeline to transform CX code into machine code. The pipeline consists of the following stages:

1.  **Lexing:** The source code is converted into a stream of tokens.
2.  **Pre-parsing:** Type declarations and function signatures are parsed.
3.  **Import Combining:** Pre-parsed data from imported modules is combined.
4.  **Parsing:** The abstract syntax tree (AST) is built.
5.  **Type Checking and Template Realization:** The types of expressions and statements are checked, and templates are realized.
6.  **Bytecode Generation:** The AST is converted into a flat SSA IR.
7.  **Backend Code Generation:** The SSA IR is converted into machine code using either Cranelift or LLVM.
8.  **Linking:** The generated object files are linked into a single executable.

## Backends

CX supports the following backends for code generation:

*   **Cranelift:** A fast and lightweight backend that is ideal for development and debugging.
*   **LLVM:** A powerful and optimizing backend that is ideal for production builds.

## Testing

To run the test suite, use the following command:

```bash
cargo test
```

## Contributing

Contributions are welcome! Please feel free to submit a pull request or open an issue.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
