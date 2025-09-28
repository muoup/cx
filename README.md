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

*   [Rust](https://www.rust-lang.org/tools/install) - for building the compiler.
*   [GCC](https://gcc.gnu.org/install/) - currently the only supported linker for building executables.

#### Optional Prerequisite: LLVM

LLVM is used as a backend for code generation (i.e. generating pseudo-assembly which can then be optimized and assembled into machine code).
LLVM is currently not required by default, instead opting for the Cranelift backend due to its more lightweight nature and better compatibility
with Rust. Generating code with LLVM currently uses Inkwell, a set of bindings for LLVM in Rust, which only supports up to LLVM version 18.1.X,
leading even more to the decision to not include LLVM by default. If you do want better optimized output with LLVM, the steps to install the 
dependency are as so:

##### On Ubuntu/Debian
The [Github releases page for LLVM](https://github.com/llvm/llvm-project/releases) has prebuilt binaries for Linux 
systems on a variety of architectures. Having worked on this project both on Windows and Linux, I can confirm that
this option is much preferable to building LLVM from source. The process that worked for me is as follows:

1. Download the prebuilt binary tarball for your architecture and extract it somewhere, for example `/usr/opt/llvm-18.1.X'
2. Add a symbolic link to the 'llvm-config' binary to somewhere in your PATH, for example:
   ```bash
   sudo ln -s /usr/opt/llvm-18.1.X/bin/llvm-config /usr/bin/llvm-config
   ```
3. Verify the installation by running:
   ```bash
   llvm-config --version
   ```
4. Hopefully, you should now be able to build the project with the LLVM backend enabled.

##### Windows and Otherwise - Building LLVM from Source
See the [official LLVM documentation](https://llvm.org/docs/GettingStarted.html#getting-the-source-code-and-building-llvm) for more information on building LLVM from source.
Good luck :)

### Installation

1.  Clone the repository:
    ```bash
    git clone https://github.com/your-username/cx.git
    ```
2.  Build the project:
    ```bash
    cargo build --release [--features llvm-backend]
    ```
    
## Usage

For information regarding the CX language syntax, see the [Language Specification](docs/language_spec.md) document.

To compile a file in your IDE during development, use the following command:

```bash
cargo run --release -- [-O0, -O1, -O2, -O3, -Ofast, -Osize] [-o <output_file>] <file_name>
```

For convenience, you may find it useful to create a link to the 'cx' executable reachable by your OS's path to allow easier compilation after building the project.

e.g. for Ubuntu/Debian, while in the base of the project directory:
```bash
# Create a symbolic link to the debug build of the project, if desired, the release build will be located in target/release/cx 
ln -s target/debug/cx /usr/bin/cx

# Compile using the symbol link you created, this link only needs to be created once assuming your project's directory does not change 
cx [-O0, -O1, etc.] [-o <output_file>] <file_name>
```

### Options

*   `-O(0-3)`: Set the optimization level (0-3).
*   `-o <output_file>`: Specify the output file name.
*   `<file_name>`: The name of the CX file to compile.

## Compiler Pipeline
For a detailed overview of the compiler pipeline, see the [Pipeline Design](docs/pipeline_design.md) document.

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
