---
title: Getting Started
description: Build the Chloro compiler and compile your first Chloro source file or project.
---

# Getting Started

Chloro is currently built from source. The compiler is written in Rust and links
executables with GCC. LLVM is optional; the default backend is Cranelift.

## Prerequisites

- [Rust](https://www.rust-lang.org/tools/install)
- [GCC](https://gcc.gnu.org/install/) for linking executables
- Optional: LLVM, if you want to build with the LLVM backend

## Build the Compiler

Clone the repository and build the compiler:

```bash
git clone https://github.com/muoup/cx.git
cd cx
cargo build --release
```

To include the LLVM backend:

```bash
cargo build --release --features backend-llvm
```

During development, you can run the compiler directly through Cargo:

```bash
cargo run --release -- <file.chl> [options]
```

For a shorter command, add the built binary to your `PATH` or create a symlink:

```bash
ln -s /path/to/chl/target/release/chl ~/.local/bin/chl
```

## Create a Project

Project mode is the preferred workflow because it enables the Chloro module system
and reads build settings from `chl.lua`.

```bash
chl init hello_chl
cd hello_chl
chl build
```

This creates a starter project with a `chl.lua` file and a `main.chl`, then writes
build output under `.internal/`.

## Compile a Single File

Chloro can also compile a single `.chl` file directly:

```bash
chl main.chl -o main
```

Single-file compilation is intended to behave like a C compiler invocation.
Imports are limited to compiler-owned libraries such as the standard library, so
larger multi-file projects should use project mode.

## Useful Options

- `--backend-cranelift`: use the Cranelift backend
- `--backend-llvm`: use the LLVM backend when available
- `-O0`, `-O1`, `-O2`, `-O3`, `-Osize`, `-Ofast`: choose optimization level
- `-c`: compile without linking
- `-I <directory>`: add a header search directory
- `-D <name>[=value]`: define a preprocessor macro
- `-o <output_file>`: set the output path
- `--verbose`: print each compilation step
- `--version`: print the compiler version

## Current Status

Chloro is a work in progress. It aims for C99 compatibility, but not every C feature
is implemented yet. Direct `.c` compilation is supported in single-file mode;
project mode remains the workflow for Chloro module builds.
