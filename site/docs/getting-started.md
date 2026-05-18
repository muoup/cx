---
title: Getting Started
description: Build the CX compiler and compile your first CX source file or project.
---

# Getting Started

CX is currently built from source. The compiler is written in Rust and links
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
cargo build --release --features llvm-backend
```

During development, you can run the compiler directly through Cargo:

```bash
cargo run --release -- <file.cx> [options]
```

For a shorter command, add the built binary to your `PATH` or create a symlink:

```bash
ln -s /path/to/cx/target/release/cx ~/.local/bin/cx
```

## Create a Project

Project mode is the preferred workflow because it enables the CX module system
and reads build settings from `cx.toml`.

```bash
cx init hello_cx
cd hello_cx
cx build
```

This creates a starter project with a `cx.toml` file and a `main.cx`, then writes
build output under `.internal/`.

## Compile a Single File

CX can also compile a single `.cx` file directly:

```bash
cx main.cx -o main
```

Single-file compilation is intended to behave like a C compiler invocation.
Imports are limited to compiler-owned libraries such as the standard library, so
larger multi-file projects should use project mode.

## Useful Options

- `--backend-cranelift`: use the Cranelift backend
- `--backend-llvm`: use the LLVM backend when available
- `-O0`, `-O1`, `-O2`, `-O3`, `-Osize`, `-Ofast`: choose optimization level
- `-o <output_file>`: set the output path
- `--analysis`: run safe-function FMIR verification before code generation
- `--verbose`: print each compilation step

## Current Status

CX is a work in progress. It aims for C99 compatibility, but not every C feature
is implemented yet. To avoid confusion, direct `.c` file compilation is not the
primary supported mode today; use `.cx` sources while the compiler grows.
