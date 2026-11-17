---
title: Getting Started
description: Build the Chloro compiler and compile your first Chloro source file or project.
---

# Getting Started

## Installation

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

It is recommended to symlink the produced `chloro` binary to your `$PATH`. Doing such
allows you to invoke the compiler by name without need for a fully qualified path. To do
so, run the following command:

```bash
ln -s /path/to/chl/target/release/chloro ~/.local/bin/chloro

# Afterwards, the compiler may be invoked by name:
chloro run
chloro build
chloro init
chloro main.c
```

## Additional Resources

With the compiler built, see [Build System](./build-system.md) for information on 

## Current Status

Chloro is a work in progress. It aims for C99 compatibility, but not every C feature
is implemented yet. Direct `.c` compilation is supported in single-file mode;
project mode remains the workflow for Chloro module builds.
