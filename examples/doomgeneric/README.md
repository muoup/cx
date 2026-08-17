# Doomgeneric C parity example

This example builds the upstream Doomgeneric sources with cx in project mode. The unchanged upstream reference is kept in `upstream/`; `doomgeneric_raylib.c` is the example-owned raylib adapter that supplies the platform entry points and window loop.

Build the shared raylib artifact once from the examples directory, then build Doomgeneric from this directory:

```sh
cd examples
./build-raylib.sh
cd doomgeneric
cx build
```

The target uses `compile_all` and its exclusions to compile the same source set selected by the upstream raylib Makefile into one executable, then links the shared raylib artifact. The example is intentionally a C-parity exercise; unsupported compiler/backend features encountered while building it are tracked in the main project rather than patched into the upstream Doom sources.
