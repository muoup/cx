# Getting Started

To try out the language, you will currently need to build the compiler from scratch. See the README.md in the [repository](https://github.com/muoup/cx) for getting the project set up.

Once built, compilation can be done in one of two ways. The preferred way, which will be referred to as 'project compilation' uses a builtin CX compiler configuration scheme to allow for full usage of CX's module system. To begin, initialize the project directory as such:

```bash
cx init [project-name]
```

This will create a starting directory template in your current working path named "project-name" containing a config file and a simple "Hello, world!" implementation which can be compiled using:

```bash
cx build
```

which produces an executable in project-name/.internal/

CX is intended to be fully backward-compatible with C and consequently the compiler also seeks to be a drop-in replacement for C compilers like `clang` and `gcc`. Therefore, one may simply compile a .cx file into an executable directly:

```bash
cx main.cx
```

which will produce a `a.out` file executable. Compiling in this mode, denoted as 'single-file target compilation' is compatible with all CX features, however `import` statements are limited to only compiler-owned libraries such as the standard-library, so multi-file projects using this compilation method will need to use standard C-style header file idioms.

***Note***: The compiler is a work-in-progress, so not all C features are fully supported, and as such to prevent confusion, compiling '.c' files in this single compilation mode will not be supported.