---
title: Introduction
description: "Welcome to the Chloro Programming Language"
---

# Overview

Chloro is a general-purpose systems language built on top of the syntax and semantics of the C programming language, introducing a mix of modern conveniences and type-system capability to an otherwise "what you see is what you get" language experience with an unflinching adherence to the principles of **traceability**, **provable safety**, and **control**.

```chl title="hello_world.chl"
import std::io as std;

int main() {
    std::println("Hello, world!");
}
```

The language may seem at times verbose, but it is designed such that all decision and control-flow is visible for a C-level control. Additionally, as the language is designed to eventually be entirely backcompatible with the [C99 standard](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf), including the compiler being a (work in progress) drop-in replacement for `gcc` and `clang`, all of the more complex features of the languages are entirely opt-in; one can write entirely in C and pick-and-choose whatever language features they so please. The languages is however developed around idioms and principles to ensure that one can get the most out of the language experience.

It is recommended also, before getting started with the language, to ensure you are familiar with C. This language reference will mostly skip over language features that are directly taken from it, referenced only to explain features which may expand or provide more reliable idioms to replace those of standard C. 

Recommended Resources:
- [W3Schools' C Programming Tutorial](https://www.w3schools.com/c/) for the basics of the language
- [CPPReference](https://cppreference.com/c/language) for an approachable middle-ground of technical details
- [The GNU C Reference Manual](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html) as the precise and verbose authoritative source