# C parity examples

These projects build unmodified upstream C software with cx. Each keeps its sources in an `upstream/` submodule and describes the build in a `cx.toml`; anything else in the directory is glue owned by the example. Check the sources out with `git submodule update --init` before building.

| Project | What it builds |
| --- | --- |
| `doomgeneric` | Doom, with a raylib adapter for the window and input. |
| `lua` | The Lua interpreter. |
| `zlib` | zlib, driven by a program that compresses and restores a buffer. |

They double as the project cases of the benchmark suite, which compares cx against a reference C compiler on the same sources; see `tests/README.md`.

Examples written in CX itself live one level up.
