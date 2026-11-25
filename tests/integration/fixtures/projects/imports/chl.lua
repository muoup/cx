package("app")
stdlib("std")
include("deps/clib", "clib")
dep("deps/mathlib", "math")
dep("deps/shared", "base")
dep("deps/clib")

target("app", "executable"):match("main.chl")
