package("app")
stdlib("std")
dep("deps/mathlib", "math")

target("app", "executable"):match("main.chl")
