package("app")
stdlib("std")
dep("deps/mathlib")

target("app", "executable"):match("main.chl")
