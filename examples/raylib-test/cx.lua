package("raylib_test")
stdlib("std")
import("../raylib.lua")

debug_profile():backend("cranelift")

target("raylib-test", "executable"):match("main.cx")
