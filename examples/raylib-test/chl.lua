package("raylib_test")
stdlib("std")
include("../artifacts/raylib/include")
dep("../raylib.lua")

local main = target("raylib-test", "executable")
main:match("main.chl")
