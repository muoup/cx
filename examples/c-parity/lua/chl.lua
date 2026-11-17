package("lua")

debug_profile():backend("llvm")

local lua = target("lua", "executable")
lua:match("upstream/*.c")
lua:exclude("upstream/onelua.c")
link("m")
