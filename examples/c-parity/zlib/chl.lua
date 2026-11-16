package("zlib")
include("upstream")

debug_profile():backend("llvm")

local roundtrip = target("roundtrip", "executable")
roundtrip:match("upstream/*.c", "roundtrip.c")
roundtrip:exclude("upstream/gz*.c")
