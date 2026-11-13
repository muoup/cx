package("http_server")
stdlib("std")

debug_profile():backend("llvm")

target("http-server", "executable"):match("main.cx")
