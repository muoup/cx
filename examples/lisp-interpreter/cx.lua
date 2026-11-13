package("lisp_interpreter")
stdlib("std")

debug_profile():backend("llvm")

target("lisp-interpreter", "executable"):match("main.cx")
