package("app")
stdlib("std")
dep("deps/plain", "plain")

target("app", "executable"):match("main.chl")
