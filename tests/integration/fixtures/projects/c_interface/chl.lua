package("exports")
stdlib("std")

local lib = library()
lib:match("api.chl")

local api = target("api", "ffi-lib")
api:headers("api.chl")

local consumer = target("consumer", "executable")
consumer:match("consumer.chl")

local native = target("native", "executable")
native:match("main.c")
native:ffi_lib_c_include(".", "api")
