project { backend = "llvm" }

executable "lua" {
  "upstream/*.c",
  "!upstream/onelua.c",
  link = { "m" },
}
