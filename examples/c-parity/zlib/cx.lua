project { backend = "llvm" }

executable "roundtrip" {
  "upstream/*.c",
  "roundtrip.c",
  "!upstream/gz*.c",
  include = { "upstream" },
}
