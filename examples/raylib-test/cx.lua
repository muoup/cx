local raylib = import "../raylib.lua"

project { backend = "cranelift" }

executable "raylib-test" { "main.cx", deps = { raylib } }
