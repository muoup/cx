return dependency {
  include = { "artifacts/raylib/include" },
  objects = { "artifacts/raylib/lib/raylib.o" },
  link = { "GL", "m", "pthread", "dl", "rt", "X11" },
}
