package("raylib")
object("artifacts/raylib/lib/raylib.o")
link("GL", "m", "pthread", "dl", "rt", "X11")

library()
