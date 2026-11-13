local raylib = import_c("raylib")
raylib:include("artifacts/raylib/include")
raylib:object("artifacts/raylib/lib/raylib.o")
raylib:link("GL", "m", "pthread", "dl", "rt", "X11")
return raylib
