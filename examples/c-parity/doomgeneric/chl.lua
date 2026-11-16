package("doomgeneric")
include("upstream/doomgeneric")
import("../../raylib.lua")

debug_profile():backend("llvm")

local doom = target("doomgeneric", "executable")
doom:match("upstream/doomgeneric/*.c", "doomgeneric_raylib.c")
doom:exclude(
  "upstream/doomgeneric/doomgeneric_allegro.c",
  "upstream/doomgeneric/doomgeneric_emscripten.c",
  "upstream/doomgeneric/doomgeneric_linuxvt.c",
  "upstream/doomgeneric/doomgeneric_sdl.c",
  "upstream/doomgeneric/doomgeneric_soso.c",
  "upstream/doomgeneric/doomgeneric_sosox.c",
  "upstream/doomgeneric/doomgeneric_win.c",
  "upstream/doomgeneric/doomgeneric_xlib.c",
  "upstream/doomgeneric/gusconf.c",
  "upstream/doomgeneric/icon.c",
  "upstream/doomgeneric/i_allegromusic.c",
  "upstream/doomgeneric/i_allegrosound.c",
  "upstream/doomgeneric/i_sdlmusic.c",
  "upstream/doomgeneric/i_sdlsound.c",
  "upstream/doomgeneric/mus2mid.c"
)
