#!/usr/bin/env sh
set -eu

RAYLIB_VERSION="${RAYLIB_VERSION:-5.5}"

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
RAYLIB_DIR="$ROOT_DIR/raylib"
SRC_DIR="$RAYLIB_DIR/src"
BUILD_DIR="$RAYLIB_DIR/build"
INCLUDE_DIR="$RAYLIB_DIR/include"
LIB_DIR="$RAYLIB_DIR/lib"
ARCHIVE="$RAYLIB_DIR/raylib-$RAYLIB_VERSION.tar.gz"
URL="https://github.com/raysan5/raylib/archive/refs/tags/$RAYLIB_VERSION.tar.gz"

mkdir -p "$RAYLIB_DIR" "$BUILD_DIR" "$INCLUDE_DIR" "$LIB_DIR"

if [ ! -d "$SRC_DIR/src" ]; then
    TMP_DIR="$RAYLIB_DIR/extracting"
    rm -rf "$TMP_DIR"
    mkdir -p "$TMP_DIR"

    if [ ! -f "$ARCHIVE" ]; then
        echo "Downloading Raylib $RAYLIB_VERSION..."
        if command -v curl >/dev/null 2>&1; then
            curl -L "$URL" -o "$ARCHIVE"
        elif command -v wget >/dev/null 2>&1; then
            wget -O "$ARCHIVE" "$URL"
        else
            echo "error: curl or wget is required to download Raylib" >&2
            exit 1
        fi
    fi

    echo "Extracting Raylib..."
    tar -xzf "$ARCHIVE" -C "$TMP_DIR" --strip-components=1
    rm -rf "$SRC_DIR"
    mv "$TMP_DIR" "$SRC_DIR"
fi

echo "Building Raylib..."
if command -v cmake >/dev/null 2>&1; then
    cmake -S "$SRC_DIR" -B "$BUILD_DIR" \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DBUILD_EXAMPLES=OFF
    cmake --build "$BUILD_DIR" --target raylib

    RAYLIB_A=$(find "$BUILD_DIR" -name 'libraylib.a' -print -quit)
elif command -v make >/dev/null 2>&1; then
    make -C "$SRC_DIR/src" PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=STATIC
    RAYLIB_A="$SRC_DIR/src/libraylib.a"
else
    echo "error: cmake or make is required to build Raylib" >&2
    exit 1
fi

if [ ! -f "$RAYLIB_A" ]; then
    echo "error: failed to find libraylib.a after build" >&2
    exit 1
fi

echo "Installing headers and relocatable object..."
cp "$SRC_DIR/src/raylib.h" "$INCLUDE_DIR/raylib.h"
cp "$SRC_DIR/src/raymath.h" "$INCLUDE_DIR/raymath.h"
cp "$SRC_DIR/src/rlgl.h" "$INCLUDE_DIR/rlgl.h"

ld -r --whole-archive "$RAYLIB_A" --no-whole-archive -o "$LIB_DIR/raylib.o"

echo "Raylib artifacts are ready:"
echo "  $INCLUDE_DIR/raylib.h"
echo "  $LIB_DIR/raylib.o"
