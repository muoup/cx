#!/usr/bin/env sh
set -eu

case "$#:${1-}" in
    0:|1:--grammar-only) ;;
    *) printf 'Usage: %s [--grammar-only]\n' "$0" >&2; exit 2 ;;
esac

root=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
target="$root/target"
grammar="$root/compiler/chl-treesitter-grammar"
nvim="$root/compiler/chl-extension-nvim/parser"
zed_source="$root/compiler/chl-extension-zed"
zed="$target/chl-extension-zed"
snapshot="$target/chl-zed-grammar"
build="$target/.setup-extensions"

mkdir -p "$target"
mkdir "$build"
trap 'rm -rf "$build"' EXIT
trap 'exit 1' HUP INT TERM

mkdir "$build/queries"
cp "$grammar/grammar.js" "$grammar/tree-sitter.json" "$build/"
cp "$grammar/queries/highlights.scm" "$build/queries/"
(
    cd "$build"
    tree-sitter generate
    tree-sitter build -o "$build/chl.so"
)

if [ "$#" -eq 0 ]; then
    (
        cd "$build"
        tree-sitter build --wasm -o "$build/chl.wasm"
    )
    cargo build --locked --manifest-path "$root/Cargo.toml" --target-dir "$target" -p chl-lsp

    if [ ! -d "$snapshot/.git" ]; then
        git init -q "$snapshot"
    fi
    mkdir -p "$snapshot/queries"
    cp "$build/grammar.js" "$build/tree-sitter.json" "$snapshot/"
    cp "$build/queries/highlights.scm" "$snapshot/queries/"
    git -C "$snapshot" add -- grammar.js tree-sitter.json queries/highlights.scm
    if ! git -C "$snapshot" diff --cached --quiet; then
        git -C "$snapshot" -c user.name=Chloro -c user.email=chloro@localhost \
            -c core.hooksPath=/dev/null commit --no-gpg-sign -qm "Snapshot local grammar"
    fi

    revision=$(git -C "$snapshot" rev-parse HEAD)
    repository=$(printf 'file://%s' "$snapshot" | sed 's/\\/\\\\/g; s/"/\\"/g')
    cat "$zed_source/extension.toml" > "$build/extension.toml"
    printf '\n[grammars.chl]\nrepository = "%s"\nrev = "%s"\n' "$repository" "$revision" >> "$build/extension.toml"
    cat "$zed_source/Cargo.toml" > "$build/Cargo.toml"
    printf '\n\n[workspace]\n' >> "$build/Cargo.toml"
    cp "$build/queries/highlights.scm" "$build/zed-highlights.scm"

    mkdir -p "$zed/grammars" "$zed/languages/chl"
    cp -R "$zed_source/src" "$zed/"
    cp "$root/Cargo.lock" "$zed/"
    cp "$zed_source/languages/chl/config.toml" "$zed/languages/chl/"
    mv -f "$build/zed-highlights.scm" "$zed/languages/chl/highlights.scm"
    mv -f "$build/Cargo.toml" "$zed/Cargo.toml"
    mv -f "$build/chl.wasm" "$zed/grammars/chl.wasm"
    mv -f "$build/extension.toml" "$zed/extension.toml"
fi

mkdir -p "$nvim"
mv -f "$build/queries/highlights.scm" "$nvim/highlights.scm"
mv -f "$build/chl.so" "$nvim/chl.so"

if [ "$#" -eq 0 ]; then
    printf 'Install or rebuild the Zed dev extension at %s\n' "$zed"
fi
