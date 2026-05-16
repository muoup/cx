use std::collections::HashMap;

use cx_tokens::token::{Token, TokenKind};

use crate::context::Macro;

pub(crate) fn builtin_macros() -> HashMap<String, Macro> {
    let mut macros = HashMap::new();

    define_language_predefines(&mut macros);
    define_compiler_predefines(&mut macros);
    define_target_predefines(&mut macros);

    macros
}

fn define_language_predefines(macros: &mut HashMap<String, Macro>) {
    define_int(macros, "__STDC__", 1);
    define_int(macros, "__STDC_HOSTED__", 1);
    define_int(macros, "__STDC_VERSION__", 199901);
}

fn define_compiler_predefines(macros: &mut HashMap<String, Macro>) {
    define_int(macros, "__GNUC__", 4);
    define_int(macros, "__GNUC_MINOR__", 8);
    define_int(macros, "__GNUC_PATCHLEVEL__", 0);
    define_int(macros, "__GNUC_STDC_INLINE__", 1);
}

fn define_target_predefines(macros: &mut HashMap<String, Macro>) {
    if cfg!(target_pointer_width = "64") {
        define_int(macros, "__LP64__", 1);
        define_int(macros, "_LP64", 1);
    }

    if cfg!(target_arch = "x86_64") {
        for name in ["__x86_64__", "__x86_64", "__amd64__", "__amd64"] {
            define_int(macros, name, 1);
        }
    } else if cfg!(target_arch = "aarch64") {
        define_int(macros, "__aarch64__", 1);
    }

    if cfg!(target_os = "linux") {
        for name in [
            "__linux__",
            "__linux",
            "linux",
            "__unix__",
            "__unix",
            "unix",
            "__ELF__",
        ] {
            define_int(macros, name, 1);
        }
    } else if cfg!(target_os = "macos") {
        for name in ["__APPLE__", "__MACH__", "__unix__", "__unix", "unix"] {
            define_int(macros, name, 1);
        }
    } else if cfg!(target_os = "windows") {
        for name in ["_WIN32", "__WIN32__"] {
            define_int(macros, name, 1);
        }

        if cfg!(target_pointer_width = "64") {
            for name in ["_WIN64", "__WIN64__"] {
                define_int(macros, name, 1);
            }
        }

        if cfg!(target_env = "gnu") {
            define_int(macros, "__MINGW32__", 1);
            if cfg!(target_pointer_width = "64") {
                define_int(macros, "__MINGW64__", 1);
            }
        }
    }
}

fn define_int(macros: &mut HashMap<String, Macro>, name: &str, value: i64) {
    macros.insert(
        name.to_string(),
        Macro::Object(Box::new([Token::new_unknown(TokenKind::IntLiteral(value))])),
    );
}
