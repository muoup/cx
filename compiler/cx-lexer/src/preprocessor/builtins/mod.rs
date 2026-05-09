use std::collections::HashMap;

use cx_tokens::token::{SpecifierType, Token, TokenKind};

use crate::context::Macro;

pub(crate) fn builtin_macros() -> HashMap<String, Macro> {
    let mut macros = HashMap::new();

    define_language_predefines(&mut macros);
    define_compiler_predefines(&mut macros);
    define_target_predefines(&mut macros);
    define_compatibility_predefines(&mut macros);

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
    define_empty(macros, "__USER_LABEL_PREFIX__");
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

fn define_compatibility_predefines(macros: &mut HashMap<String, Macro>) {
    define_empty(macros, "__extension__");
    define_empty_function(macros, "__attribute__", &["x"]);
    define_empty_function(macros, "__attribute", &["x"]);

    for name in ["__const", "__const__"] {
        define_specifier(macros, name, SpecifierType::Const);
    }

    for name in ["__inline", "__inline__"] {
        define_specifier(macros, name, SpecifierType::Inline);
    }

    for name in ["__restrict", "__restrict__"] {
        // FIXME: support restrict specifier
        // define_specifier(macros, name, SpecifierType::Restrict);
        define_empty(macros, name);
    }

    for name in ["__volatile", "__volatile__"] {
        define_specifier(macros, name, SpecifierType::Volatile);
    }
}

fn define_empty(macros: &mut HashMap<String, Macro>, name: &str) {
    macros.insert(name.to_string(), Macro::Object(Box::new([])));
}

fn define_empty_function(macros: &mut HashMap<String, Macro>, name: &str, params: &[&str]) {
    macros.insert(
        name.to_string(),
        Macro::Function {
            params: params
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            body: Box::new([]),
        },
    );
}

fn define_int(macros: &mut HashMap<String, Macro>, name: &str, value: i64) {
    macros.insert(
        name.to_string(),
        Macro::Object(Box::new([Token::new_unknown(TokenKind::IntLiteral(value))])),
    );
}

fn define_specifier(macros: &mut HashMap<String, Macro>, name: &str, specifier: SpecifierType) {
    macros.insert(
        name.to_string(),
        Macro::Object(Box::new([Token::new_unknown(TokenKind::Specifier(
            specifier,
        ))])),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defines_language_and_compiler_basics() {
        let macros = builtin_macros();

        assert_int_macro(&macros, "__STDC__", 1);
        assert_int_macro(&macros, "__STDC_HOSTED__", 1);
        assert_int_macro(&macros, "__STDC_VERSION__", 199901);
        assert_int_macro(&macros, "__GNUC__", 4);
        assert_int_macro(&macros, "__GNUC_MINOR__", 8);
    }

    #[test]
    fn defines_native_target_basics() {
        let macros = builtin_macros();

        if cfg!(target_pointer_width = "64") {
            assert_int_macro(&macros, "__LP64__", 1);
            assert_int_macro(&macros, "_LP64", 1);
        }

        if cfg!(target_arch = "x86_64") {
            assert_int_macro(&macros, "__x86_64__", 1);
        } else if cfg!(target_arch = "aarch64") {
            assert_int_macro(&macros, "__aarch64__", 1);
        }

        if cfg!(target_os = "linux") {
            assert_int_macro(&macros, "__linux__", 1);
        } else if cfg!(target_os = "macos") {
            assert_int_macro(&macros, "__APPLE__", 1);
        } else if cfg!(target_os = "windows") {
            assert_int_macro(&macros, "_WIN32", 1);
        }
    }

    fn assert_int_macro(macros: &HashMap<String, Macro>, name: &str, expected: i64) {
        let Some(Macro::Object(body)) = macros.get(name) else {
            panic!("missing object macro {name}");
        };

        assert!(matches!(
            body.as_ref(),
            [Token {
                kind: TokenKind::IntLiteral(value),
                ..
            }] if *value == expected
        ));
    }
}
