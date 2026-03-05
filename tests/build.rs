use std::env;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy)]
enum TestKind {
    E2E,
    CompileOnly,
    ParseError,
    TypeError,
    VerifierError,
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("Missing manifest dir"));
    let tests = [
        ("e2e", TestKind::E2E),
        ("compile-only", TestKind::CompileOnly),
        ("parse-errors", TestKind::ParseError),
        ("type-errors", TestKind::TypeError),
        ("verifier-errors", TestKind::VerifierError),
    ];

    let mut output = String::new();

    for (root, kind) in tests {
        let root_path = manifest_dir.join(root);
        println!("cargo:rerun-if-changed={}", root_path.display());
        if root_path.exists() {
            write_module(&mut output, &sanitize_ident(root), &root_path, &root_path, kind, 0);
        }
    }

    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("Missing OUT_DIR"));
    fs::write(out_dir.join("generated_tests.rs"), output).expect("Failed to write generated tests");
}

fn write_module(
    output: &mut String,
    module_name: &str,
    root: &Path,
    current: &Path,
    kind: TestKind,
    depth: usize,
) {
    let indent = "    ".repeat(depth);
    output.push_str(&format!("{indent}mod {module_name} {{\n"));

    let mut entries = fs::read_dir(current)
        .unwrap_or_else(|_| panic!("Failed to read directory: {}", current.display()))
        .map(|entry| entry.expect("Failed to read directory entry"))
        .collect::<Vec<_>>();
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        let file_name = entry.file_name();
        let file_name = file_name.to_string_lossy();

        if file_name.starts_with('_') {
            continue;
        }

        if path.is_dir() {
            write_module(
                output,
                &sanitize_ident(&file_name),
                root,
                &path,
                kind,
                depth + 1,
            );
            continue;
        }

        if path.extension().and_then(|ext| ext.to_str()) != Some("cx") {
            continue;
        }

        if matches!(kind, TestKind::E2E) && !path.with_extension("cx-output").exists() {
            continue;
        }

        let path_literal = format!("{:?}", path.to_string_lossy().to_string());
        let test_name = sanitize_ident(
            path.file_stem()
                .expect("Missing file stem")
                .to_string_lossy()
                .as_ref(),
        );
        let indent = "    ".repeat(depth + 1);

        match kind {
            TestKind::E2E => output.push_str(&format!(
                "{indent}#[test]\n{indent}fn {test_name}() {{ crate::run_e2e_test(std::path::Path::new({path_literal})); }}\n"
            )),
            TestKind::CompileOnly => {
                let analysis = path
                    .components()
                    .any(|component| component.as_os_str() == "analysis");
                output.push_str(&format!(
                    "{indent}#[test]\n{indent}fn {test_name}() {{ crate::run_compile_only_test(std::path::Path::new({path_literal}), {analysis}); }}\n"
                ));
            }
            TestKind::ParseError => output.push_str(&format!(
                "{indent}#[test]\n{indent}fn {test_name}() {{ crate::run_parse_error_test(std::path::Path::new({path_literal})); }}\n"
            )),
            TestKind::TypeError => output.push_str(&format!(
                "{indent}#[test]\n{indent}fn {test_name}() {{ crate::run_type_error_test(std::path::Path::new({path_literal})); }}\n"
            )),
            TestKind::VerifierError => output.push_str(&format!(
                "{indent}#[test]\n{indent}fn {test_name}() {{ crate::run_verifier_error_test(std::path::Path::new({path_literal})); }}\n"
            )),
        }
    }

    output.push_str(&format!("{indent}}}\n"));
}

fn sanitize_ident(name: &str) -> String {
    let mut output = String::with_capacity(name.len());

    for c in name.chars() {
        output.push(if c.is_ascii_alphanumeric() { c } else { '_' });
    }

    if output.is_empty() || output.chars().next().unwrap().is_ascii_digit() {
        output.insert(0, '_');
    }

    output
}
