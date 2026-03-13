// mod backends;
mod linker;
mod scheduler;
mod template_realizing;
mod backends;

use crate::linker::link;
use crate::scheduler::scheduling_loop;
use crate::scheduler::scheduling_loop_collect_errors;
use cx_util::format::with_dump_directory;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::jobs::{CompilationJob, CompilationStep};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Mutex;

// Re-export LSP diagnostic types for use by cx-lsp
pub use crate::scheduler::{LSPErrorSpan, LSPErrors};

pub fn standard_compilation(config: CompilerConfig, base_file: &Path) -> Option<()> {
    let compiler_context = GlobalCompilationContext {
        config,
        module_db: ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        CompilationUnit::from_rooted(base_file.to_str()?, &compiler_context.config.working_directory),
    );

    with_dump_directory(compiler_context.config.internal_directory.clone(), || {
        scheduling_loop(&compiler_context, initial_job)?;
        link(&compiler_context)
    })?;

    Some(())
}

/// Typecheck-only compilation for LSP integration.
///
/// This function runs the typechecker on the project starting from the given file
/// and returns all errors found. Unlike standard_compilation, this does not
/// generate code or link - it only performs lexing, parsing, and typechecking.
///
/// This is designed for LSP use cases where we want to report errors to the user
/// without the overhead of full compilation.
///
/// # Arguments
/// * `context` - The global compilation context (module database, config)
/// * `initial_file` - The file to start typechecking from (typically the saved file)
///
/// # Returns
/// A vector of LSPErrors (both type errors and fatal errors) found during compilation
pub fn typecheck_only_lsp(
    context: &GlobalCompilationContext,
    initial_file: &CompilationUnit,
) -> Vec<LSPErrors> {
    let mut errors = Vec::new();

    let initial_job = CompilationJob::new(
        vec![],
        CompilationStep::PreParse,
        initial_file.clone(),
    );

    scheduling_loop_collect_errors(context, initial_job, &mut errors);
    errors
}

#[cfg(test)]
mod tests {
    use super::{typecheck_only_lsp, LSPErrorSpan, LSPErrors};
    use cx_pipeline_data::db::ModuleData;
    use cx_pipeline_data::{CompilationUnit, CompilerBackend, CompilerConfig, GlobalCompilationContext, OptimizationLevel};
    use std::collections::HashSet;
    use std::fs;
    use std::path::PathBuf;
    use std::sync::Mutex;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_test_root(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock went backwards")
            .as_nanos();
        std::env::temp_dir().join(format!("cx-lsp-tests-{name}-{nanos}"))
    }

    fn run_lsp_typecheck(name: &str, source: &str) -> Vec<LSPErrors> {
        let root = unique_test_root(name);
        fs::create_dir_all(&root).expect("failed to create temp test root");

        let file_path = root.join("main.cx");
        fs::write(&file_path, source).expect("failed to write test source");

        let context = GlobalCompilationContext {
            config: CompilerConfig {
                backend: CompilerBackend::Cranelift,
                optimization_level: OptimizationLevel::O0,
                output: root.join("a.out"),
                analysis: false,
                working_directory: root.clone(),
                internal_directory: root.join(".internal").join("zed-lsp"),
            },
            module_db: ModuleData::new(),
            linking_files: Mutex::new(HashSet::new()),
        };

        let unit = CompilationUnit::from_rooted("main.cx", &root);
        let errors = typecheck_only_lsp(&context, &unit);
        fs::remove_dir_all(&root).ok();
        errors
    }

    #[test]
    fn lsp_reports_parse_error_with_byte_span() {
        let errors = run_lsp_typecheck(
            "parse-error",
            r#"struct Resource : @nodrop {
    int data;
}

Resource make_resource() {
    return (Resource) {
        .data = 0
    }
}
"#,
        );

        assert!(errors.iter().any(|error| matches!(
            error,
            LSPErrors::SpannedError {
                message,
                span: LSPErrorSpan::ByteRange { start, end },
                ..
            } if message.contains("PARSER ERROR") && *start == 45 && *end >= 68
        )));
    }

//     #[test]
//     fn lsp_reports_type_error_for_type_mismatch() {
//         let errors = run_lsp_typecheck(
//             "type-error",
//             r#"struct Resource : @nodrop {
//     int data;
// };

// Resource make_resource() {
//     return (Resource) {
//         .data = 0
//     };
// }

// void Resource::drop(*this) safe {
//     @unsafe {
//         @leak(this);
//     };
// }

// int main() {
//     int value = make_resource();
//     value;
//     return 0;
// }
// "#,
//         );

//         assert!(errors.iter().any(|error| matches!(
//             error,
//             LSPErrors::SpannedError {
//                 message,
//                 span: LSPErrorSpan::TokenRange { .. },
//                 ..
//             } if message.contains("No implicit cast")
//         )));
//     }
}
