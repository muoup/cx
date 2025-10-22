use cx_pipeline::standard_compilation;
use cx_pipeline_data::{CompilerBackend, CompilerConfig, OptimizationLevel};
use std::path::Path;
use std::process::Command;

macro_rules! test_files {
    ($($name:ident),*) => {
        $(
            #[test]
            fn $name() {
                let path = format!("{}.cx", stringify!($name));

                execute_test(&Path::new(&path));
            }
        )*
    };
}

#[cfg(test)]
mod regression_tests {
    use super::*;

    test_files!(
        hello_world,
        basic_arithmetic,
        basic_compound_expr,
        basic_for,
        basic_strong_ptr,
        basic_template,
        basic_template_type,
        basic_while,
        bool_tests,
        complex_expressions,
        conditional_lifetime,
        deferring,
        enum_type,
        short_circuit_eval,
        struct_and_pointers,
        struct_parameter,
        template_include,
        vector,
        templated_destructor,
        basic_global_variable,
        global_in_template,
        sum_type
    );
}

#[ctor::ctor]
fn init() {
    let root = env!("CARGO_MANIFEST_DIR");
    let full_path = format!("{root}/cases");
    std::env::set_current_dir(&full_path).unwrap();

    std::fs::remove_dir_all(".internal").unwrap_or(());
}

fn get_output(path: &str) -> String {
    let mut cmd = Command::new(format!("./{path}"));
    let output = cmd.output().unwrap();
    String::from_utf8(output.stdout).unwrap()
}

fn execute_test(input: &Path) {
    if !input.exists() {
        panic!("[{}] Test file does not exist", input.display());
    }

    let expected_output = input.with_extension("cx-output");

    if !expected_output.exists() {
        panic!("[{}] No expected output file found", input.display());
    }

    let obj_output = format!("{}.out", input.file_stem().unwrap().to_str().unwrap());

    let cranelift_config = CompilerConfig {
        backend: CompilerBackend::Cranelift,
        optimization_level: OptimizationLevel::O0,
        output: (&obj_output).into(),
    };
    let llvm_config = CompilerConfig {
        backend: CompilerBackend::LLVM,
        optimization_level: OptimizationLevel::O1,
        output: (&obj_output).into(),
    };

    let Some(expected_output) = std::fs::read_to_string(&expected_output).ok() else {
        eprintln!(
            "[{}] No expected output file found, skipping...",
            input.display()
        );
        return;
    };

    println!("[{}] Compiling...", input.display());

    standard_compilation(cranelift_config.clone(), input).expect("Cranelift compilation failed");
    assert_eq!(
        expected_output,
        get_output(&obj_output),
        "Cranelift output does not match expected output for {}",
        input.display()
    );
    println!(
        "[{}] Cranelift output matches expected output.",
        input.display()
    );

    if cfg!(feature = "backend-llvm") {
        standard_compilation(llvm_config.clone(), input).expect("LLVM compilation failed");
        assert_eq!(
            expected_output,
            get_output(&obj_output),
            "LLVM output does not match expected output for {}",
            input.display()
        );
        println!("[{}] LLVM output matches expected output.", input.display());
    }

    std::fs::remove_file(&obj_output).unwrap_or_else(|_| {
        panic!("Could not remove output file: {obj_output}");
    });
}
