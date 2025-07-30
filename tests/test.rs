use std::process::Command;
use cx_data_pipeline::{CompilerBackend, CompilerConfig};
use cx_pipeline::standard_compilation;

fn get_output() -> String {
    let mut cmd = Command::new("./a.out");
    let output = cmd.output().unwrap();
    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn run_tests() {
    // Change CWD to workspace root to make paths simpler.
    let root = std::env::current_dir().unwrap().parent().unwrap().to_path_buf();
    std::env::set_current_dir(&root).unwrap();
    std::env::set_current_dir("tests/cases").unwrap();
    
    std::fs::remove_dir_all(".internal")
        .unwrap_or({});
    
    let cases_dir = std::fs::read_dir("./").unwrap();
    
    let cranelift_config = CompilerConfig {
        backend: CompilerBackend::Cranelift,
        optimization_level: cx_data_pipeline::OptimizationLevel::O0,
        output: "a.out".into(),
    };
    let llvm_config = CompilerConfig {
        backend: CompilerBackend::LLVM,
        optimization_level: cx_data_pipeline::OptimizationLevel::O0,
        output: "a.out".into(),
    };

    for case in cases_dir {
        let case = case.unwrap();
        let path = case.path(); // path is now relative to workspace root, e.g., "tests/cases/hello_world.cx"

        if path.extension().is_some() && path.extension().unwrap() == "cx" {
            let expected_output_path = path.with_extension("cx-output");
            let expected_output = std::fs::read_to_string(&expected_output_path).unwrap_or_else(|_|
                panic!("Could not read expected output file: {expected_output_path:?}")
            );
            
            println!("[{}] Compiling...", path.display());

            std::env::set_current_dir(&root).unwrap();
            std::env::set_current_dir("tests/cases").unwrap();
            standard_compilation(cranelift_config.clone(), path.as_path())
                .expect("Cranelift compilation failed");
            assert_eq!(expected_output, get_output(), "Cranelift output does not match expected output for {}", path.display());
            println!("[{}] Cranelift output matches expected output.", path.display());
            
            if cfg!(feature = "backend-llvm") {
                std::env::set_current_dir(&root).unwrap();
                std::env::set_current_dir("tests/cases").unwrap();
                standard_compilation(llvm_config.clone(), path.as_path())
                    .expect("LLVM compilation failed");
                assert_eq!(expected_output, get_output(), "LLVM output does not match expected output for {}", path.display());
                println!("[{}] LLVM output matches expected output.", path.display());
            }

            std::fs::remove_file("a.out").unwrap_or_else(|_| {
                panic!("Could not remove output file: a.out");
            });
        }
    }
}
