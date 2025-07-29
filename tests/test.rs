use std::process::Command;
use cx_data_pipeline::CompilerBackend;
use cx_exec_pipeline::debug_compile;

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

    for case in cases_dir {
        let case = case.unwrap();
        let path = case.path(); // path is now relative to workspace root, e.g., "tests/cases/hello_world.cx"

        if path.extension().is_some() && path.extension().unwrap() == "cx" {
            let expected_output_path = path.with_extension("cx-output");
            let expected_output = std::fs::read_to_string(&expected_output_path).unwrap_or_else(|_|
                panic!("Could not read expected output file: {expected_output_path:?}")
            );
            
            println!("[{}] Compiling...", path.display());
            
            debug_compile(path.to_str().unwrap(), "a.out", CompilerBackend::Cranelift, Default::default());
            assert_eq!(expected_output, get_output(), "Cranelift output does not match expected output for {}", path.display());
            
            if cfg!(feature = "backend-llvm") {
                debug_compile(path.to_str().unwrap(), "a.out", CompilerBackend::LLVM, Default::default());
                assert_eq!(expected_output, get_output(), "LLVM output does not match expected output for {}", path.display());
            }
            
            println!("[{}] Output matches expected output.", path.display());

            std::fs::remove_file("a.out").unwrap_or_else(|_| {
                panic!("Could not remove output file: a.out");
            });
        }
    }
}
