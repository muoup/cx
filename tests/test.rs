use std::path::Path;
use std::process::Command;
use cx_data_pipeline::{CompilerBackend, CompilerConfig, OptimizationLevel};
use cx_pipeline::standard_compilation;

macro_rules! test_files {
    ($($name:ident),*) => {
        $(
            #[test]
            fn $name() {
                let path = format!("{}.cx", stringify!($name));

                test(&Path::new(&path));
            }
        )*
    };
}

test_files!(
    hello_world,
    basic_arithmetic,
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
    vector
);

#[ctor::ctor]
fn init() {
    let root = env!("CARGO_MANIFEST_DIR");
    let full_path = format!("{root}/cases");
    std::env::set_current_dir(&full_path).unwrap();

    std::fs::remove_dir_all(".internal")
        .unwrap_or({
            // Ignore error if the directory does not exist
        });
}

fn get_output(path: &str) -> String {
    let mut cmd = Command::new(format!("./{path}"));
    let output = cmd.output().unwrap();
    String::from_utf8(output.stdout).unwrap()
}

fn test(input: &Path) {
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
        output: (&obj_output).into()
    };
    let llvm_config = CompilerConfig {
        backend: CompilerBackend::LLVM,
        optimization_level: OptimizationLevel::O1,
        output: (&obj_output).into()
    };

    let Some(expected_output) = std::fs::read_to_string(&expected_output).ok() else {
        eprintln!("[{}] No expected output file found, skipping...", input.display());
        return;
    };

    println!("[{}] Compiling...", input.display());

    standard_compilation(cranelift_config.clone(), input)
        .expect("Cranelift compilation failed");
    assert_eq!(expected_output, get_output(&obj_output), "Cranelift output does not match expected output for {}", input.display());
    println!("[{}] Cranelift output matches expected output.", input.display());

    if cfg!(feature = "backend-llvm") {
        standard_compilation(llvm_config.clone(), input)
            .expect("LLVM compilation failed");
        assert_eq!(expected_output, get_output(&obj_output), "LLVM output does not match expected output for {}", input.display());
        println!("[{}] LLVM output matches expected output.", input.display());
    }
    
    std::fs::remove_file(&obj_output).unwrap_or_else(|_| {
        panic!("Could not remove output file: {}", obj_output);
    });
}


// #[test]
// fn run_tests() {
//     // CWD to workspace root to make paths simpler.
//     let root = std::env::current_dir().unwrap().parent().unwrap().to_path_buf();
//     std::env::set_current_dir(&root).unwrap();
//     std::env::set_current_dir("tests/cases").unwrap();
//
//     std::fs::remove_dir_all(".internal")
//         .unwrap_or(());
//
//     let cases_dir = std::fs::read_dir("./").unwrap();
//     let files = cases_dir
//         .filter_map(Result::ok)
//         .collect::<Vec<_>>();
//
//     for (i, case) in files.iter().enumerate() {
//         let path = case.path(); // path is now relative to workspace root, e.g., "tests/cases/hello_world.cx"
//
//         let cranelift_config = CompilerConfig {
//             backend: CompilerBackend::Cranelift,
//             optimization_level: cx_data_pipeline::OptimizationLevel::O0,
//             output: format!("a{}.out", i).into(),
//         };
//         let llvm_config = CompilerConfig {
//             backend: CompilerBackend::LLVM,
//             optimization_level: cx_data_pipeline::OptimizationLevel::O1,
//             output: format!("a{}.out", i).into(),
//         };
//
//         if path.extension().is_some() && path.extension().unwrap() == "cx" {
//             let expected_output_path = path.with_extension("cx-output");
//             let Some(expected_output) = std::fs::read_to_string(&expected_output_path).ok() else {
//                 eprintln!("[{}] No expected output file found, skipping...", path.display());
//                 return;
//             };
//
//             println!("[{}] Compiling...", path.display());
//
//             std::env::set_current_dir(&root).unwrap();
//             std::env::set_current_dir("tests/cases").unwrap();
//             standard_compilation(cranelift_config.clone(), path.as_path())
//                 .expect("Cranelift compilation failed");
//             assert_eq!(expected_output, get_output(), "Cranelift output does not match expected output for {}", path.display());
//             println!("[{}] Cranelift output matches expected output.", path.display());
//
//             if cfg!(feature = "backend-llvm") {
//                 std::env::set_current_dir(&root).unwrap();
//                 std::env::set_current_dir("tests/cases").unwrap();
//                 standard_compilation(llvm_config.clone(), path.as_path())
//                     .expect("LLVM compilation failed");
//                 assert_eq!(expected_output, get_output(), "LLVM output does not match expected output for {}", path.display());
//                 println!("[{}] LLVM output matches expected output.", path.display());
//             }
//
//             std::fs::remove_file(llvm_config.output).unwrap_or_else(|_| {
//                 panic!("Could not remove output file: a.out");
//             });
//         }
//     }
//
//     std::env::set_current_dir(&root).unwrap();
// }
