use std::process::Command;
use std::fs;
use cx_exec_pipeline::standard_compile;

#[test]
fn run_tests() {
    // Change CWD to workspace root to make paths simpler.
    let root = std::env::current_dir().unwrap().parent().unwrap().to_path_buf();
    std::env::set_current_dir(&root).unwrap();
    std::env::set_current_dir("tests/cases").unwrap();
    
    let cases_dir = fs::read_dir("./").unwrap();

    for case in cases_dir {
        let case = case.unwrap();
        let path = case.path(); // path is now relative to workspace root, e.g., "tests/cases/hello_world.cx"

        if path.extension().is_some() && path.extension().unwrap() == "cx" {
            standard_compile(path.to_str().unwrap(), "a.out", Default::default(), Default::default());

            let mut cmd = Command::new("./a.out");
            let output = cmd.output().unwrap();
            let stdout = String::from_utf8(output.stdout).unwrap();

            let expected_output_path = path.with_extension("cx-output");
            let expected_output = fs::read_to_string(&expected_output_path).unwrap_or_else(|_|
                panic!("Could not read expected output file: {:?}", expected_output_path)
            );

            assert_eq!(stdout, expected_output);
            println!("[{}] Output matches expected output.", path.display());

            fs::remove_file("a.out").unwrap_or_else(|_| {
                panic!("Could not remove output file: a.out");
            });
        }
    }
}
