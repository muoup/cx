use std::env;
use std::path::Path;
use cx_exec_pipeline::standard_llvm_compile;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };
    
    if !file_name.ends_with(".cx") {
        println!("Error: The file must have a .cx extension");
        return;
    }
    
    let path = Path::new(file_name);
    // set working directory to the directory of the file
    env::set_current_dir(&path.parent().unwrap()).unwrap();
    let file_name = path.file_name().unwrap().to_str().unwrap().to_owned();
    
    std::fs::create_dir_all(".internal")
        .expect("Failed to create internal directory");
    std::fs::write(".internal/compiler-dump.data", "")
        .expect("Failed to clear dump file");

    standard_llvm_compile(file_name.as_str());

    println!("Compilation complete!");
}
