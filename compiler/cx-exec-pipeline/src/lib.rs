use std::path::Path;
use std::sync::LazyLock;
use std::time::SystemTime;

pub mod pipeline;

static START_TIME: LazyLock<SystemTime> = LazyLock::new(SystemTime::now);

pub fn request_compile(file_paths: &[String]) -> Option<Vec<String>> {
    let mut imports = Vec::new();
    
    for file_path in file_paths {
        let internal_path = format!(".internal/{}.cx-types", file_path);
        let internal_path = Path::new(&internal_path);
        
        if !internal_path.exists() ||
            *START_TIME > internal_path.metadata().unwrap().modified().unwrap() {

            let cx_path_str = format!("{}.cx", file_path);
            imports.extend(module_llvm_compile(cx_path_str)?);
        } else {
            // If the internal path exists and the source file has not been modified,
            // we can skip recompilation.
            println!("Skipping recompilation for {}", file_path);
        }
    }
    
    Some(imports)
}

pub fn module_llvm_compile(file_path: String) -> Option<Vec<String>> {
    let pipeline = pipeline::CompilerPipeline::new(
        file_path,
        "a.exe".to_owned()
    );

    let pipeline = pipeline
        .read_file()
        .preprocess()
        .dump()
        .lex()
        .parse()
        .dump()
        .verify()
        .dump()
        .generate_bytecode()
        .dump()
        .llvm_codegen();
    
    Some(pipeline.imports)
}

pub fn standard_llvm_compile(file_path: &str) -> Option<()> {
    let pipeline = pipeline::CompilerPipeline::new(
        file_path.to_owned(),
        "a.exe".to_owned()
    );

    pipeline
        .read_file()
        .preprocess()
        .dump()
        .lex()
        .parse()
        .dump()
        .verify()
        .dump()
        .generate_bytecode()
        .dump()
        .llvm_codegen()
        .link();
    
    Some(())
}