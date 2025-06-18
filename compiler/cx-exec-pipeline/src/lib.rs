use std::path::Path;
use std::sync::LazyLock;
use std::time::SystemTime;

pub mod pipeline;

static START_TIME: LazyLock<SystemTime> = LazyLock::new(SystemTime::now);

pub fn request_type_compilation(file_paths: &[String]) -> Option<Vec<String>> {
    let mut imports = Vec::new();
    
    for import_path in file_paths.iter() {
        let internal_path = format!(".internal/{}.cx-types", import_path);
        let internal_path = Path::new(&internal_path);

        if !internal_path.exists() ||
            *START_TIME > internal_path.metadata().unwrap().modified().unwrap() {

            let cx_path_str = if import_path.starts_with("std") {
                let current_exe = std::env::current_exe()
                    .expect("Failed to get current executable path");
                format!("{}/../../lib/{}.cx", current_exe.parent().unwrap().display(), &import_path)
            } else {
                format!("{}.cx", import_path)
            };
            
            imports.extend(
                module_type_compile(format!(".internal/{}", import_path), cx_path_str)?
            );
        } else {
            // If the internal path exists and the source file has not been modified,
            // we can skip recompilation.
            println!("Skipping recompilation for {}", import_path);
        }
    }

    Some(imports)
}

pub fn request_compile(file_paths: &[String]) -> Option<Vec<String>> {
    let mut imports = Vec::new();

    for import_path in file_paths.iter() {
        let internal_path = format!(".internal/{}.cx-functions", import_path);
        let internal_path = Path::new(&internal_path);

        if !internal_path.exists() ||
            *START_TIME > internal_path.metadata().unwrap().modified().unwrap() {

            let cx_path_str = if import_path.starts_with("std") {
                let current_exe = std::env::current_exe()
                    .expect("Failed to get current executable path");
                format!("{}/../../lib/{}.cx", current_exe.parent().unwrap().display(), &import_path)
            } else {
                format!("{}.cx", import_path)
            };

            imports.extend(module_llvm_compile(format!(".internal/{}", import_path), cx_path_str)?);
        } else {
            // If the file exists in the internal directory from after compilation began,
            // we can skip recompilation.
            println!("Skipping recompilation for {}", import_path);
        }
    }

    Some(imports)
}

pub fn module_type_compile(internal_dir: String, file_path: String) -> Option<Vec<String>> {
    let mut pipeline = pipeline::CompilerPipeline::new(
        file_path,
        String::new()
    );
    
    pipeline.internal_dir = internal_dir;

    let pipeline = pipeline
        .read_file()
        .preprocess()
        .dump()
        .lex()
        .parse_types_and_deps()
        .emit_type_defs();
    
    Some(pipeline.imports)
}

pub fn module_llvm_compile(internal_dir: String, file_path: String) -> Option<Vec<String>> {
    let mut pipeline = pipeline::CompilerPipeline::new(
        file_path,
        "a.exe".to_owned()
    );
    
    pipeline.internal_dir = internal_dir;

    let pipeline = pipeline
        .read_file()
        .preprocess()
        .dump()
        .lex()
        .parse_types_and_deps()
        .parse()
        .emit_function_defs()
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
        .parse_types_and_deps()
        .emit_type_defs()
        .parse()
        .emit_function_defs()
        .dump()
        .verify()
        .dump()
        .generate_bytecode()
        .dump()
        .llvm_codegen()
        .link();
    
    Some(())
}