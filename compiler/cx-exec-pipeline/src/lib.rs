use std::path::Path;

pub mod pipeline;

pub fn request_compile(file_paths: &[String]) -> Option<Vec<String>> {
    let mut imports = Vec::new();
    
    for file_path in file_paths {
        let internal_path = format!(".internal/{}.cx-types", file_path);
        let internal_path = Path::new(&internal_path);
        
        let cx_path = format!(".internal/{}.cx", file_path);
        let cx_path = Path::new(&cx_path);
        
        if !internal_path.exists() || 
            cx_path.metadata().map_or(true, |meta| {
               meta.modified().map_or(true, |modified| {
                   modified > internal_path.metadata().map_or(modified, |m| m.modified().unwrap())
               })
            }) {
            
            imports.extend(module_llvm_compile(file_path.clone())?);
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