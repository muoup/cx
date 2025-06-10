use std::env;

mod pipeline;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };

    std::fs::create_dir_all(".internal")
        .expect("Failed to create internal directory");
    std::fs::write(".internal/compiler-dump.data", "")
        .expect("Failed to clear dump file");

    let pipeline = pipeline::CompilerPipeline::new(
        file_name.clone(),
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

    println!("Compilation complete!");
}
