use std::env;

mod lex;
mod parse;
mod preprocessor;
mod codegen;
mod pipeline;
mod util;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };

    pipeline::CompilerPipeline::new(file_name.clone(), "a.exe".to_string())
        .preprocess()
        .lex()
        .parse()
        .codegen()
        .link();
}
