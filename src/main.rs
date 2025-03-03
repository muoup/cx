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

    let mut output = file_name.split('.').next().unwrap().to_string();
    output.push_str(".o");

    pipeline::CompilerPipeline::new(file_name.clone(), output)
        .preprocess()
        .lex()
        .parse()
        .codegen()
        .link();
}
