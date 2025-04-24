use std::env;
use crate::parse::{pass_molded, pass_typecheck, pass_unverified};
use crate::util::dump_data;

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

    let content = std::fs::read_to_string(file_name)
        .expect("Failed to read file");
    let mut parser_data = parse::parser::ParserData {
        visibility: parse::parser::VisibilityMode::Package,
        toks: parse::parser::TokenIter {
            slice: &lex::generate_tokens(&content),
            index: 0
        },
    };

    std::fs::write(".internal/compiler-dump.data", "")
        .expect("Failed to clear dump file");

    let uv = pass_unverified::generate_unverified(&mut parser_data).unwrap();
    dump_data(&uv);

    let mut molded = pass_molded::mold_ast(&uv).unwrap();
    dump_data(&molded);

    pass_typecheck::type_check(&mut molded).unwrap();
    dump_data(&molded);

    // pipeline::CompilerPipeline::new(file_name.clone(), "a.exe".to_string())
    //     .preprocess()
    //     .lex()
    //     .parse()
    //     .codegen()
    //     .link();
}
