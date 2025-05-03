use std::env;
use crate::codegen::ast_codegen;
use crate::parse::{pass_bytecode, pass_ast, pass_typecheck, FileInformation};
use crate::parse::parser::{ParserData, TokenIter, VisibilityMode};
use crate::parse::pass_ast::parse_ast;
use crate::util::{dump_all, dump_data};

pub mod lex;
pub mod parse;
pub mod preprocessor;
pub mod util;
pub mod codegen;
mod mangling;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };

    let content = std::fs::read_to_string(file_name)
        .expect(format!("Failed to read file: {:?}", std::fs::canonicalize(file_name).unwrap()).as_str());
    let mut parser_data = ParserData {
        visibility: VisibilityMode::Package,
        toks: TokenIter {
            slice: &lex::generate_tokens(&content),
            index: 0
        },
    };

    std::fs::create_dir_all(".internal")
        .expect("Failed to create internal directory");
    std::fs::write(".internal/compiler-dump.data", "")
        .expect("Failed to clear dump file");

    let (file_name, file_path) = file_name.rsplit_once('/')
        .unwrap_or((file_name, ""));

    let file_information = FileInformation {
        file_name: file_name.to_string(),
        file_path: file_path.to_string(),
    };

    let mut ast = parse_ast(parser_data).unwrap();
    dump_data(&ast);

    pass_typecheck::type_check(&file_information, &mut ast).unwrap();
    dump_data(&ast);

    let program_bytecode = pass_bytecode::gen_bytecode(ast).unwrap();
    dump_data(&program_bytecode);

    ast_codegen(&program_bytecode, "a.o").unwrap();

    // pipeline::CompilerPipeline::new(file_name.clone(), "a.exe".to_string())
    //     .preprocess()
    //     .lex()
    //     .parse()
    //     .codegen()
    //     .link();
}