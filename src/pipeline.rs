use cranelift::codegen;
use crate::lex::token::Token;
use crate::parse::verify::VerifiedAST;
use crate::{lex, log_error, parse, preprocessor};
use crate::codegen::ast_codegen;

#[derive(Default, Debug)]
pub(crate) struct CompilerPipeline {
    source_dir: String,
    output: String,

    file_contents: String,

    preprocessed: String,
    lexer: Vec<Token>,
    ast: Option<VerifiedAST>,
}

impl CompilerPipeline {
    pub(crate) fn new(source: String, output: String) -> Self {
        Self {
            source_dir: source,
            output,
            file_contents: String::new(),
            preprocessed: String::new(),
            lexer: Vec::new(),
            ast: None,
        }
    }

    pub(crate) fn preprocess(mut self) -> Self {
        self.file_contents = std::fs::read_to_string(self.source_dir.as_str()).unwrap();
        self.preprocessed = preprocessor::preprocess(self.file_contents.as_str());
        self
    }

    pub(crate) fn lex(mut self) -> Self {
        self.lexer = lex::generate_tokens(self.preprocessed.as_str());
        self
    }

    pub(crate) fn parse(mut self) -> Self {
        self.ast = parse::parse_ast(&mut self.lexer);
        self
    }

    pub(crate) fn codegen(mut self) -> Self {
        if let Some(ast) = &self.ast {
            let Some(_) = ast_codegen(ast, self.output.as_str()) else {
                println!("Failed to generate code");
                return self
            };
        }

        self
    }
}