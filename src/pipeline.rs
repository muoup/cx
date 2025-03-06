use std::fs::File;
use crate::codegen::ast_codegen;
use crate::lex::token::Token;
use crate::parse::ast_interface::emit_interface;
use crate::parse::verify;
use crate::parse::verify::VerifiedAST;
use crate::{lex, parse, preprocessor};
use crate::parse::ast::AST;

#[derive(Default, Debug)]
pub struct CompilerPipeline {
    source_dir: String,
    file_name: String,

    internal_dir: String,

    output: String,
    file_contents: String,

    preprocessed: String,
    lexer: Vec<Token>,
    ast: Option<VerifiedAST>,
}

impl CompilerPipeline {
    pub fn new(source: String, output: String) -> Self {
        let extensionless = source.replace(".cx", "");
        let (path, file) = extensionless.rfind('/')
            .map(|index| (&extensionless[..index], &extensionless[index..]))
            .unwrap_or(("", &extensionless));

        let internal = format!(".internal/{}", path);

        Self {
            source_dir: source,
            internal_dir: internal,
            file_name: file.to_string(),

            output,
            file_contents: String::new(),
            preprocessed: String::new(),
            lexer: Vec::new(),
            ast: None,
        }
    }

    pub fn source_file(&self) -> Option<File> {
        File::open(self.source_dir.as_str()).ok()
    }

    pub fn find_previous_header(&self) -> Option<File> {
        let previous = format!("{}/{}.hx", self.internal_dir, self.file_name);
        File::open(previous).ok()
    }

    pub fn find_previous_object(&self) -> Option<File> {
        let previous = format!("{}/{}.o", self.internal_dir, self.file_name);
        File::open(previous).ok()
    }

    pub fn preprocess(mut self) -> Self {
        self.file_contents = std::fs::read_to_string(self.source_dir.as_str()).unwrap();
        self.preprocessed = preprocessor::preprocess(self.file_contents.as_str());
        self
    }

    pub fn lex(mut self) -> Self {
        self.lexer = lex::generate_tokens(self.preprocessed.as_str());
        self
    }

    pub fn parse(mut self) -> Self {
        let Some(ast) = parse::parse_ast(&mut self.lexer) else {
            println!("Failed to parse AST");
            return self
        };

        std::fs::create_dir_all(self.internal_dir.as_str()).unwrap();
        emit_interface(&ast, format!("{}/{}.hx", self.internal_dir, self.file_name).as_str()).unwrap();

        self.ast = verify::verify_ast(ast);
        self
    }

    pub fn parse_interface(&mut self) -> Option<AST> {
        let ast = parse::parse_ast(&mut self.lexer)?;

        std::fs::create_dir_all(self.internal_dir.as_str()).unwrap();
        emit_interface(&ast, format!("{}/{}.hx", self.internal_dir, self.file_name).as_str()).unwrap();

        Some(ast)
    }

    pub fn codegen(mut self) -> Self {
        let output_path = format!("{}/{}.o", self.internal_dir, self.file_name);

        if let Some(ast) = &self.ast {
            let Some(_) = ast_codegen(ast, output_path.as_str()) else {
                println!("Failed to generate code");
                return self
            };
        }

        self
    }

    pub fn link(mut self) -> Self {
        let output = std::process::Command::new("gcc")
            .arg("-o")
            .arg(self.output.as_str())
            .arg(format!("{}/{}.o", self.internal_dir, self.file_name).as_str())
            .output()
            .expect("Linker failed to start");

        if !output.status.success() {
            println!("Failed to link object file");
            println!("{}", String::from_utf8_lossy(&output.stderr));
        }

        std::fs::remove_file(self.output.as_str()).unwrap();

        self
    }
}