use std::fs::File;
use std::process::Command;
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
            .map(|index| (&extensionless[..index], &extensionless[index + 1..]))
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

    pub fn header_path(&self) -> String {
        format!("{}/{}.hx", self.internal_dir, self.file_name)
    }

    pub fn find_previous_header(&self) -> Option<File> {
        File::open(self.header_path()).ok()
    }

    pub fn object_path(&self) -> String {
        format!("{}/{}.o", self.internal_dir, self.file_name)
    }

    pub fn find_previous_object(&self) -> Option<File> {
        File::open(self.object_path()).ok()
    }

    pub fn preprocess(mut self) -> Self {
        self.file_contents = std::fs::read_to_string(self.source_dir.as_str()).unwrap();
        self.preprocessed = preprocessor::preprocess(self.file_contents.as_str());
        self
    }

    pub fn take_ast(self) -> Option<VerifiedAST> {
        self.ast
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

        if self.ast.is_none() {
            println!("Failed to verify AST");
        }

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

    pub fn link(self) -> Self {
        let mut imports = self.ast
            .as_ref()
            .unwrap()
            .imports
            .iter()
            .map(|import|
                if import.starts_with("std") {
                    format!(".internal/lib/{}.o", import)
                } else {
                    format!(".internal/{}.o", import)
                }
            )
            .collect::<Vec<_>>();

        imports.push(format!("{}/{}.o", self.internal_dir, self.file_name));

        let mut cmd = Command::new("gcc");
        cmd
            .args(&["-o", self.output.as_str()])
            .args(imports);

        cmd
            .output()
            .expect("Failed to link object files");

        println!("Linked object files");

        self
    }
}