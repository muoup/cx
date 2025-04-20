use std::arch::x86_64::__cpuid;
use std::fs::File;
use std::process::{exit, Command};
use crate::codegen::ast_codegen;
use crate::lex::token::Token;
use crate::parse::ast_interface::emit_interface;
use crate::parse::verify;
use crate::parse::verify::ProgramBytecode;
use crate::{lex, parse, preprocessor};
use crate::parse::ast::AST;

#[derive(Default, Debug)]
pub struct CompilerPipeline {
    source_dir: String,
    file_name: String,
    output_file: String,

    internal_dir: String,

    pipeline_stage: PipelineStage
}

#[derive(Default, Debug)]
pub enum PipelineStage {
    #[default]
    None,
    FileRead(String),
    Preprocessed(String),
    Lexed(Vec<Token>),
    Parsed(AST),
    Bytecode(ProgramBytecode),
    Codegen,
    Linked
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

            output_file: output,

            pipeline_stage: PipelineStage::None
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

    pub fn read_file(mut self) -> Self {
        let file_contents = std::fs::read_to_string(self.source_dir.as_str())
            .expect("PIPELINE ERROR: Failed to read source file");

        self.pipeline_stage = PipelineStage::FileRead(file_contents);
        self
    }

    pub fn preprocess(mut self) -> Self {
        let PipelineStage::FileRead(file_contents) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot preprocess without reading a file!");
            exit(1);
        };

        let preprocessed = preprocessor::preprocess(file_contents);

        self.pipeline_stage = PipelineStage::Preprocessed(preprocessed);
        self
    }

    pub fn lex(mut self) -> Self {
        let PipelineStage::Preprocessed(preprocessed) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot lex without preprocessing!");
            exit(1);
        };

        let lexed = lex::generate_tokens(preprocessed);

        self.pipeline_stage = PipelineStage::Lexed(lexed);
        self
    }

    pub fn parse(mut self) -> Self {
        let PipelineStage::Lexed(lexed) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot parse without lexing!");
            exit(1);
        };

        let Some(ast) = parse::parse_ast(lexed) else {
            println!("ERROR: Failed to parse AST");
            exit(1);
        };

        self.pipeline_stage = PipelineStage::Parsed(ast);
        self
    }

    pub fn verify(mut self) -> Self {
        let lazy_get_ast = || {
            let PipelineStage::Parsed(ast) = self.pipeline_stage else {
                eprintln!("PIPELINE ERROR: Cannot verify without a parsed AST!");
                exit(1);
            };

            ast
        };

        let lazy_verify = || {
            verify::verify_ast(lazy_get_ast()).unwrap_or_else(|| {
                println!("ERROR: Failed to verify AST");
                exit(1);
            })
        };

        self.pipeline_stage = PipelineStage::Bytecode(lazy_verify());
        self
    }

    pub fn generate_interface(mut self) -> Self {
        let PipelineStage::Parsed(ast) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot generate interface without a parsed AST!");
            exit(1);
        };

        std::fs::create_dir_all(self.internal_dir.as_str()).unwrap();
        emit_interface(&ast, format!("{}/{}.hx", self.internal_dir, self.file_name).as_str()).unwrap();

        self
    }

    pub fn take_ast(self) -> Option<AST> {
        let PipelineStage::Parsed(ast) = self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot take AST without a parsed AST!");
            exit(1);
        };

        Some(ast)
    }

    pub fn codegen(mut self) -> Self {
        let PipelineStage::Bytecode(bytecode) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot generate code without a parsed AST!");
            exit(1);
        };

        let output_path = format!("{}/{}.o", self.internal_dir, self.file_name);
        ast_codegen(bytecode, output_path.as_str()).or_else(|| {
            eprintln!("ERROR: Failed to generate code");
            exit(1);
        });

        self
    }

    pub fn link(self) -> Self {
        let PipelineStage::Bytecode(bytecode) = &self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot link without a parsed AST!");
            exit(1);
        };

        let mut imports = bytecode
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
            .args(&["-o", self.output_file.as_str()])
            .args(imports);

        cmd
            .output()
            .expect("Failed to link object files");

        println!("Linked object files");

        self
    }
}