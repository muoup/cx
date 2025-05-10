use std::fs::File;
use std::process::{exit, Command};
use cx_backend_cranelift::bytecode_aot_codegen;
use cx_data_ast::lex::token::Token;
use cx_compiler_ast::{lex, parse, preprocessor, LexContents, ParseContents, PreprocessContents};
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_bytecode::generate_bytecode;
use cx_compiler_typechecker::type_check;
use cx_data_ast::parse::ast::CXAST;
use cx_data_bytecode::ProgramBytecode;

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
    Preprocessed(PreprocessContents),
    Lexed(LexContents),
    Parsed(ParseContents),
    Verified(CXAST),
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
        let PipelineStage::FileRead(file_contents) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot preprocess without reading a file!");
            exit(1);
        };

        let preprocessed = preprocessor::preprocess(file_contents.as_str());

        self.pipeline_stage = PipelineStage::Preprocessed(preprocessed);
        self
    }

    pub fn lex(mut self) -> Self {
        let PipelineStage::Preprocessed(preprocessed) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot lex without preprocessing!");
            exit(1);
        };

        let lexed = lex::generate_tokens(preprocessed.as_str());

        self.pipeline_stage = PipelineStage::Lexed(lexed);
        self
    }

    pub fn parse(mut self) -> Self {
        let PipelineStage::Lexed(lexed) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot parse without lexing!");
            exit(1);
        };

        let Some(ast) = parse(lexed) else {
            println!("ERROR: Failed to parse AST");
            exit(1);
        };

        self.pipeline_stage = PipelineStage::Parsed(ast);
        self
    }

    pub fn verify(mut self) -> Self {
        let PipelineStage::Parsed(mut ast) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot verify without a parsed AST!");
            exit(1);
        };

        let Some(()) = type_check(&mut ast) else {
            eprintln!("ERROR: Failed to verify AST");
            exit(1);
        };

        self.pipeline_stage = PipelineStage::Verified(ast);

        self
    }

    pub fn generate_bytecode(mut self) -> Self {
        let PipelineStage::Verified(ast) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot generate bytecode without a verified AST!");
            exit(1);
        };

        let Some(bytecode) = generate_bytecode(ast) else {
            eprintln!("ERROR: Failed to generate bytecode");
            exit(1);
        };

        self.pipeline_stage = PipelineStage::Bytecode(bytecode);
        self
    }

    pub fn take_ast(self) -> Option<CXAST> {
        let PipelineStage::Parsed(ast) = self.pipeline_stage else {
            eprintln!("PIPELINE ERROR: Cannot take AST without a parsed AST!");
            exit(1);
        };

        Some(ast)
    }

    pub fn codegen(mut self) -> Self {
        let PipelineStage::Bytecode(bytecode) = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot generate code without a parsed AST!");
            exit(1);
        };

        let output_path = format!("{}/{}.o", self.internal_dir, self.file_name);
        bytecode_aot_codegen(&bytecode, output_path.as_str()).or_else(|| {
            eprintln!("ERROR: Failed to generate code");
            exit(1);
        });

        self.pipeline_stage = PipelineStage::Codegen;
        self
    }

    pub fn link(mut self) -> Self {
        let PipelineStage::Codegen = std::mem::take(&mut self.pipeline_stage) else {
            eprintln!("PIPELINE ERROR: Cannot link without generating code!");
            exit(1);
        };

        let output_path = format!("{}/{}.o", self.internal_dir, self.file_name);
        let output_file = self.output_file.clone();

        Command::new("gcc")
            .arg(output_path)
            .arg("-o")
            .arg(output_file)
            .status()
            .expect("Failed to execute command");

        println!("Successfully created executable: {}", self.output_file);

        self
    }
}