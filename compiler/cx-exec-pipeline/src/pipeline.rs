use crate::{request_compile, request_type_compilation};
use cx_compiler_ast::parse::parse_ast;
use cx_compiler_ast::{lex, preprocessor, LexContents, ParseContents, PreprocessContents};
use cx_compiler_bytecode::generate_bytecode;
use cx_compiler_typechecker::type_check;
use cx_data_ast::parse::ast::{CXTypeMap, CXAST};
use cx_data_ast::parse::parser::ParserData;
use cx_data_bytecode::node_type_map::ExprTypeMap;
use cx_data_bytecode::ProgramBytecode;
use cx_util::format::{dump_data, dump_write};
use std::collections::HashSet;
use std::fs::File;
use std::path::Path;
use std::process::{exit, Command};

#[derive(Default, Debug)]
pub struct CompilerPipeline {
    source_dir: String,
    file_name: String,
    output_file: String,
    pub internal_dir: String,
    
    pub imports: Vec<String>,
    
    pipeline_stage: PipelineStage
}

#[derive(Default, Debug)]
pub enum PipelineStage {
    #[default]
    None,
    FileRead(String),
    Preprocessed(PreprocessContents),
    Lexed(LexContents),
    TypesAndDependences(LexContents, CXTypeMap, Vec<String>),
    Parsed(ParseContents),
    Typechecked(CXAST, ExprTypeMap),
    Bytecode(ProgramBytecode),
    Codegen,
    Linked
}

impl PipelineStage {
    fn dump(&self) {
        match self {
            PipelineStage::Preprocessed(contents) => {
                dump_write("/// Preprocessed contents ///\n");
                dump_data(contents);
            },
            PipelineStage::Parsed(contents) => {
                dump_write("/// Parsed contents ///\n");
                dump_data(contents);
            },
            PipelineStage::Typechecked(ast, ..) => {
                dump_write("/// Typechecked AST ///\n");
                dump_data(ast);
            },
            PipelineStage::Bytecode(bytecode) => {
                dump_write("/// Bytecode ///\n");
                dump_data(bytecode);
            },
            _ => {
                dump_write(format!("Pipeline stage has no dump implementation: {:?}", self).as_str())
            }
        }
    }
}

impl CompilerPipeline {
    pub fn new(source: String, output: String) -> Self {
        let extensionless = source.replace(".cx", "");
        let el_as_path = Path::new(&extensionless);

        let internal = format!(".internal/{}{}", 
                               el_as_path.parent().unwrap().to_str().unwrap(),
                               el_as_path.file_stem().unwrap().to_str().unwrap());

        Self {
            source_dir: source,
            internal_dir: internal,
            file_name: el_as_path.file_stem().unwrap().to_str().unwrap().to_string(),

            output_file: output,

            pipeline_stage: PipelineStage::None,

            ..Self::default()
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

    pub fn dump(self) -> Self {
        self.pipeline_stage.dump();

        self
    }

    pub fn read_file(mut self) -> Self {
        let file_contents = std::fs::read_to_string(format!("{}", self.source_dir.as_str()))
            .expect(format!("PIPELINE ERROR: Failed to read source file \"{}\"", self.source_dir).as_str());

        self.pipeline_stage = PipelineStage::FileRead(file_contents);
        self
    }

    pub fn preprocess(mut self) -> Self {
        let PipelineStage::FileRead(file_contents) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot preprocess without reading a file!");
        };

        let preprocessed = preprocessor::preprocess(file_contents.as_str());

        self.pipeline_stage = PipelineStage::Preprocessed(preprocessed);
        self
    }

    pub fn lex(mut self) -> Self {
        let PipelineStage::Preprocessed(preprocessed) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot lex without preprocessing!");
        };

        let lexed = lex::generate_tokens(preprocessed.as_str());

        self.pipeline_stage = PipelineStage::Lexed(lexed);
        self
    }

    pub fn parse_types_and_deps(mut self) -> Self {
        let PipelineStage::Lexed(lexed) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot parse types and dependencies without lexing!");
        };

        let parser_data = ParserData::new(self.source_dir.clone(), lexed.as_slice());

        let (mut type_map, mut imports) = cx_compiler_ast::parse::parse_types_and_deps(parser_data, self.internal_dir.as_str())
            .expect("Failed to parse types and dependencies");

        let recursive_imports = request_type_compilation(imports.as_slice())
            .expect("Failed to request compile for imports");
        imports.extend(recursive_imports);
        
        for import in imports.iter() {
            let deserialized = cx_compiler_modules::deserialize_type_data(import)
                .expect(format!("Failed to deserialize module data for import: {}", import).as_str());
            
            for (type_name, cx_type) in deserialized {
                if type_map.contains_key(&type_name) {
                    eprintln!("WARNING: Type {} already exists in the type map, skipping import.", type_name);
                } else {
                    type_map.insert(type_name, cx_type);
                }
            }
        }


        self.imports = imports;
        self.pipeline_stage = PipelineStage::TypesAndDependences(lexed, type_map, self.imports.clone());
        self
    }

    pub fn parse(mut self) -> Self {
        let PipelineStage::TypesAndDependences(lexed, types, dependencies) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot parse without types and dependencies! Found stage: {:?}", self.pipeline_stage);
        };
        
        let mut parser_data = ParserData::new(self.source_dir.clone(), lexed.as_slice());
        
        for name in types.keys() {
            parser_data.type_symbols.insert(name.clone());
        }
        
        request_compile(dependencies.as_slice())
            .expect("Failed to request compile for dependencies");

        let Some(ast) = parse_ast(parser_data, self.internal_dir.as_str(), types, dependencies) else {
            panic!("ERROR: Failed to parse AST");
        };
        
        self.pipeline_stage = PipelineStage::Parsed(ast);
        self
    }

    pub fn verify(mut self) -> Self {
        let PipelineStage::Parsed(mut ast) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot verify without a parsed AST!");
        };

        let Some(expr_type_map) = type_check(&mut ast) else {
            dump_data(&ast);
            panic!("ERROR: Failed to verify AST");
        };

        self.pipeline_stage = PipelineStage::Typechecked(ast, expr_type_map);

        self
    }

    pub fn generate_bytecode(mut self) -> Self {
        let PipelineStage::Typechecked(ast, expr_type_map) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot generate bytecode without a verified AST!");
        };

        let Some(bytecode) = generate_bytecode(ast, expr_type_map) else {
            panic!("ERROR: Failed to generate bytecode");
        };

        self.pipeline_stage = PipelineStage::Bytecode(bytecode);
        self
    }

    pub fn take_ast(self) -> Option<CXAST> {
        let PipelineStage::Parsed(ast) = self.pipeline_stage else {
            panic!("PIPELINE ERROR: Cannot take AST without a parsed AST!");
        };

        Some(ast)
    }

    pub fn llvm_codegen(mut self) -> Self {
        let PipelineStage::Bytecode(bytecode) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot generate code without a parsed AST!");
        };
        
        let internal_path = Path::new(&self.internal_dir);
        std::fs::create_dir_all(internal_path.parent().unwrap().as_os_str())
            .expect("Failed to create internal directory");

        let output_path = format!("{}.o", self.internal_dir);
        cx_backend_llvm::bytecode_aot_codegen(&bytecode, output_path.as_str()).or_else(|| {
            panic!("ERROR: Failed to generate code");
        });

        self.pipeline_stage = PipelineStage::Codegen;
        self
    }

    pub fn cranelift_codegen(mut self) -> Self {
        let PipelineStage::Bytecode(bytecode) = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot generate code without a parsed AST!");
        };
        
        let internal_path = Path::new(&self.internal_dir);
        std::fs::create_dir_all(internal_path.parent().unwrap().as_os_str())
            .expect("Failed to create internal directory");
        
        let output_path = format!("{}.o", self.internal_dir);
        cx_backend_cranelift::bytecode_aot_codegen(&bytecode, output_path.as_str()).or_else(|| {
            panic!("ERROR: Failed to generate code");
        });

        self.pipeline_stage = PipelineStage::Codegen;
        self
    }

    pub fn link(mut self) -> Self {
        let PipelineStage::Codegen = std::mem::take(&mut self.pipeline_stage) else {
            panic!("PIPELINE ERROR: Cannot link without generating code!");
        };

        let output_path = format!("{}.o", self.internal_dir);
        let output_file = self.output_file.clone();
        
        let mut imports = HashSet::new();
        
        for import in &self.imports {
            let import_path = format!(".internal/{}.o", import);
            if !Path::new(&import_path).exists() {
                eprintln!("ERROR: Import path does not exist: {}", import_path);
                exit(1);
            }
            
            imports.insert(import_path);
        }

        let mut cmd = Command::new("gcc");
        cmd
            .arg(output_path)
            .arg("-o")
            .arg(output_file)
            .arg("-g")
            .arg("-no-pie");
        
        for import in imports {
            cmd.arg(import);
        }
        
        println!("Linking with command: {:?}", cmd);
        
        let status = cmd.status().expect("Failed to execute linker command");

        if !status.success() {
            eprintln!("ERROR: Linking failed with status: {}", status);
            exit(1);
        }
        
        println!("Successfully created executable: {}", self.output_file);

        self
    }
}