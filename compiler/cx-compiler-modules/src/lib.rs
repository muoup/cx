use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXAST};
use cx_data_ast::parse::value_type::CXType;

pub struct ModuleData {
    pub types: Vec<(String, CXType)>,
    pub functions: Vec<CXFunctionPrototype>,
}

pub fn serialize_module_data(ast: &CXAST) -> Option<()> {
    let directory = Path::new(&ast.internal_path);
    std::fs::create_dir_all(directory.parent()?)
        .expect("Failed to create internal directory");
    
    let mut type_file = File::create(format!("{}.cx-types", ast.internal_path))
        .expect("Failed to create type file");
    
    for (type_name, cx_type) in &ast.type_map {
        let serialized = serde_json::to_string(&(type_name, cx_type))
            .expect("Failed to serialize type");
        
        type_file.write_all(serialized.as_bytes())
            .and_then(|_| type_file.write_all(b"\n"))
            .expect("Failed to write type to file");
    }
    
    let mut function_file = File::create(format!("{}.cx-functions", ast.internal_path))
        .expect("Failed to create function file");
    
    for (_, function) in &ast.function_map {
        let serialized = serde_json::to_string(function)
            .expect("Failed to serialize function");
        
        function_file.write_all(serialized.as_bytes())
            .and_then(|_| function_file.write_all(b"\n"))
            .expect("Failed to write function to file");
        
    }
    
    Some(())
}

pub fn deserialize_module_data(file_path: &str) -> Option<ModuleData> {
    let mut module_data = ModuleData {
        types: Vec::new(),
        functions: Vec::new(),
    };
    
    let type_file_path = format!(".internal/{}.cx-types", file_path);
    let function_file_path = format!(".internal/{}.cx-functions", file_path);
    
    let type_file = File::open(&type_file_path)
        .expect(format!("Failed to open type file: {}", type_file_path).as_str());
    
    for line in BufReader::new(type_file).lines() {
        let line = line.expect("Failed to read line from type file");
        let (type_name, cx_type): (String, CXType) = serde_json::from_str(&line)
            .expect("Failed to deserialize type");
        
        module_data.types.push((type_name, cx_type));
    }
    
    let function_file = File::open(&function_file_path)
        .expect("Failed to open function file");
    
    for line in BufReader::new(function_file).lines() {
        let line = line.expect("Failed to read line from function file");
        let function: CXFunctionPrototype = serde_json::from_str(&line)
            .expect("Failed to deserialize function");
        
        module_data.functions.push(function);
    }
    
    Some(module_data)
}