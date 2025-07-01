use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use cx_data_ast::parse::ast::{CXFunctionMap, CXFunctionPrototype, CXTypeMap, CXAST};
use cx_data_ast::parse::value_type::CXType;

pub struct ModuleData {
    pub types: Vec<(String, CXType)>,
    pub functions: Vec<CXFunctionPrototype>,
}

pub fn serialize_type_data<'a>(internal_path: &str, type_map: &CXTypeMap, types: impl Iterator<Item = &'a String>) -> Option<()> {
    let directory = Path::new(internal_path);
    
    std::fs::create_dir_all(format!("{}/{}", directory.parent()?.display(), directory.file_stem()?.display()))
        .expect("Failed to create internal directory");
    let type_file_dir = format!("{internal_path}/.cx-types");
    let mut type_file = File::create(type_file_dir)
        .expect("Failed to create type file");
    
    for type_name in types {
        let cx_type = type_map.get(type_name)
            .expect("Type not found in type map");
        let serialized = serde_json::to_string(&(type_name, cx_type))
            .expect("Failed to serialize type");
        
        type_file.write_all(serialized.as_bytes())
            .and_then(|_| type_file.write_all(b"\n"))
            .expect("Failed to write type to file");
    }
    
    Some(())
}

pub fn serialize_function_data<'a>(internal_path: &str, function_map: &CXFunctionMap, functions: impl Iterator<Item = &'a String>) -> Option<()> {
    let directory = Path::new(internal_path);
    std::fs::create_dir_all(directory.parent()?)
        .expect("Failed to create internal directory");
    
    let mut function_file = File::create(format!("{internal_path}/.cx-functions"))
        .expect("Failed to create function file");
    
    for fn_name in functions {
        let function = function_map.get(fn_name)
            .expect("Function not found in function map");
        
        let serialized = serde_json::to_string(function)
            .expect("Failed to serialize function");
        
        function_file.write_all(serialized.as_bytes())
            .and_then(|_| function_file.write_all(b"\n"))
            .expect("Failed to write function to file");
    }
    
    Some(())
}

pub fn deserialize_type_data(file_path: &str) -> Option<Vec<(String, CXType)>> {
    let mut types = Vec::new();
    
    let type_file_path = format!(".internal/{file_path}/.cx-types");
    let type_file = File::open(&type_file_path)
        .unwrap_or_else(|_| panic!("Failed to open type file: {type_file_path}"));
    
    for line in BufReader::new(type_file).lines() {
        let line = line.expect("Failed to read line from type file");
        let (type_name, cx_type): (String, CXType) = serde_json::from_str(&line)
            .expect("Failed to deserialize type");
        
        types.push((type_name, cx_type));
    }
    
    Some(types)
}

pub fn deserialize_function_data(file_path: &str) -> Option<Vec<CXFunctionPrototype>> {
    let mut functions = Vec::new();
    
    let function_file_path = format!(".internal/{file_path}/.cx-functions");
    let function_file = File::open(&function_file_path)
        .expect("Failed to open function file");
    
    for line in BufReader::new(function_file).lines() {
        let line = line.expect("Failed to read line from function file");
        let function: CXFunctionPrototype = serde_json::from_str(&line)
            .expect("Failed to deserialize function");
        
        functions.push(function);
    }
    
    Some(functions)
}

pub fn serialize_module_data(ast: &CXAST) -> Option<()> {
    let directory = Path::new(&ast.internal_path);
    std::fs::create_dir_all(directory.parent()?)
        .expect("Failed to create internal directory");
    
    let mut type_file = File::create(format!("{}/.cx-types", ast.internal_path))
        .expect("Failed to create type file");
    
    for (type_name, cx_type) in &ast.type_map {
        let serialized = serde_json::to_string(&(type_name, cx_type))
            .expect("Failed to serialize type");
        
        type_file.write_all(serialized.as_bytes())
            .and_then(|_| type_file.write_all(b"\n"))
            .expect("Failed to write type to file");
    }
    
    let mut function_file = File::create(format!("{}/.cx-functions", ast.internal_path))
        .expect("Failed to create function file");
    
    for function in ast.function_map.values() {
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
    
    let type_file_path = format!(".internal/{file_path}/.cx-types");
    let function_file_path = format!(".internal/{file_path}/.cx-functions");
    
    let type_file = File::open(&type_file_path)
        .unwrap_or_else(|_| panic!("Failed to open type file: {type_file_path}"));
    
    for line in BufReader::new(type_file).lines() {
        let line = line.expect("Failed to read line from type file");
        let (type_name, cx_type): (String, CXType) = serde_json::from_str(&line)
            .expect("Failed to deserialize type");
        
        module_data.types.push((type_name, cx_type));
    }
    
    let function_file = File::open(&function_file_path)
        .unwrap_or_else(|_| panic!("Failed to open function file: {function_file_path}"));
    
    for line in BufReader::new(function_file).lines() {
        let line = line.expect("Failed to read line from function file");
        let function: CXFunctionPrototype = serde_json::from_str(&line)
            .expect("Failed to deserialize function");
        
        module_data.functions.push(function);
    }
    
    Some(module_data)
}