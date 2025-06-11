use std::path::Path;
use std::process::exit;
use cx_compiler_modules::deserialize_module_data;
use cx_data_ast::parse::ast::CXAST;

pub(crate) fn import_module_data(cxast: &mut CXAST) {
    for import in cxast.imports.iter() {
        let internal_type_path = format!(".internal/{}.cx-types", import);
        let internal_type_path = Path::new(&internal_type_path);
        
        let cx_path = format!("{}.cx", import);
        let cx_path = Path::new(&cx_path);
        
        if !internal_type_path.exists() || !cx_path.exists() {
            eprintln!("Import path does not exist: {}", import);
            exit(1);
        }
        
        let module_data = deserialize_module_data(import.as_str())
            .expect("Failed to deserialize module data");
     
        cxast.type_map.extend(module_data.types);
        cxast.function_map.extend(module_data.functions.into_iter()
            .map(|func| (func.name.as_string(), func)));
    }
}