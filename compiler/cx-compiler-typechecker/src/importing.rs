use std::path::Path;
use std::process::exit;
use cx_compiler_modules::deserialize_module_data;
use cx_data_ast::parse::ast::CXAST;

pub(crate) fn import_module_data(cxast: &mut CXAST) {
    for import_path in cxast.imports.iter() {
        let type_path_str = format!("{}.cx-types", cxast.internal_path);
        let internal_type_path = Path::new(&type_path_str);

        let cx_path_str = if import_path.starts_with("std") {
            let current_exe = std::env::current_exe()
                .expect("Failed to get current executable path");
            format!("{}/lib/std/{}.cx", current_exe.parent().unwrap().display(), &import_path[3..])
        } else {
            format!("{}.cx", import_path)
        };

        let cx_path = Path::new(&cx_path_str);
        
        if !internal_type_path.exists() {
            eprintln!("Import path does not exist: {}", type_path_str);
            exit(1);
        }
        
        if !cx_path.exists() {
            eprintln!("Source file does not exist: {}", cx_path_str);
            exit(1);
        }
        
        let module_data = deserialize_module_data(import_path.as_str())
            .expect("Failed to deserialize module data");
     
        cxast.type_map.extend(module_data.types);
        cxast.function_map.extend(module_data.functions.into_iter()
            .map(|func| (func.name.as_string(), func)));
    }
}