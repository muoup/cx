use cx_data_ast::parse::intrinsic_types::INTRINSIC_TYPES;
use cx_data_ast::parse::maps::CXTypeMap;

pub fn add_intrinsic_imports(imports: &mut Vec<String>) {
    macro_rules! add {
        ($name:expr) => {
            imports.push($name.to_string());
        };
    }

    add!("std/intrinsic/memory");
}

pub fn add_intrinsic_types(type_map: &mut CXTypeMap) {
    for (name, _type) in INTRINSIC_TYPES {
        type_map.insert(name.to_string(), _type.clone().to_val_type());
    }
}