use std::fmt::Display;

pub fn mangle_deconstructor(name: &str) -> String {
    format!("__deconstructor_{name}")
}

pub fn mangle_destructor(type_name: impl Display) -> String {
    format!("__destructor_{type_name}")
}

pub fn mangle_member_function(type_id: impl Display, function_name: &str) -> String {
    format!("_{type_id}_{function_name}")
}