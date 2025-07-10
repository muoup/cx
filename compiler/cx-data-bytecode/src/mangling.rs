pub fn mangle_destructor(type_name: &str) -> String {
    format!("__destructor_{}", type_name)
}