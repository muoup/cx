pub(crate) fn member_function_mangle(struct_name: &str, fn_name: &str) -> String {
    format!("_{}_{}", struct_name, fn_name)
}