pub(crate) fn member_function_mangle(
    class_name: &str,
    function_name: &str,
) -> String {
    format!("__int_member_fn_{}__{}", class_name, function_name)
}