pub(crate) fn member_function_mangle(
    class_name: &str,
    function_name: &str,
) -> String {
    format!("__member_fn_{}_{}", class_name, function_name)
}