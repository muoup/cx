use crate::module::QualifiedName;

pub fn mangle_static_symbol(symbol_name: &str, function_name: &str) -> String {
    format!("_S{}_{}_{}", symbol_name.len(), symbol_name, function_name)
}

pub fn mangle_namespace_symbol(name: &QualifiedName) -> String {
    let mut mangled = String::from("_N");
    let mut push_component = |str: &str| {
        mangled.push_str(str.len().to_string().as_str());
        mangled.push('_');
        mangled.push_str(str);
    };

    push_component(name.namespace.segments().len().to_string().as_str());

    for segment in name.namespace.segments() {
        push_component(segment.as_str());
    }

    push_component(name.name.as_str());
    mangled
}