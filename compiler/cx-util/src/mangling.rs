use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};

pub fn mangle_destructor(type_name: &str) -> String {
    format!("__destructor_{type_name}")
}

pub fn mangle_member_function(type_id: impl Display, function_name: &str) -> String {
    format!("_{type_id}_{function_name}")
}

pub fn mangle_templated_fn<ArgType: Hash>(fn_name: &str, template_args: &[ArgType]) -> String {
    let mut hash = DefaultHasher::new();
    for arg in template_args {
        arg.hash(&mut hash);
    }
    format!("{}_{}", fn_name, hash.finish())
}

pub fn mangle_templated_type<ArgType: Hash>(type_name: &str, template_args: &[ArgType]) -> String {
    let mut hash = DefaultHasher::new();
    for arg in template_args {
        arg.hash(&mut hash);
    }
    format!("{}_{}", type_name, hash.finish())
}