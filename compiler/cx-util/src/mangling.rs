use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};

pub fn mangle_deconstructor(name: &str) -> String {
    format!("__deconstructor_{name}")
}
pub fn mangle_destructor(type_name: impl Display) -> String {
    format!("__destructor_{type_name}")
}

pub fn mangle_member_function(type_id: impl Display, function_name: &str) -> String {
    format!("_{type_id}_{function_name}")
}

pub fn mangle_template<ArgType: Hash>(name: &str, template_args: &[ArgType]) -> String {
    let mut hash = DefaultHasher::new();
    for arg in template_args {
        arg.hash(&mut hash);
    }
    format!("{}_{}", name, hash.finish())
}
