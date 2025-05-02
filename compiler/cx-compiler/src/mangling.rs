use std::fmt::Display;

pub(crate) fn namespace_mangle(
    names: &Vec<impl Display>
) -> String {
    match names.len() {
        0..=1 => panic!("Namespace mangling should not occur over less than 2 names"),
        2 => format!("__{}_{}", names[0], names[1]),
        _ => {
            let mut mangled = format!("__{}", names[0]);
            for name in names.iter().skip(1) {
                mangled.push_str(&format!("_{}", name));
            }
            mangled
        }
    }
}