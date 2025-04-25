use std::io::Write;
use crate::parse::value_type::CXValType;

pub fn emit_types<'a>(
    file_path: &str,
    file_name: &str,
    types: impl Iterator<Item = (&'a String, &'a CXValType)>,
) -> std::fmt::Result {
    let with_changed_extension = file_name
        .split('.')
        .next()
        .unwrap_or(file_name)
        .to_string()
        + ".cx_type";

    let file_path = ".internal/".to_string() + file_path + "/" + with_changed_extension.as_str();
    let mut file = std::fs::File::create(file_path).expect("Failed to create file");

    for (type_name, type_) in types {
        emit_type(&mut file, type_name, type_)?;
    }

    Ok(())
}

fn emit_type(
    file: &mut std::fs::File,
    name: &str,
    type_: &CXValType,
) -> std::fmt::Result {
    write!(file, "{}: {}\n", name, type_).unwrap();

    Ok(())
}