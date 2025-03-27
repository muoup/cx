use std::io::Write;
use crate::parse::ast::{GlobalStatement, ValueType, AST};

pub fn emit_interface(ast: &AST, output_path: &str) -> Option<()> {
    let directory = output_path.rfind('/')
        .map(|index| &output_path[..index])
        .or(Some(""))?;

    std::fs::create_dir_all(directory)
        .expect("Failed to create output directory");
    let mut file = std::fs::File::create(output_path)
        .expect("Failed to create output file");

    for index in ast.public_interface.iter() {
        let stmt = &ast.statements[*index];
        let as_str = global_stmt_as_str(stmt)?;

        file.write(b"\n").ok()?;
        file.write_all(as_str.as_bytes()).ok()?;
    }

    Some(())
}

pub(crate) fn global_stmt_as_str(global_statement: &GlobalStatement) -> Option<String> {
    match global_statement {
        GlobalStatement::Function { prototype, .. } => {
            let mut header = initialization_as_name(
                &prototype.return_type,
                prototype.name.clone()
            )?;

            header.push('(');

            for (index, arg) in prototype.args.iter().enumerate() {
                if index != 0 {
                    header.push_str(", ");
                }

                header.push_str(&initialization_as_name(
                    &arg.type_,
                    arg.name.clone()
                )?);
            }

            header.push_str(");\n");

            Some(header)
        },

        GlobalStatement::MemberFunction {
            struct_parent, prototype, ..
        } => {
            let mut header = initialization_as_name(
                &prototype.return_type,
                String::new()
            )?;

            header.push_str(&format!("{}::{}", struct_parent, prototype.name));
            header.push_str("(this");

            for arg in prototype.args.iter() {
                header.push_str(", ");
                header.push_str(&initialization_as_name(
                    &arg.type_,
                    arg.name.clone()
                )?);
            }

            header.push_str(");\n");

            Some(header)
        },

        GlobalStatement::TemplatedFunction {
            ..
        } => {
            Some("// templated functions unimplemented".to_string())
        },

        GlobalStatement::TypeDeclaration {
            name: Some(name),
            type_,
        } => {
            let header = initialization_as_name(type_, name.clone())?;
            Some(format!("typedef {}\n", header))
        },

        _ => Some(String::new())
    }
}

pub(crate) fn initialization_as_name(value_type: &ValueType, name: String) -> Option<String> {
    match value_type {
        ValueType::Identifier(type_name) => Some(format!("{} {}", type_name, name)),
        ValueType::PointerTo(inner) => {
            initialization_as_name(inner.as_ref(), format!("*{}", name))
        },
        ValueType::Array { size, _type } => {
            initialization_as_name(_type.as_ref(), format!("{}[{}]", name, size))
        },
        ValueType::Structured { fields } => {
            let mut header = format!("struct {{\n");

            for field in fields {
                header.push_str("\t");
                header.push_str(&initialization_as_name(
                    &field.type_,
                    field.name.clone()
                )?);
                header.push_str(";\n");
            }

            header.push_str("}");

            Some(format!("{} {}", header, name))
        },

        _ => unimplemented!("initialization: {:#?}", value_type)
    }
}