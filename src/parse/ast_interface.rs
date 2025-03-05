use std::fs::File;
use std::io::Write;
use crate::parse::ast::{GlobalStatement, ValueType, AST};

pub fn emit_interface(ast: &AST, output_path: &str) -> Option<()> {
    let mut file = File::create(output_path).ok()?;

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
        GlobalStatement::Function { prototype, body } => {
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
            initialization_as_name(inner.as_ref(), format!("(*{})", name))
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