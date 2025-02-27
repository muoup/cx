use crate::parse::ast::{GlobalStatement, ValueType, AST};
use crate::parse::verify::context::{FnMap, TypeMap};

pub(crate) fn gen_declarations(ast: &AST) -> Option<(TypeMap, FnMap)> {
    Some((gen_type_decls(ast)?, gen_fn_decls(ast)?))
}

pub(crate) fn gen_fn_decls(ast: &AST) -> Option<FnMap> {
    let mut map = FnMap::new();

    for statement in &ast.statements {
        let GlobalStatement::Function {
            prototype, body
        } = statement else {
            continue;
        };

        map.insert(
            prototype.name.clone(),
            prototype.clone()
        );
    }

    Some(map)
}


pub(crate) fn gen_type_decls(ast: &AST) -> Option<TypeMap> {
    let mut map = base_type_defs();

    for statement in &ast.statements {
        let GlobalStatement::TypeDeclaration { name, type_, .. } = statement else {
            continue;
        };

        let Some(name) = name else {
            continue;
        };

        map.insert(
            name.clone(),
            type_.clone()
        );
    }

    Some(map)
}

fn base_type_defs() -> TypeMap {
    let mut type_defs: TypeMap = TypeMap::new();

    type_defs.insert("i8" .to_owned(), ValueType::Integer{ bytes: 1, signed: true   }.into());
    type_defs.insert("i16".to_owned(), ValueType::Integer{ bytes: 2, signed: true   }.into());
    type_defs.insert("i32".to_owned(), ValueType::Integer{ bytes: 4, signed: true   }.into());
    type_defs.insert("i64".to_owned(), ValueType::Integer{ bytes: 8, signed: true   }.into());
    type_defs.insert("u8" .to_owned(), ValueType::Integer{ bytes: 1, signed: false  }.into());
    type_defs.insert("u16".to_owned(), ValueType::Integer{ bytes: 2, signed: false  }.into());
    type_defs.insert("u32".to_owned(), ValueType::Integer{ bytes: 4, signed: false  }.into());
    type_defs.insert("u64".to_owned(), ValueType::Integer { bytes: 8, signed: false }.into());
    type_defs.insert("f32".to_owned(), ValueType::Float   { bytes: 4                }.into());
    type_defs.insert("f64".to_owned(), ValueType::Float   { bytes: 8                }.into());

    type_defs.insert("int".to_owned(), type_defs.get("i32").unwrap().clone());
    type_defs.insert("unsigned int".to_owned(), type_defs.get("u32").unwrap().clone());
    type_defs.insert("float".to_owned(), type_defs.get("f32").unwrap().clone());
    type_defs.insert("double".to_owned(), type_defs.get("f64").unwrap().clone());
    type_defs.insert("char".to_owned(), type_defs.get("i8").unwrap().clone());
    type_defs.insert("unsigned char".to_owned(), type_defs.get("u8").unwrap().clone());

    type_defs.insert("void".to_owned(), ValueType::Unit.into());

    type_defs
}