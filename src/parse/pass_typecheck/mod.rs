use crate::parse::FileInformation;
use crate::parse::interface_serializer::emit_types;
use crate::parse::value_type::ValueType;
use crate::parse::pass_molded::{CXGlobalStmt, CXParameter, FunctionMap, TypeMap, CXAST};
use crate::parse::pass_typecheck::checker::{type_check_traverse};
use crate::parse::pass_typecheck::intrinsic_types::add_internal_types;
use crate::util::ScopedMap;

mod checker;
mod type_utils;
mod intrinsic_types;

pub fn type_check(file_information: &FileInformation, ast: &mut CXAST) -> Option<()> {
    emit_types(
        &file_information.file_name,
        &file_information.file_path,
        ast.type_map.iter(),
    ).ok()?;

    add_internal_types(&mut ast.type_map);

    let mut type_environment = TypeEnvironment {
        type_map: &ast.type_map,
        fn_map: &ast.function_map,
        symbol_table: ScopedMap::new(),
        return_type: ValueType::Unit,
    };

    // TODO: Global Variables

    for function in &mut ast.global_stmts {
        let CXGlobalStmt::FunctionDefinition { parameters, body, return_type, .. } = function else {
            continue;
        };

        type_environment.return_type = return_type.clone();
        type_environment.symbol_table.push_scope();

        for CXParameter { type_, name } in parameters.iter() {
            if let Some(name) = name {
                type_environment.symbol_table.insert(name.clone(), type_.clone());
            }
        }

        type_check_traverse(&mut type_environment, body);
        type_environment.symbol_table.pop_scope();
    }

    Some(())
}

pub(crate) struct TypeEnvironment<'a> {
    type_map: &'a TypeMap,
    fn_map: &'a FunctionMap,
    symbol_table: ScopedMap<ValueType>,

    return_type: ValueType,
}