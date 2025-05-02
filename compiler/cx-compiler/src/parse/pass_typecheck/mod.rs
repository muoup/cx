use crate::log_error;
use crate::parse::FileInformation;
use crate::parse::interface_serializer::emit_types;
use crate::parse::value_type::CXValType;
use crate::parse::pass_molded::{CXGlobalStmt, CXParameter, FunctionMap, TypeMap, CXAST};
use crate::parse::pass_typecheck::checker::{type_check_traverse};
use crate::parse::pass_typecheck::intrinsic_types::add_internal_types;
use crate::parse::pass_typecheck::mappings::parse_fn_mappings;
use crate::util::ScopedMap;

pub mod type_utils;

mod checker;
mod intrinsic_types;
mod mappings;

pub fn type_check(file_information: &FileInformation, ast: &mut CXAST) -> Option<()> {
    emit_types(
        &file_information.file_name,
        &file_information.file_path,
        ast.type_map.iter(),
    ).ok()?;

    parse_fn_mappings(ast);
    add_internal_types(&mut ast.type_map);

    let mut type_environment = TypeEnvironment {
        type_map: &mut ast.type_map,
        fn_map: &mut ast.function_map,
        symbol_table: ScopedMap::new(),
        return_type: CXValType::Unit,
    };

    // TODO: Global Variables

    for function in &mut ast.global_stmts {
        let CXGlobalStmt::FunctionDefinition { prototype, body } = function else {
            continue;
        };

        type_environment.return_type = prototype.return_type.clone();
        type_environment.symbol_table.push_scope();

        for CXParameter { type_, name } in prototype.parameters.iter() {
            if let Some(name) = name {
                type_environment.symbol_table.insert(name.clone(), type_.clone());
            }
        }

        type_check_traverse(&mut type_environment, body)?;

        type_environment.symbol_table.pop_scope();
    }

    Some(())
}

pub(crate) struct TypeEnvironment<'a> {
    type_map: &'a mut TypeMap,
    fn_map: &'a mut FunctionMap,
    symbol_table: ScopedMap<CXValType>,

    return_type: CXValType,
}