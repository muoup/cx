use cx_data_ast::parse::ast::{CXGlobalStmt, CXParameter, FunctionMap, TypeMap, CXAST};
use cx_data_ast::parse::value_type::CXValType;
use cx_util::scoped_map::ScopedMap;
use crate::checker::type_check_traverse;
use crate::intrinsic_types::add_internal_types;
use crate::mappings::parse_fn_mappings;

pub mod type_utils;
pub mod checker;
pub mod intrinsic_types;
pub mod mappings;
mod struct_typechecking;

pub fn type_check(ast: &mut CXAST) -> Option<()> {
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
                type_environment.symbol_table.insert(name.to_owned(), type_.clone());
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
