use cx_data_ast::lex::token::{TokenKind, Token};
use cx_data_ast::parse::ast::{CXFunctionMap, CXGlobalStmt, CXParameter, CXTypeMap, CXAST};
use cx_data_ast::parse::value_type::CXType;
use cx_data_bytecode::node_type_map::ExprTypeMap;
use cx_util::scoped_map::ScopedMap;
use crate::checker::type_check_traverse;
use crate::importing::import_module_data;

pub mod type_utils;
pub mod checker;
mod struct_typechecking;
mod casting;
mod importing;

pub fn type_check(ast: &mut CXAST) -> Option<ExprTypeMap> {
    import_module_data(ast);

    let mut type_environment = TypeEnvironment {
        tokens: &ast.tokens,
        
        type_map: &mut ast.type_map,
        fn_map: &mut ast.function_map,
        
        symbol_table: ScopedMap::new(),
        return_type: CXType::unit(),
        expr_type_map: ExprTypeMap::new(),
    };

    // TODO: Global Variables

    for function in &mut ast.global_stmts {
        let CXGlobalStmt::FunctionDefinition { prototype, body } = function else {
            continue;
        };

        type_environment.return_type = prototype.return_type.clone();
        type_environment.symbol_table.push_scope();

        for CXParameter { type_, name } in prototype.params.iter() {
            if let Some(name) = name {
                type_environment.symbol_table.insert(name.as_string(), type_.clone());
            }
        }

        type_check_traverse(&mut type_environment, body)?;

        type_environment.symbol_table.pop_scope();
    }

    Some(type_environment.expr_type_map)
}

pub(crate) struct TypeEnvironment<'a> {
    tokens: &'a [Token],
    
    type_map: &'a mut CXTypeMap,
    fn_map: &'a mut CXFunctionMap,
    symbol_table: ScopedMap<CXType>,
    expr_type_map: ExprTypeMap,
    
    return_type: CXType,
}
