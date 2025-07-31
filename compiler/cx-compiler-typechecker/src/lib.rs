use crate::deconstructed_types::generate_deconstructor_data;
use crate::global_stmts::{add_destructor_prototypes, typecheck_destructor, typecheck_function};
use crate::importing::import_module_data;
use cx_data_ast::lex::token::Token;
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXGlobalStmt, CXGlobalVariable, CXAST};
use cx_data_ast::parse::value_type::CXType;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_util::scoped_map::ScopedMap;
use std::collections::HashMap;
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};

pub mod typemap_collapsing;
pub mod deconstructed_types;
pub mod checker;
mod struct_typechecking;
mod casting;
mod importing;
mod global_stmts;
mod structured_initialization;

pub type TypeCheckResult<T> = Option<T>;

pub fn type_check(tokens: &[Token], ast: &mut CXAST) -> Option<TypeCheckData> {
    import_module_data(ast);

    let mut type_environment = TypeEnvironment {
        tokens,
        
        type_map: &mut ast.type_map,
        fn_map: &mut ast.function_map,
        
        symbol_table: ScopedMap::new(),
        global_variables: &ast.global_variables,
        
        current_prototype: None,
        typecheck_data: TypeCheckData::new(),
    };
    
    add_destructor_prototypes(
        type_environment.type_map,
        type_environment.fn_map,
    )?;
    
    for stmt in &mut ast.global_stmts {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } =>
                typecheck_function(&mut type_environment, prototype, body)?,
            CXGlobalStmt::DestructorDefinition { type_name, body } =>
                typecheck_destructor(&mut type_environment, type_name, body)?,
            CXGlobalStmt::GlobalVariable { .. } =>
                todo!("Global variable type checking is not implemented yet"),
            
            _ => continue,
        }
    }
    
    type_environment.typecheck_data.deconstructor_data
        = generate_deconstructor_data(&type_environment)?;
    
    Some(type_environment.typecheck_data)
}

pub(crate) struct TypeEnvironment<'a> {
    tokens: &'a [Token],
    
    type_map: &'a mut CXTypeMap,
    fn_map: &'a mut CXFunctionMap,
    symbol_table: ScopedMap<CXType>,
    typecheck_data: TypeCheckData,
    
    global_variables: &'a HashMap<String, CXGlobalVariable>,
    
    current_prototype: Option<CXFunctionPrototype>,
}
