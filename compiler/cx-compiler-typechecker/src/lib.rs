use crate::deconstructed_types::generate_deconstructor_data;
use crate::global_stmts::{add_destructor_prototypes, typecheck_destructor, typecheck_function};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXGlobalStmt, CXGlobalVariable, CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTemplateRequest, CXTypeMap};
use cx_data_ast::parse::template::CXTemplateInput;
use cx_data_ast::parse::value_type::CXType;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_lexer::token::Token;
use cx_util::mangling::mangle_templated_fn;
use cx_util::scoped_map::ScopedMap;
use std::collections::HashMap;

pub mod typemap_collapsing;
pub mod deconstructed_types;
pub mod checker;
mod struct_typechecking;
mod casting;
mod global_stmts;
mod structured_initialization;

pub type TypeCheckResult<T> = Option<T>;

pub fn type_check(tokens: &[Token], ast: &mut CXAST) -> Option<TypeCheckData> {
    let mut type_environment = TypeEnvironment {
        tokens,
        
        type_map: &mut ast.type_map,
        fn_map: &mut ast.function_map,
        
        symbol_table: ScopedMap::new(),
        global_variables: &ast.global_variables,
        
        current_prototype: None,
        typecheck_data: TypeCheckData::new(),
    };
    
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

    add_destructor_prototypes(
        &mut type_environment.type_map,
        &mut type_environment.fn_map,
    )?;
    
    type_environment.typecheck_data.deconstructor_data
        = generate_deconstructor_data(&type_environment)?;
    
    Some(type_environment.typecheck_data)
}

pub fn template_fn_typecheck(tokens: &[Token], ast: &CXAST, request: CXTemplateRequest) -> Option<(TypeCheckData, CXFunctionPrototype, CXGlobalStmt)> {
    let mut type_map = ast.type_map.clone();
    let mut fn_map = ast.function_map.clone();
    
    let args = ast.function_map
        .template_args(&request.template_name)
        .unwrap_or_else(|| {
            panic!("Template generator not found in type map for {}", request.template_name)
        });
    
    for (name, _type) in args.iter().zip(&request.input.params) {
        type_map.insert(name.clone(), _type.clone());
    }
    
    let mangled_name = mangle_templated_fn(
        &request.template_name,
        &request.input.params
    );
    let mut prototype = fn_map.get_template(&ast.type_map, &request.template_name, request.input)
        .expect("Template generator not found in type map");
    
    let mut env = TypeEnvironment {
        tokens,
        
        type_map: &mut type_map,
        fn_map: &mut fn_map,
        
        symbol_table: ScopedMap::new(),
        global_variables: &ast.global_variables,
        
        current_prototype: None,
        typecheck_data: TypeCheckData::new(),
    };
    
    let mut body = ast.global_stmts.iter()
        .find_map(
            |stmt| match stmt {
                CXGlobalStmt::TemplatedFunction { fn_name, body } 
                if fn_name == &prototype.name 
                    => Some(body),
                _ => None,
            }
        )
        .unwrap_or_else(|| {
            panic!("Function template body not found for {}", prototype.name);
        })
        .as_ref()
        .clone();
    
    typecheck_function(&mut env, &prototype, &mut body)?;
    prototype.name.data = mangled_name;
    
    Some((env.typecheck_data, prototype.clone(), CXGlobalStmt::FunctionDefinition {
        prototype,
        body: Box::new(body),
    }))
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

impl TypeEnvironment<'_> {
    pub fn function_template_prototype(&self, name: &str, input: CXTemplateInput) -> Option<CXFunctionPrototype> {
        self.fn_map.get_template(self.type_map, name, input)
    }
}