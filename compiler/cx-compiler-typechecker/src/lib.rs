use crate::deconstructed_types::generate_deconstructor_data;
use crate::global_stmts::{add_destructor_prototypes, typecheck_destructor, typecheck_function};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXGlobalStmt, CXGlobalVariable, CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_typechecker::template::CXTemplateInput;
use cx_data_typechecker::cx_types::CXType;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_lexer::token::Token;
use cx_util::mangling::mangle_templated_fn;
use cx_util::scoped_map::ScopedMap;
use std::collections::HashMap;
use cx_data_ast::parse::identifier::CXIdent;

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
        typecheck_data: TypeCheckData::new(&ast.destructor_map),
    };
    
    for stmt in &mut ast.global_stmts {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } =>
                typecheck_function(&mut type_environment, prototype, body)?,
            CXGlobalStmt::GlobalVariable { .. } =>
                todo!("Global variable type checking is not implemented yet"),
            
            _ => continue,
        }
    }

    add_destructor_prototypes(&type_environment.typecheck_data.destructor_map, type_environment.fn_map)?;
    type_environment.typecheck_data.deconstructor_data = generate_deconstructor_data(&type_environment)?;

    for stmt in &mut ast.global_stmts {
        match stmt {
            CXGlobalStmt::DestructorDefinition { type_name, body } =>
                typecheck_destructor(&mut type_environment, type_name, body)?,
            _ => continue,
        }
    }
    
    Some(type_environment.typecheck_data)
}

pub fn template_fn_typecheck(tokens: &[Token], ast: &CXAST, input: CXTemplateInput, mut prototype: CXFunctionPrototype) -> Option<(TypeCheckData, CXFunctionPrototype, CXGlobalStmt)> {
    let mut type_map = ast.type_map.clone();
    let mut fn_map = ast.function_map.clone();
    
    let args = ast.function_map
        .template_args(&prototype.name.as_string())
        .unwrap_or_else(|| {
            panic!("Template generator not found in type map for {}", &prototype.name.as_string())
        });
    
    for (name, _type) in args.iter().zip(&input.params) {
        type_map.insert(name.clone(), _type.clone());
    }
    
    let mut env = TypeEnvironment {
        tokens,
        
        type_map: &mut type_map,
        fn_map: &mut fn_map,
        
        symbol_table: ScopedMap::new(),
        global_variables: &ast.global_variables,
        
        current_prototype: None,
        typecheck_data: TypeCheckData::new(&ast.destructor_map),
    };
    
    let prototype_name = prototype.name.as_string();
    
    let mut body = ast.global_stmts.iter()
        .find_map(
            |stmt| match stmt {
                CXGlobalStmt::TemplatedFunction { prototype, body, .. } 
                    if prototype.name.as_string() == prototype_name => Some(body),
                _ => None,
            }
        )
        .unwrap_or_else(|| {
            panic!("Function template body not found for {}", prototype_name);
        })
        .as_ref()
        .clone();
    prototype.name.map_name_ident(|data| mangle_templated_fn(data, &input.params));
    
    typecheck_function(&mut env, &prototype, &mut body)?;
    
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