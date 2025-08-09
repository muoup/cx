use cx_data_ast::parse::ast::{CXExpr, CXFunctionPrototype, CXParameter};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::maps::{CXDestructorMap, CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_util::mangling::mangle_destructor;
use cx_util::log_error;
use crate::checker::type_check_traverse;
use crate::TypeEnvironment;

pub(crate) fn add_destructor_prototypes(
    destructor_definitions: &CXDestructorMap,
    fn_map: &mut CXFunctionMap,
) -> Option<()> {
    for (_type, name) in destructor_definitions.iter() {
        let destructor_name = mangle_destructor(name);

        let this_type = CXType::new(
            0,
            CXTypeKind::PointerTo {
                inner_type: Box::new(_type.clone()),

                sizeless_array: false,
                weak: true,
                nullable: false,
            },
        );

        let prototype = CXFunctionPrototype {
            name: CXIdent::from_owned(destructor_name.clone()),
            params: vec![CXParameter { name: None, _type: this_type }],
            return_type: CXType::unit(),
            var_args: false
        };

        fn_map.insert(destructor_name, prototype);
    }
    Some(())
}

pub(crate) fn typecheck_function(
    env: &mut TypeEnvironment,
    prototype: &CXFunctionPrototype,
    body: &mut CXExpr,
) -> Option<()> {
    env.current_prototype = Some(prototype.clone());
    env.symbol_table.push_scope();

    for CXParameter { _type: type_, name } in prototype.params.iter() {
        if let Some(name) = name {
            env.symbol_table.insert(name.as_string(), type_.clone());
        }
    }

    type_check_traverse(env, body)?;

    env.symbol_table.pop_scope();
    env.current_prototype = None;

    Some(())
}

pub(crate) fn typecheck_destructor(
    env: &mut TypeEnvironment,
    type_name: &str,
    body: &mut CXExpr,
) -> Option<()> {
    let destructor_name = mangle_destructor(type_name);
    let prototype = env.fn_map.get(&destructor_name)
        .clone()
        .unwrap_or_else(|| panic!("Failed to find destructor prototype for {type_name} in function map"))
        .clone();
    let _type = env.type_map.get(type_name)?.clone();
    
    if !_type.is_structured() {
        log_error!("Destructor can only be defined for structured types, found: {}", type_name);
    }
    
    env.symbol_table.push_scope();
    env.symbol_table.insert(
        "this".to_string(),
        _type.pointer_to()
    );
    
    env.current_prototype = Some(prototype);
    type_check_traverse(env, body)?;
    
    env.symbol_table.pop_scope();
    env.current_prototype = None;
    
    Some(())
}