use cx_data_ast::parse::ast::{CXExpr, CXExprKind};
use cx_data_typechecker::cx_types::{CXType, CXTypeKind};
use cx_util::log_error;
use crate::checker::implicit_coerce;
use crate::TypeEnvironment;

pub fn coerce_initializer_list(
    env: &mut TypeEnvironment,
    initializer: &mut CXExpr,
    to_type: &CXType
) -> Option<()> {
    match &to_type.kind {
        CXTypeKind::Array { inner_type: _type, size } =>
            organize_array_initializer(env, initializer, _type, Some(*size)),
        
        CXTypeKind::PointerTo { inner_type: inner, sizeless_array: true, .. } =>
            organize_array_initializer(env, initializer, inner.as_ref(), None),
        
        CXTypeKind::Structured { .. } =>
            organize_structured_initializer(env, initializer, to_type),
        
        _ => log_error!("TYPE ERROR: Cannot coerce initializer to type {to_type}"),
    }
}

fn organize_array_initializer(
    env: &mut TypeEnvironment,
    initializer: &mut CXExpr,
    inner_type: &CXType,
    size: Option<usize>
) -> Option<()> {
    let CXExprKind::InitializerList { indices } = &mut initializer.kind else {
        unreachable!("PANIC: organize_array_initializer expected initialzer, found: {initializer}");
    };
    
    let mut counter = 0;
    
    for (i, index) in indices.iter_mut().enumerate() {
        if let Some(name) = index.name.as_ref() {
            log_error!("TYPE ERROR: Array initializer cannot have named indices, found: {name}");
        }
        
        index.index = counter;
        implicit_coerce(env, &mut index.value, inner_type.clone())?;
        
        if let Some(size) = size {
            if i < size {
                counter += 1;
            }
        }
    }
    
    let array_size = match size {
        Some(size) => size,
        None => indices.len(),
    };
    
    let init_list_type = CXTypeKind::Array {
        inner_type: Box::new(inner_type.clone()),
        size: array_size,
    }.into();
    
    env.typecheck_data.insert(initializer, init_list_type);
    
    Some(())
}

fn organize_structured_initializer(
    env: &mut TypeEnvironment,
    initializer: &mut CXExpr,
    to_type: &CXType,
) -> Option<()> {
    let CXExprKind::InitializerList { indices } = &mut initializer.kind else {
        unreachable!("PANIC: organize_structured_initializer expected initialzer, found: {initializer}");
    };
    let CXTypeKind::Structured { fields, .. } = &to_type.kind else {
        log_error!("TYPE ERROR: Expected structured type for initializer, found: {to_type}");
    };
    
    let mut counter = 0;
    
    for index in indices.iter_mut() {
        if let Some(name) = &index.name {
            let Some(index) = fields.iter().position(|(field_name, _)| name == field_name) else {
                log_error!("TYPE ERROR: Structured initializer has unexpected field: {name}");
            };
            
            counter = index;
        }
        
        index.index = counter;
        
        let field_type = &fields[counter].1;
        implicit_coerce(env, &mut index.value, field_type.clone())?;
        
        if counter < fields.len() {
            counter += 1;
        }
    }
    
    env.typecheck_data.insert(initializer, to_type.clone());
    
    Some(())
}