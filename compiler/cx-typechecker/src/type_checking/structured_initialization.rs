use cx_typechecker_data::{
    ast::TCExpr,
    mir::types::{CXType, CXTypeKind},
};
use cx_util::{CXResult, log_error};

pub fn coerce_initializer_list(initializer: &mut TCExpr, to_type: &CXType) -> CXResult<()> {
    let to_type = match &to_type.kind {
        CXTypeKind::MemoryReference(inner) => inner.as_ref(),
        _ => to_type,
    };

    match &to_type.kind {
        CXTypeKind::Array {
            inner_type: _type,
            size,
        } => organize_array_initializer(initializer, _type, Some(*size)),

        CXTypeKind::PointerTo {
            inner_type: inner,
            sizeless_array: true,
            ..
        } => organize_array_initializer(initializer, inner.as_ref(), None),

        CXTypeKind::Structured { .. } => organize_structured_initializer(initializer, to_type),

        _ => log_error!(" Cannot coerce initializer to type {to_type}"),
    }
}

fn organize_array_initializer(
    _initializer: &mut TCExpr,
    _inner_type: &CXType,
    _size: Option<usize>,
) -> CXResult<()> {
    todo!()
    // let TCExprKind::InitializerList { indices } = &mut initializer.kind else {
    //     unreachable!(
    //         "PANIC: organize_array_initializer expected initialzer, found: {initializer:?}"
    //     );
    // };

    // let mut counter = 0;

    // for (i, index) in indices.iter_mut().enumerate() {
    //     if let Some(name) = index.name.as_ref() {
    //         log_error!(" Array initializer cannot have named indices, found: {name}");
    //     }

    //     index.index = counter;
    //     implicit_cast(&mut index.value, inner_type)?;

    //     if let Some(size) = size
    //         && i < size {
    //             counter += 1;
    //         }
    // }

    // let array_size = match size {
    //     Some(size) => size,
    //     None => indices.len(),
    // };

    // let init_list_type = CXTypeKind::Array {
    //     inner_type: Box::new(inner_type.clone()),
    //     size: array_size,
    // }
    // .into();

    // initializer._type = init_list_type;
    // Ok(())
}

fn organize_structured_initializer(_initializer: &mut TCExpr, _to_type: &CXType) -> CXResult<()> {
    todo!()
    // let TCExprKind::InitializerList { indices } = &mut initializer.kind else {
    //     unreachable!(
    //         "PANIC: organize_structured_initializer expected initialzer, found: {initializer:?}"
    //     );
    // };
    // let CXTypeKind::Structured { fields, .. } = &to_type.kind else {
    //     log_error!(" Expected structured type for initializer, found: {to_type}");
    // };

    // let mut counter = 0;

    // for index in indices.iter_mut() {
    //     if let Some(name) = &index.name {
    //         let Some(index) = fields.iter().position(|(field_name, _)| name == field_name) else {
    //             log_error!(" Structured initializer has unexpected field: {name}");
    //         };

    //         counter = index;
    //     }

    //     index.index = counter;

    //     let field_type = &fields[counter].1;
    //     implicit_cast(&mut index.value, field_type)?;

    //     if counter < fields.len() {
    //         counter += 1;
    //     }
    // }

    // initializer._type = to_type.clone();
    // Ok(())
}
