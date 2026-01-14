use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind, CXInitIndex};
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
    types::{CXIntegerType, MIRType, MIRTypeKind},
};
use cx_util::{CXResult, identifier::CXIdent};

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        accumulation::TypecheckResult,
        binary_ops::{generate_assignment, struct_field},
        casting::implicit_cast,
        typechecker::typecheck_expr,
    },
};

pub struct TypeConstructor<'a> {
    pub union_name: CXIdent,
    pub variant_name: CXIdent,
    pub inner: &'a CXExpr,
}

pub fn deconstruct_type_constructor<'a>(
    env: &mut TypeEnvironment,
    pattern: &'a CXExpr,
) -> CXResult<TypeConstructor<'a>> {
    let CXExprKind::BinOp {
        op: CXBinOp::MethodCall,
        lhs,
        rhs: inner,
    } = &pattern.kind
    else {
        return log_typecheck_error!(
            env,
            pattern,
            "Expected type constructor"
        );
    };

    let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs,
        rhs,
    } = &lhs.kind
    else {
        return log_typecheck_error!(
            env,
            pattern,
            "Expected type constructor"
        );
    };

    let CXExprKind::Identifier(union_name) = &lhs.kind else {
        return log_typecheck_error!(
            env,
            pattern,
            "Expected type constructor"
        );
    };

    let CXExprKind::Identifier(variant_name) = &rhs.kind else {
        return log_typecheck_error!(
            env,
            pattern,
            "Expected type constructor"
        );
    };

    Ok(TypeConstructor {
        union_name: union_name.clone(),
        variant_name: variant_name.clone(),
        inner,
    })
}

pub fn typecheck_initializer_list(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    indices: &[CXInitIndex],
    to_type: Option<&MIRType>,
) -> CXResult<MIRExpression> {
    let Some(to_type) = to_type else {
        return log_typecheck_error!(env, expr, " Initializer lists must have an explicit type");
    };

    let to_type = match &to_type.kind {
        MIRTypeKind::MemoryReference(inner) => inner.as_ref(),
        _ => to_type,
    };

    match &to_type.kind {
        MIRTypeKind::Array {
            inner_type: _type,
            size,
        } => typecheck_array_initializer(env, base_data, indices, _type, Some(*size), to_type),

        MIRTypeKind::PointerTo {
            inner_type: inner,
            sizeless_array: true,
            ..
        } => typecheck_array_initializer(env, base_data, indices, inner.as_ref(), None, to_type),

        MIRTypeKind::Structured { .. } => {
            typecheck_structured_initializer(env, base_data, expr, indices, to_type)
        }

        _ => log_typecheck_error!(env, expr, " Cannot coerce initializer to type {to_type}"),
    }
}

fn typecheck_array_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    indices: &[CXInitIndex],
    inner_type: &MIRType,
    size: Option<usize>,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    for index in indices {
        if let Some(name) = &index.name {
            return log_typecheck_error!(
                env,
                &CXExpr::default(),
                "Array initializer cannot have named indices, found: {name}"
            );
        }
    }

    if let Some(size) = size
        && indices.len() > size
    {
        return log_typecheck_error!(
            env,
            &CXExpr::default(),
            "Too many elements in array initializer (expected {}, found {})",
            size,
            indices.len()
        );
    }

    let array_size = size.unwrap_or(indices.len());
    let array_type = MIRType::from(MIRTypeKind::Array {
        inner_type: Box::new(inner_type.clone()),
        size: array_size,
    });

    // Build a block of expressions: allocate + writes
    let mut statements = Vec::new();

    // Stack allocation for the array
    let allocation = TypecheckResult::stack_allocation(array_type.clone());
    statements.push(allocation.expression);

    // Initialize each element
    for (i, index) in indices.iter().enumerate() {
        let value = typecheck_expr(env, base_data, &index.value, Some(inner_type))?;

        // Get pointer to element i
        let index_expr = MIRExpression::int_literal(i as i64, CXIntegerType::I64, true);
        let element_ptr = TypecheckResult::array_access(
            allocation.clone(),
            TypecheckResult::new(index_expr),
            inner_type.clone(),
            inner_type.clone().mem_ref_to(),
        );

        // Write value to element
        let write_result = generate_assignment(
            env,
            &element_ptr.into_expression(),
            &value,
            inner_type,
        )?;
        statements.push(write_result.into_expression());
    }

    Ok(MIRExpression {
        kind: MIRExpressionKind::Block { statements },
        _type: to_type.clone(),
    })
}

fn typecheck_structured_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    indices: &[CXInitIndex],
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let MIRTypeKind::Structured { fields, .. } = &to_type.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Expected structured type for initializer, found: {to_type}"
        );
    };

    // Build a block of expressions: allocate + field writes
    let mut statements = Vec::new();

    // Stack allocation for the struct
    let allocation = TypecheckResult::stack_allocation(to_type.clone());
    statements.push(allocation.expression);

    let mut counter = 0;
    let mut initialized_fields = vec![false; fields.len()];

    for index in indices.iter() {
        if let Some(name) = &index.name {
            let Some(found_index) = fields
                .iter()
                .position(|(field_name, _)| name.as_str() == field_name.as_str())
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Structured initializer has unexpected field: {name}"
                );
            };
            counter = found_index;
        }

        if counter >= fields.len() {
            return log_typecheck_error!(env, expr, "Too many elements in struct initializer");
        }

        if initialized_fields[counter] {
            return log_typecheck_error!(
                env,
                expr,
                "Field '{}' initialized more than once",
                fields[counter].0
            );
        }

        let (field_name, field_type) = &fields[counter];
        let value = typecheck_expr(env, base_data, &index.value, Some(field_type))
            .and_then(|v| implicit_cast(env, &index.value, v, field_type))?;

        let Some(struct_field_info) = struct_field(to_type, field_name.as_str()) else {
            return log_typecheck_error!(
                env,
                expr,
                " Could not find field '{}' in type {}",
                field_name,
                to_type
            );
        };

        // Get pointer to field
        let element_ptr = TypecheckResult::struct_field_access(
            allocation.clone(),
            struct_field_info.index,
            struct_field_info.offset,
            to_type.clone(),
            field_type.clone().mem_ref_to(),
        );

        // Write value to field
        let write_result = generate_assignment(
            env,
            &element_ptr.into_expression(),
            &value,
            field_type,
        )?;
        statements.push(write_result.into_expression());

        initialized_fields[counter] = true;

        if index.name.is_none() {
            counter += 1;
        }
    }

    Ok(MIRExpression {
        kind: MIRExpressionKind::Block { statements },
        _type: to_type.clone(),
    })
}
