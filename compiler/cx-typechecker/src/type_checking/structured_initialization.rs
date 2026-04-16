use cx_ast::{
    ast::{CXBinOp, CXExpr, CXExprKind, CXInitIndex},
    data::{CXTypeKind, PredeclarationType},
};
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpressionKind, StructInitialization},
    program::MIRBaseMappings,
};
use cx_tokens::TokenRange;
use cx_util::{CXResult, identifier::CXIdent};

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        accumulation::TypecheckResult, binary_ops::struct_field, casting::implicit_cast,
        typechecker::typecheck_expr,
    },
};

pub struct TypeConstructor<'a> {
    pub union_type: MIRType,
    pub variant_name: CXIdent,
    pub inner: Option<&'a CXExpr>,
}

pub fn deconstruct_type_constructor<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    pattern: &'a CXExpr,
) -> CXResult<TypeConstructor<'a>> {
    let (constructor, inner) = match &pattern.kind {
        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs: inner,
        } => (lhs.as_ref(), Some(inner.as_ref())),

        // T::Variant with no inner value can be written without parentheses, so we also allow that form for patterns with no inner value
        _ => (pattern, None),
    };

    let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs: union,
        rhs: variant,
    } = &constructor.kind
    else {
        return log_typecheck_error!(env, pattern.token_range(), "Expected type constructor");
    };

    let union_name = match &union.kind {
        CXExprKind::Identifier(union_name) => {
            let as_type = CXTypeKind::Identifier {
                predeclaration: PredeclarationType::None,
                name: union_name.clone(),
            }
            .to_type();

            env.complete_type(base_data, union, &as_type)?
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let as_type = CXTypeKind::TemplatedIdentifier {
                name: name.clone(),
                input: template_input.clone(),
            }
            .to_type();

            env.complete_type(base_data, union, &as_type)?
        }

        _ => {
            return log_typecheck_error!(
                env,
                union.token_range(),
                "Expected union name in type constructor pattern"
            );
        }
    };

    let CXExprKind::Identifier(variant_name) = &variant.kind else {
        return log_typecheck_error!(
            env,
            variant.token_range(),
            "Expected variant name in type constructor pattern"
        );
    };

    Ok(TypeConstructor {
        union_type: union_name,
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
) -> CXResult<TypecheckResult> {
    let Some(to_type) = to_type else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Initializer lists must have an explicit type"
        );
    };

    let owned_inner;
    let to_type = if let Some(inner_type) = env.generated_types.mem_ref_inner(to_type) {
        owned_inner = inner_type.clone();
        &owned_inner
    } else {
        to_type
    };

    match &to_type.kind {
        MIRTypeKind::Array {
            inner_type: _type,
            size,
        } => {
            let inner_type = env
                .generated_types
                .get(*_type.as_ref())
                .unwrap_or_else(|| panic!("Unknown type id {}", _type.0))
                .clone();
            typecheck_array_initializer(env, base_data, indices, &inner_type, Some(*size), to_type)
        }

        MIRTypeKind::PointerTo {
            inner_type: inner, ..
        } => {
            let inner_type = env
                .generated_types
                .get(*inner.as_ref())
                .unwrap_or_else(|| panic!("Unknown type id {}", inner.0))
                .clone();
            typecheck_array_initializer(env, base_data, indices, &inner_type, None, to_type)
        }

        MIRTypeKind::Structured { .. } => {
            typecheck_structured_initializer(env, base_data, expr, indices, to_type)
        }

        _ => log_typecheck_error!(
            env,
            expr.token_range(),
            "Cannot coerce initializer to type {to_type}"
        ),
    }
}

fn typecheck_array_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    indices: &[CXInitIndex],
    inner_type: &MIRType,
    size: Option<usize>,
    _to_type: &MIRType,
) -> CXResult<TypecheckResult> {
    for index in indices {
        if let Some(name) = &index.name {
            return log_typecheck_error!(
                env,
                &TokenRange::default(),
                "Array initializer cannot have named indices, found: {name}"
            );
        }
    }

    if let Some(size) = size
        && indices.len() > size
    {
        return log_typecheck_error!(
            env,
            &TokenRange::default(),
            "Too many elements in array initializer (expected {}, found {})",
            size,
            indices.len()
        );
    }

    let array_size = size.unwrap_or(indices.len());
    let inner_type_id = env.intern_type(inner_type.clone());
    let array_type = MIRType::from(MIRTypeKind::Array {
        inner_type: Box::new(inner_type_id),
        size: array_size,
    });

    let elements = indices
        .iter()
        .map(|index| {
            typecheck_expr(env, base_data, &index.value, Some(inner_type))
                .map(|v| v.into_expression())
        })
        .collect::<CXResult<_>>()?;

    Ok(TypecheckResult::expr(
        array_type,
        MIRExpressionKind::ArrayInitializer {
            elements,
            element_type: inner_type.clone(),
        },
    ))
}

fn typecheck_structured_initializer(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    indices: &[CXInitIndex],
    to_type: &MIRType,
) -> CXResult<TypecheckResult> {
    let Some(fields) = to_type.aggregate_fields(&env.generated_types) else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Expected a structured type for initializer, found {to_type}"
        );
    };
    let fields = fields.clone();

    let mut initializations = Vec::new();

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
                    expr.token_range(),
                    "Structured initializer has unexpected field: {name}"
                );
            };
            counter = found_index;
        }

        if counter >= fields.len() {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Too many elements in struct initializer"
            );
        }

        if initialized_fields[counter] {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Field '{}' initialized more than once",
                fields[counter].0
            );
        }

        let (field_name, field_type) = &fields[counter];
        let value = typecheck_expr(env, base_data, &index.value, Some(field_type))
            .and_then(|v| implicit_cast(env, &index.value, v.into_expression(), field_type))?;

        let Some(struct_field_info) =
            struct_field(to_type, &env.generated_types, field_name.as_str())
        else {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Could not find field '{}' in type {}",
                field_name,
                to_type
            );
        };

        initializations.push(StructInitialization {
            field_index: struct_field_info.index,
            field_offset: struct_field_info.offset,
            value,
        });
        initialized_fields[counter] = true;

        if index.name.is_none() {
            counter += 1;
        }
    }

    Ok(TypecheckResult::expr(
        to_type.clone(),
        MIRExpressionKind::StructInitializer {
            struct_type: to_type.clone(),
            initializations,
        },
    ))
}
