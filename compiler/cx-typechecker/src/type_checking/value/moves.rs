use cx_log::catalogue::typecheck as catalogue;
use std::collections::{HashMap, HashSet};

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        result::{BindingPlaceKind, TypecheckResult},
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::{
    expression::{HIRExpression, HIRUnpackBinding},
    modifiers::HIR_CONST,
};
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::{
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{
            THIRCoercion, THIRExpression, THIRExpressionKind, THIRLocalID, THIRUnpackBinding,
        },
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

pub(crate) fn typecheck_move(
    env: &mut TypeEnvironment,
    _: &NamespacePath,
    inner: TypecheckResult,
    inner_expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let binding = inner.binding().cloned();
    let inner_val = inner.standard_ready_coerce(env, inner_expr.token_range())?;

    if !inner_val.ty.is_memory_reference() {
        return Ok(TypecheckResult::from(inner_val));
    }

    let Some(binding) = binding else {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::LOCAL_VARIABLE_REQUIRED,
            "move".into()
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::LOCAL_VARIABLE_REQUIRED,
            "move".into()
        );
    };

    if !matches!(inner_val.kind, THIRExpressionKind::Variable { .. }) {
        return env.log_error(
            inner_expr.token_range(),
            &catalogue::LOCAL_VARIABLE_REQUIRED,
            "move".into()
        );
    }

    let Some(inner_type) = env.symbols.mem_ref_inner(&inner_val.ty).cloned() else {
        unreachable!()
    };

    Ok(TypecheckResult::new(
        inner_type,
        THIRExpressionKind::Move {
            name: binding.root.clone(),
            local_id: binding.local_id,
        },
    )
    .with_binding(binding))
}

pub(crate) fn typecheck_adopt(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)?;
    let binding = value.binding().cloned();
    let value = value.standard_ready_coerce(env, inner.token_range())?;
    let Some(inner_type) = env.symbols.mem_ref_inner(&value.ty).cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::TYPE_MISMATCH,
            (
                "@adopt".into(),
                "memory reference type".into(),
                format!("{}", value.ty.display_with(&env.symbols)),
            ),
        );
    };

    if value.ty.get_specifier(HIR_CONST) || inner_type.get_specifier(HIR_CONST) {
        return env.log_error(
            expr.token_range(),
            &catalogue::TYPE_MISMATCH,
            (
                "@adopt".into(),
                "non-const type".into(),
                format!("{}", value.ty.display_with(&env.symbols)),
            ),
        );
    }

    if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Local
    {
        return env.log_error(
            expr.token_range(),
            &catalogue::INVALID_CONTEXT,
            ("@adopt".into(), "local variable; use a move instead".into())
        );
    }

    Ok(TypecheckResult::new(
        inner_type,
        THIRExpressionKind::TypeConversion {
            operand: Box::new(value),
            conversion: THIRCoercion::Adopt,
        },
    )
    .with_adopting())
}

pub(crate) fn typecheck_leak(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)?;

    let Some(binding) = value.binding().cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::LOCAL_VARIABLE_REQUIRED,
            "@leak".into()
        );
    };

    if binding.kind != BindingPlaceKind::Local {
        return env.log_error(
            expr.token_range(),
            &catalogue::LOCAL_VARIABLE_REQUIRED,
            "@leak".into()
        );
    };

    let value = value.standard_ready_coerce(env, inner.token_range())?;

    let Some(inner_type) = env.symbols.mem_ref_inner(&value.ty).cloned() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::TYPE_MISMATCH,
            (
                "@leak".into(),
                "memory reference type".into(),
                format!("{}", value.ty.display_with(&env.symbols)),
            ),
        );
    };

    let leak_type = if inner_type.is_nodrop() {
        THIRType::unit()
    } else {
        value.ty.clone()
    };

    Ok(TypecheckResult::new(
        leak_type,
        THIRExpressionKind::Leak {
            expression: Box::new(value),
        },
    ))
}

pub(crate) fn typecheck_unpack(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    inner: &HIRExpression,
    bindings: &[HIRUnpackBinding],
) -> CXResult<TypecheckResult> {
    let value = typecheck_expr(env, namespace, inner, None)?
        .standard_ready_assure(env, expr.token_range())?;

    let thir_expr = value.standard_ready_coerce(env, inner.token_range())?;
    let THIRTypeKind::Structured { fields } = &thir_expr.ty.kind else {
        return env.log_error(
            expr.token_range(),
            &catalogue::TYPE_MISMATCH,
            (
                "@unpack".into(),
                "owned structured type".into(),
                format!("{}", thir_expr.ty.display_with(&env.symbols)),
            ),
        );
    };

    let field_map = fields
        .iter()
        .enumerate()
        .filter_map(|(index, field)| Some((field.name()?.to_string(), (index, field.ty()))))
        .collect::<HashMap<_, _>>();

    let mut seen_fields = HashSet::new();
    let mut seen_bindings = HashSet::new();

    for unpack_binding in bindings {
        if !field_map.contains_key(unpack_binding.field.as_str()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNKNOWN_MEMBER,
                (
                    format!("{}", thir_expr.ty.display_with(&env.symbols)),
                    format!("{}", unpack_binding.field),
                ),
            );
        }

        if !seen_fields.insert(unpack_binding.field.as_string()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::DUPLICATE_ITEM,
                (format!("{}", unpack_binding.field), "@unpack binding".into()),
            );
        }

        if !seen_bindings.insert(unpack_binding.binding.as_string()) {
            return env.log_error(
                expr.token_range(),
                &catalogue::INCOMPATIBLE_DECLARATION,
                ("@unpack binding".into(), format!("{}", unpack_binding.binding)),
            );
        }
    }

    for (field_name, (_, field_ty_id)) in field_map.iter() {
        let _ty = env.symbols.resolve_type_id(*field_ty_id);
        if _ty.is_nodrop() && !seen_fields.contains(field_name) {
            return env.log_error(
                expr.token_range(),
                &catalogue::MISSING_ENTITY,
                (
                    format!("drop of @nodrop field {}", field_name),
                    "@unpack statement".into()
                ),
            );
        }
    }

    let mut thir_bindings = Vec::new();

    for unpack_binding in bindings {
        let local_id = THIRLocalID::fresh();

        let Some(field_pos) = fields.iter().position(|f| {
            f.name()
                .map(|n| n == unpack_binding.field.as_str())
                .unwrap_or(false)
        }) else {
            return env.log_error(
                expr.token_range(),
                &catalogue::UNKNOWN_MEMBER,
                (
                    format!("{}", unpack_binding.field),
                    format!("{}", thir_expr.ty.display_with(&env.symbols)),
                ),
            );
        };

        let field_type = env.symbols.resolve_type_id(fields[field_pos].ty()).clone();
        let symbol_type = env.symbols.mem_ref_to(field_type.clone());

        env.symbols.insert_local_value(
            QualifiedName::new_raw(unpack_binding.binding.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                ty: symbol_type,
                kind: THIRExpressionKind::Variable {
                    name: unpack_binding.binding.clone(),
                    local_id,
                },
            },
        );

        thir_bindings.push(THIRUnpackBinding {
            field_name: unpack_binding.field.clone(),
            field_index: field_pos,
            field_type,

            binding_name: unpack_binding.binding.clone(),
            binding_local_id: local_id,
        })
    }

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Unpack {
            value: Box::new(thir_expr),
            bindings: thir_bindings,
        },
    ))
}
