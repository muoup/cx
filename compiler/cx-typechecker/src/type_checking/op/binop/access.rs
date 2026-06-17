use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::value::locals::ensure_binding_available;
use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_ast::ast::modifiers::CX_CONST;
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::{MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::type_context::MIRTypeContext;

struct AccessBase {
    source: MIRExpression,
    source_type: MIRType,
}

fn resolve_access_base(
    env: &mut TypeEnvironment,
    _: &EnvironmentNamespace,
    expr: &CXExpression,
    lhs: MIRExpression,
) -> CXResult<AccessBase> {
    // Here, our aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let mut lhs = lhs;
    let lhs_inner = loop {
        let lhs_type = lhs._type.clone();

        if let Some(inner_type) = env.symbols.mem_ref_inner(&lhs_type).cloned() {
            if let Some(ptr_inner) = env.symbols.ptr_inner(&inner_type).cloned() {
                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: env.symbols.pointer_to(ptr_inner.clone()),
                };

                break ptr_inner;
            }

            if env.symbols.mem_ref_inner(&inner_type).is_some() {
                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: inner_type.clone(),
                };
            } else {
                break inner_type;
            }
        } else if let Some(inner_type) = env.symbols.ptr_inner(&lhs_type).cloned() {
            break inner_type;
        } else {
            break lhs_type;
        }
    };

    if !matches!(
        lhs_inner.kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
    ) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Expected a struct or union type on the left-hand side of an access expression, found {}",
            lhs_inner.display_with(&env.symbols)
        );
    }

    Ok(AccessBase {
        source: lhs,
        source_type: lhs_inner,
    })
}

pub fn typecheck_access(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: TypecheckResult,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    ensure_binding_available(env, expr.token_range(), lhs.binding())?;
    let lhs_binding = lhs.binding().cloned();

    let base = resolve_access_base(
        env,
        namespace,
        expr,
        lhs.standard_ready_coerce(env, expr.token_range())?,
    )?;

    let CXExprKind::Identifier {
        name,
        template_input: None,
    } = &rhs.kind
    else {
        return log_typecheck_error!(
            env,
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier"
        );
    };

    let Some(rhs_name) = name.root_name_ref() else {
        return log_typecheck_error!(
            env,
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier"
        );
    };

    let Some(struct_field) = struct_field(&env.symbols, &base.source_type, rhs_name.as_str())
    else {
        return log_typecheck_error!(
            env,
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier"
        );
    };

    let mut result = TypecheckResult::new(
        env.symbols
            .mem_ref_to(struct_field.field_type.clone().with_specifier(
                if base.source_type.get_specifier(CX_CONST) {
                    CX_CONST
                } else {
                    0
                },
            )),
        MIRExpressionKind::MemberAccess {
            base: Box::new(base.source),
            member_index: struct_field.index,
            aggregate_type: base.source_type.clone(),
        },
    );

    if let Some(binding) = lhs_binding.as_ref().map(|binding| binding.project()) {
        result = result.with_binding(binding);
    }

    return Ok(result);
}
