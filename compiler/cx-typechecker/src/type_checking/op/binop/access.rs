use crate::environment::TypeEnvironment;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::value::{IndirectBase, resolve_indirect_base};
use cx_hir::ast::expression::{HIRExprKind, HIRExpression};
use cx_hir::ast::modifiers::HIR_CONST;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::THIRTypeKind;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};

fn resolve_access_base(
    env: &mut TypeEnvironment,
    _: &EnvironmentNamespace,
    expr: &HIRExpression,
    lhs: THIRExpression,
) -> CXResult<IndirectBase> {
    let lhs = resolve_indirect_base(env, lhs);

    if !matches!(
        lhs.source_type.kind,
        THIRTypeKind::Structured { .. }
            | THIRTypeKind::Union { .. }
            | THIRTypeKind::TaggedUnion { .. }
    ) {
        return env.log_error(expr.token_range(), format!("Expected a struct or union type on the left-hand side of an access expression, found {}", lhs.source_type.display_with(&env.symbols)));
    }

    Ok(lhs)
}

pub fn typecheck_access(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: TypecheckResult,
    rhs: &HIRExpression,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let lhs_binding = lhs.binding().cloned();

    let base = resolve_access_base(
        env,
        namespace,
        expr,
        lhs.standard_ready_coerce(env, expr.token_range())?,
    )?;

    let HIRExprKind::Identifier {
        name,
        template_input: None,
        ..
    } = &rhs.kind
    else {
        return env.log_error(
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier".to_string(),
        );
    };

    let Some(rhs_name) = name.root_name_ref() else {
        return env.log_error(
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier".to_string(),
        );
    };

    let Some(struct_field) = struct_field(&env.symbols, &base.source_type, rhs_name.as_str())
    else {
        return env.log_error(
            rhs.token_range(),
            "Invalid right-hand side of access expression: expected an identifier".to_string(),
        );
    };

    let mut result = TypecheckResult::new(
        env.symbols
            .mem_ref_to(struct_field.field_type.clone().with_specifier(
                if base.source_type.get_specifier(HIR_CONST) {
                    HIR_CONST
                } else {
                    0
                },
            )),
        THIRExpressionKind::MemberAccess {
            base: Box::new(base.source),
            member_index: struct_field.index,
            aggregate_type: base.source_type.clone(),
        },
    );

    if let Some(binding) = lhs_binding.as_ref().map(|binding| binding.project()) {
        result = result.with_binding(binding);
    }

    Ok(result)
}
