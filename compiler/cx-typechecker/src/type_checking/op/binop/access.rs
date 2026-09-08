use crate::environment::TypeEnvironment;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::value::{IndirectBase, resolve_indirect_base};
use cx_hir::ast::expression::{HIRExprKind, HIRExpression};
use cx_hir::ast::modifiers::HIR_CONST;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::data::THIRTypeKind;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};

fn resolve_access_base(
    env: &mut TypeEnvironment,
    _: &NamespacePath,
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
        return env.log_error(
            expr.token_range(),
            &catalogue::EXPECTED_A_STRUCT_OR_UNION_TYPE_ON_THE_LEFT_HAND,
            format!("{}", lhs.source_type.display_with(&env.symbols)),
        );
    }

    Ok(lhs)
}

pub fn typecheck_access(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
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
            &catalogue::INVALID_RIGHT_HAND_SIDE_OF_ACCESS_EXPRESSION_EXPECTED_AN_IDENTIFIER,
            (),
        );
    };

    let Some(rhs_name) = name.root_name_ref() else {
        return env.log_error(
            rhs.token_range(),
            &catalogue::INVALID_RIGHT_HAND_SIDE_OF_ACCESS_EXPRESSION_EXPECTED_AN_IDENTIFIER,
            (),
        );
    };

    let Some(struct_field) = struct_field(&env.symbols, &base.source_type, rhs_name.as_str())
    else {
        return env.log_error(
            rhs.token_range(),
            &catalogue::INVALID_RIGHT_HAND_SIDE_OF_ACCESS_EXPRESSION_EXPECTED_AN_IDENTIFIER,
            (),
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
