use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};
use cx_thir::thir::r#type::{THIRArrayLength, THIRType, THIRTypeKind};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;

use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::conversion::is_char_array;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::typechecker::typecheck_expr;

pub(crate) struct ObjectInitializer {
    pub object_type: THIRType,
    pub value: THIRExpression,
    pub adopting: bool,
}

pub(crate) fn typecheck_object_initializer(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    initializer: &HIRExpression,
    declared_type: &THIRType,
) -> CXResult<ObjectInitializer> {
    let checked = typecheck_expr(env, namespace, initializer, Some(declared_type))?;
    let adopting = checked.is_adopting();
    let expression = checked
        .apply_expected_type(env, namespace, declared_type)?
        .standard_ready_coerce(env, initializer.token_range())?;

    let expression = if declared_type.is_array()
        && env
            .symbols
            .mem_ref_inner(&expression.ty)
            .is_some_and(THIRType::is_array)
    {
        lvalue::try_conversion(env, expression, true)?
            .catch_unapplied(|expression, _| Ok(expression))?
    } else {
        expression
    };

    let mut object_type = declared_type.clone();
    if let THIRTypeKind::Array {
        length: THIRArrayLength::Implicit,
        ..
    } = &declared_type.kind
    {
        let length = match &expression.ty.kind {
            THIRTypeKind::Array {
                length: THIRArrayLength::Known(length),
                ..
            } => THIRArrayLength::Known(length.clone()),
            _ if is_char_array(env, declared_type) => match &expression.kind {
                THIRExpressionKind::StringLiteral { value } => {
                    THIRArrayLength::Known(Box::new(THIRExpression {
                        token_range: TokenRange::internal(),
                        ty: env.get_intrinsic_type("int"),
                        kind: THIRExpressionKind::IntLiteral((value.len() + 1) as i64),
                    }))
                }
                _ => {
                    return env.log_error(
                        initializer.token_range(),
                        &catalogue::INCOMPLETE_TYPE,
                        format!("{}", declared_type.display_with(&env.symbols)),
                    );
                }
            },
            _ => {
                return env.log_error(
                    initializer.token_range(),
                    &catalogue::INCOMPLETE_TYPE,
                    format!("{}", declared_type.display_with(&env.symbols)),
                );
            }
        };
        if let THIRTypeKind::Array {
            length: object_length,
            ..
        } = &mut object_type.kind
        {
            *object_length = length;
        }
    }

    let expression = implicit_cast(env, expression, &object_type)?;
    Ok(ObjectInitializer {
        object_type,
        value: expression,
        adopting,
    })
}
