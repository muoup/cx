use cx_log::{CXResult, catalogue::mir};
use cx_mir::{MIRConstant, MIRIntType};
use cx_thir::thir::{
    data::THIRTypeKind,
    expression::{THIRExpression, THIRExpressionKind},
};

use crate::{builder::MIRBuilder, log::mir_error, lowering::types::{lower_float_type, lower_int_type, lower_type}};

fn constant_error(expression: &THIRExpression, context: &str) -> cx_log::error::CXError {
    mir_error(
        &expression.token_range,
        (&mir::EXPECTED_CONSTANT, context.to_owned()),
    )
}

pub(crate) fn evaluate_integer(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    context: &str,
) -> CXResult<usize> {
    match evaluate(builder, expression)? {
        MIRConstant::Integer { value, .. } => {
            usize::try_from(value).map_err(|_| constant_error(expression, context))
        }
        _ => Err(constant_error(expression, context)),
    }
}

pub(crate) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let ty = match &expression._type.kind {
        THIRTypeKind::Integer { _type, .. } => Some(lower_int_type(*_type)),
        _ => None,
    };
    match &expression.kind {
        THIRExpressionKind::BoolLiteral(value) => Ok(MIRConstant::Integer {
            value: *value as i128,
            ty: MIRIntType::I1,
        }),
        THIRExpressionKind::IntLiteral(value) => Ok(MIRConstant::Integer {
            value: *value as i128,
            ty: ty.unwrap_or(MIRIntType::I64),
        }),
        THIRExpressionKind::FloatLiteral(value) => {
            let THIRTypeKind::Float { _type } = expression._type.kind else {
                return Err(constant_error(expression, "constant expression"));
            };
            Ok(MIRConstant::Float {
                value: *value,
                ty: lower_float_type(_type),
            })
        }
        THIRExpressionKind::StringLiteral { value } => Ok(MIRConstant::String(value.clone())),
        THIRExpressionKind::Unit => Ok(MIRConstant::Unit),
        THIRExpressionKind::SizeOf { _type } | THIRExpressionKind::AlignOf { _type } => {
            let type_id = lower_type(builder, _type)?;
            let layout = cx_mir::ty::layout::calculate_type_layout(builder.types(), type_id);
            let value = if matches!(expression.kind, THIRExpressionKind::SizeOf { .. }) {
                layout.size()
            } else {
                layout.alignment()
            };
            Ok(MIRConstant::Integer {
                value: value as i128,
                ty: MIRIntType::I64,
            })
        }
        _ => Err(constant_error(expression, "constant expression")),
    }
}
