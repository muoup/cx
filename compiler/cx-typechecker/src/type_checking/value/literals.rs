use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};
use cx_log::CXResult;
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind},
};
use cx_tokens::{
    TokenRange,
    token::{FloatSuffix, IntegerBase, IntegerLength, IntegerSuffix},
};
use cx_util::unsafe_float::FloatWrapper;

pub(crate) fn typecheck_int_literal(
    env: &TypeEnvironment,
    token_range: &TokenRange,
    magnitude: u64,
    base: IntegerBase,
    suffix: IntegerSuffix,
) -> CXResult<TypecheckResult> {
    let decimal = base == IntegerBase::Decimal;
    let candidates: &[&str] = match (suffix.unsigned, suffix.length, decimal) {
        (false, IntegerLength::Default, true) => &["int", "long", "long long"],
        (false, IntegerLength::Default, false) => &[
            "int",
            "unsigned int",
            "long",
            "unsigned long",
            "long long",
            "unsigned long long",
        ],
        (true, IntegerLength::Default, _) => {
            &["unsigned int", "unsigned long", "unsigned long long"]
        }
        (false, IntegerLength::Long, true) => &["long", "long long"],
        (false, IntegerLength::Long, false) => {
            &["long", "unsigned long", "long long", "unsigned long long"]
        }
        (true, IntegerLength::Long, _) => &["unsigned long", "unsigned long long"],
        (false, IntegerLength::LongLong, true) => &["long long"],
        (false, IntegerLength::LongLong, false) => &["long long", "unsigned long long"],
        (true, IntegerLength::LongLong, _) => &["unsigned long long"],
    };

    let literal_type = candidates
        .iter()
        .map(|name| env.get_intrinsic_type(name))
        .find(|candidate| integer_type_can_represent(candidate, magnitude));
    let Some(literal_type) = literal_type else {
        return env.log_error(
            token_range,
            format!("Integer literal {magnitude} does not fit any permitted type"),
        );
    };

    Ok(TypecheckResult::from(THIRExpression {
        token_range: token_range.clone(),
        // MIR stores integer literal bits in an i64; signedness lives in the MIR type.
        kind: THIRExpressionKind::IntLiteral(magnitude as i64),
        _type: literal_type,
    }))
}

fn integer_type_can_represent(candidate: &THIRType, magnitude: u64) -> bool {
    let THIRTypeKind::Integer { _type, signed } = candidate.kind else {
        unreachable!("integer literal candidate was not an integer type")
    };
    let bits = (_type.bytes() * 8) as u32;
    if signed {
        bits > 64 || magnitude <= ((1_u64 << (bits - 1)) - 1)
    } else {
        bits >= 64 || magnitude <= ((1_u64 << bits) - 1)
    }
}

pub(crate) fn typecheck_float_literal(
    env: &TypeEnvironment,
    token_range: &TokenRange,
    val: FloatWrapper,
    suffix: FloatSuffix,
) -> CXResult<TypecheckResult> {
    if suffix == FloatSuffix::LongDouble {
        return env.log_error(
            token_range,
            "Long double literals are not supported by the current MIR".to_string(),
        );
    }

    Ok(TypecheckResult::from(THIRExpression {
        token_range: token_range.clone(),
        kind: THIRExpressionKind::FloatLiteral(val),
        _type: env.get_intrinsic_type(if suffix == FloatSuffix::Float {
            "float"
        } else {
            "double"
        }),
    }))
}

pub(crate) fn typecheck_unit() -> TypecheckResult {
    TypecheckResult::from(THIRExpression {
        token_range: TokenRange::internal(),
        kind: THIRExpressionKind::Unit,
        _type: THIRType::unit(),
    })
}
