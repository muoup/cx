use cx_mir::{mir::expression::{MIRCoercion, MIRExpression, MIRExpressionKind}, type_context::MIRTypeContext};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

///
/// In C99:
///  - An expression of array type undergoes an implicit coercion to a pointer to the first element of the array.
///
/// In CX:
///  - Usage of raw pointers is explicitly unsafe, however there are no additional CX-specific types that would make
/// this coercion invalid as long as the inner types match and the pointer is used in defined ways.
///

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CXResult<CoercionResult> {
    let Some(mem_inner) = env.symbols.mem_ref_inner(&expr._type).cloned() else {
        return CoercionResult::unapplied(expr);
    };

    if !mem_inner.is_array() {
        return CoercionResult::unapplied(expr);
    }

    let array_inner = env.symbols.array_inner(&mem_inner).unwrap().clone();

    let new_type = env.symbols.pointer_to(array_inner);
    let coerced = MIRExpression {
        token_range: expr.token_range.clone(),

        _type: new_type,
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: MIRCoercion::ReinterpretBits,
        },
    };

    CoercionResult::success(coerced)
}
