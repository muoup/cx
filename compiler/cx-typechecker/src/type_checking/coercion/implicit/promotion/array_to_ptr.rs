use cx_log::CXResult;
use cx_thir::{
    thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
    type_context::THIRTypeContext,
};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

///
/// In C99:
///  - An expression of array type undergoes an implicit coercion to a pointer to the first element of the array.
///
/// In CX:
///  - Usage of raw pointers is explicitly unsafe, however there are no additional CX-specific types that would make
/// this coercion invalid as long as the inner types match and the pointer is used in defined ways.
///

pub fn try_conversion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    let Some(mem_inner) = env.symbols.mem_ref_inner(&expr._type).cloned() else {
        return CoercionResult::unapplied(expr);
    };

    if !mem_inner.is_array() {
        return CoercionResult::unapplied(expr);
    }

    let array_inner = env.symbols.array_inner(&mem_inner).unwrap().clone();
    let new_type = env.symbols.pointer_to(array_inner);
 
    let coerced = THIRExpression {
        _type: new_type,
        token_range: expr.token_range.clone(),
        kind: THIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: THIRCoercion::ReinterpretBits,
        },
    };

    CoercionResult::success(coerced)
}
