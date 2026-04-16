use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

///
/// NOTE: We can safely assume that an lvalue type = memory reference to type. All values in MIR are abstracted
/// to correspond to a virtual 'region', what differentiates a MemoryReference<T> from a T is whether its region
/// can be validly addressed.
///
/// In C99:
///
/// - Type remains the same, but the value loses const/volatile/restricted qualifiers and atomic (TODO) properties
/// - The value remains the same, but loses its lvalue properties (the address may no longer be taken).
///
/// Additions with CX:
///
/// - Lvalue -> Rvalue coercion represents an implicit 'copy', so any structured types marked @nocopy should throw
/// an explicit error on compilation unless that value is moved.
///

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CoercionResult {
    let Some(mem_inner) = env.type_context.mem_ref_inner(&expr._type).cloned() else {
        return CoercionResult::none(expr);
    };
    
    if mem_inner.is_array() {
        return CoercionResult::none(expr);
    }

    if !env.is_copyable(&expr._type) {
        return CoercionResult::error(
            format!("Cannot implicitly copy value of type {}", &expr._type),
            expr,
        );
    }

    let loaded = MIRExpression {
        token_range: expr.token_range.clone(),

        _type: mem_inner.clone(),
        kind: MIRExpressionKind::MemoryRead {
            source: Box::new(expr),
        },
    };

    CoercionResult::some(loaded, mem_inner)
}
