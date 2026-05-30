use cx_ast::ast::modifiers::CX_CONST;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{CoercionObstacle, CoercionResult},
};

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
/// a compilation error here. The correct way to handle a 'copy' in this scenario is by moving, however a moved value
/// loses its memory reference wrapper, meaning this conversion will be skipped.
///

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CXResult<CoercionResult> {
    let Some(mem_inner) = env.symbols.context.mem_ref_inner(&expr._type).cloned() else {
        return CoercionResult::unapplied(expr);
    };

    if mem_inner.is_array() || mem_inner.is_str() {
        return CoercionResult::unapplied(expr);
    }

    if !env.symbols.is_copyable(&mem_inner) {
        return CoercionResult::unapplied_with_obstacle(expr, CoercionObstacle::Uncopyable);
    }

    let token_range = expr.token_range.clone();
    let result_type = mem_inner.without_specifier(CX_CONST);
    let loaded = MIRExpression {
        token_range,
        _type: result_type.clone(),
        kind: MIRExpressionKind::RegionDuplicate {
            source: Box::new(expr),
        },
    };

    CoercionResult::success(loaded)
}
