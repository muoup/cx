use cx_ast::data::CX_CONST;
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
/// a compilation error here. The correct way to handle a 'copy' in this scenario is by moving, however a moved value
/// loses its memory reference wrapper, meaning this conversion will be skipped.
///

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CoercionResult {
    let Some(mem_inner) = env.type_context.mem_ref_inner(&expr._type).cloned() else {
        return CoercionResult::none(expr);
    };

    if mem_inner.is_array() || mem_inner.is_str() {
        return CoercionResult::none(expr);
    }

    if !env.is_copyable(&mem_inner) {
        return CoercionResult::error(
            format!("Cannot implicitly copy value of type {}", mem_inner),
            expr,
        );
    }

    let token_range = expr.token_range.clone();
    let result_type = mem_inner.without_specifier(CX_CONST);
    let loaded = MIRExpression {
        token_range,
        _type: result_type.clone(),
        kind: if mem_inner.is_memory_resident() {
            MIRExpressionKind::RegionDuplicate {
                source: Box::new(expr),
                _type: result_type
            }
        } else {
            MIRExpressionKind::MemoryRead {
                source: Box::new(expr),
            }
        },
    };

    CoercionResult::some(loaded)
}
