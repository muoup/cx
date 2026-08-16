use cx_hir::ast::modifiers::HIR_CONST;
use cx_log::CXResult;
use cx_thir::{
    thir::expression::{THIRExpression, THIRExpressionKind},
    type_context::THIRTypeContext,
};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    if !env.symbols.is_cx_str(&expr._type) {
        return CoercionResult::unapplied(expr);
    }

    let ch = env.get_intrinsic_type("char");
    let c_str = env.symbols.pointer_to(ch).with_specifier(HIR_CONST);

    let loaded = THIRExpression {
        token_range: expr.token_range.clone(),
        _type: c_str,
        kind: THIRExpressionKind::Typechange(Box::new(expr)),
    };

    CoercionResult::success(loaded)
}
