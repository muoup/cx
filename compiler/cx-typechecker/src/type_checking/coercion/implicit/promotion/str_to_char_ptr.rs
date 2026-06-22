use cx_ast::ast::modifiers::CX_CONST;
use cx_log::CXResult;
use cx_mir::{mir::expression::{MIRExpression, MIRExpressionKind}, type_context::MIRTypeContext};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CXResult<CoercionResult> {
    if !env.symbols.is_cx_str(&expr._type) {
        return CoercionResult::unapplied(expr);
    }

    let ch = env.get_intrinsic_type("char");
    let c_str = env.symbols.pointer_to(ch)
        .with_specifier(CX_CONST);
    
    let loaded = MIRExpression {
        token_range: expr.token_range.clone(),
        _type: c_str,
        kind: MIRExpressionKind::Typechange(Box::new(expr))
    };

    CoercionResult::success(loaded)
}