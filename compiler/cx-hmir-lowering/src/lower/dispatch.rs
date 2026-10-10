use cx_hmir::{
    HMIRBody, HMIRControlOp, HMIROp,
    expr::{HMIRExpr, HMIRExprKind},
};
use cx_log::CXResult;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        HMIRValue,
        binding::{lower_block, lower_let, lower_local},
        coerce::lower_coerce,
        constant::lower_constant,
        control::{lower_if, lower_return, lower_while},
        op::{lower_binary, lower_unary},
    },
};

pub fn lower_expression(
    env: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: &HMIRExpr,
) -> CXResult<HMIRValue> {
    let span = expr.span();

    match expr.kind() {
        HMIRExprKind::Constant(constant) => lower_constant(env, body, span, constant),
        HMIRExprKind::Local(local) => lower_local(env, *local),
        HMIRExprKind::Let { local, initializer } => lower_let(env, body, span, *local, *initializer),
        HMIRExprKind::Block { statements } => lower_block(env, body, span, statements),

        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(env, body, span, *condition, *then_branch, *else_branch),
        HMIRExprKind::While {
            condition,
            body: loop_body,
            pre_eval,
        } => lower_while(env, body, span, *condition, *loop_body, *pre_eval),

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => lower_binary(env, body, span, *op, *lhs, *rhs),
            HMIROp::UnOp { op, operand } => lower_unary(env, body, span, *op, *operand),
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => lower_coerce(env, body, span, *mode, *value, *target),
            HMIROp::Control(HMIRControlOp::Return(value)) => lower_return(env, body, span, *value),

            HMIROp::Assign { .. } => todo!("HMIROp::Assign"),
            HMIROp::AddressOf(_) => todo!("HMIROp::AddressOf"),
            HMIROp::Dereference(_) => todo!("HMIROp::Dereference"),
            HMIROp::Type(_) => todo!("HMIROp::Type"),
            HMIROp::Control(control) => todo!("HMIROp::Control({})", control.path()),
            HMIROp::OwnershipOp(ownership) => {
                todo!("HMIROp::OwnershipOp({})", ownership.path())
            }
            HMIROp::AggregateOp(_) => todo!("HMIROp::AggregateOp"),
        },

        HMIRExprKind::Def(_) => todo!("HMIRExprKind::Def"),
        HMIRExprKind::Hole(_) => todo!("HMIRExprKind::Hole"),
        HMIRExprKind::Comptime(_) => todo!("HMIRExprKind::Comptime"),
        HMIRExprKind::Quote { .. } => todo!("HMIRExprKind::Quote"),
        HMIRExprKind::Splice { .. } => todo!("HMIRExprKind::Splice"),
        HMIRExprKind::Intrinsic(_) => todo!("HMIRExprKind::Intrinsic"),
        HMIRExprKind::Error(_) => todo!("HMIRExprKind::Error"),
        HMIRExprKind::Call { .. } => todo!("HMIRExprKind::Call"),
        HMIRExprKind::For { .. } => todo!("HMIRExprKind::For"),
        HMIRExprKind::Switch { .. } => todo!("HMIRExprKind::Switch"),
        HMIRExprKind::Case { .. } => todo!("HMIRExprKind::Case"),
        HMIRExprKind::Match { .. } => todo!("HMIRExprKind::Match"),
        HMIRExprKind::Label { .. } => todo!("HMIRExprKind::Label"),
    }
}
