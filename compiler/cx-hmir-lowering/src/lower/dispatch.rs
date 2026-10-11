use cx_hmir::{
    HMIRBody, HMIRControlOp, HMIROp,
    expr::{HMIRExpr, HMIRExprKind},
    value::HMIRValue,
};
use cx_log::CXResult;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        binding::{lower_block, lower_let, lower_local},
        coerce::lower_coerce,
        constant::lower_constant,
        control::{lower_if, lower_return, lower_while},
        op::{lower_binary, lower_unary},
        place::{lower_address_of, lower_assign, lower_dereference},
    },
};

pub fn lower_expression(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: &HMIRExpr,
) -> CXResult<HMIRValue> {
    let span = expr.span();

    match expr.kind() {
        HMIRExprKind::Constant(constant) => lower_constant(context, body, span, constant),
        HMIRExprKind::Local(local) => lower_local(context, span, *local),
        HMIRExprKind::Let { local, initializer } => {
            lower_let(context, body, span, *local, *initializer)
        }
        HMIRExprKind::Block { statements } => lower_block(context, body, span, statements),

        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(context, body, span, *condition, *then_branch, *else_branch),
        HMIRExprKind::While {
            condition,
            body: loop_body,
            pre_eval,
        } => lower_while(context, body, span, *condition, *loop_body, *pre_eval),

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => lower_binary(context, body, span, *op, *lhs, *rhs),
            HMIROp::UnOp { op, operand } => lower_unary(context, body, span, *op, *operand),
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => lower_coerce(context, body, span, *mode, *value, *target),
            HMIROp::Control(HMIRControlOp::Return(value)) => {
                lower_return(context, body, span, *value)
            }

            HMIROp::Assign { target, op, value } => {
                lower_assign(context, body, span, *target, *op, *value)
            }
            HMIROp::AddressOf(operand) => lower_address_of(context, body, span, *operand),
            HMIROp::Dereference(pointer) => lower_dereference(context, body, span, *pointer),

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
