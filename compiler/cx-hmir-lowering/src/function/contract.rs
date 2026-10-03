use std::rc::Rc;

use cx_hmir::{HMIRContract, HMIRDefKind, HMIRExprID};
use cx_log::catalogue::typecheck;
use cx_mir::{MIRInternalIntrinsic, MIRValue};

use crate::{
    eval::{Signature, eval_frame_for},
    function::{
        Expect, Frame, FunctionLowering, LowerResult, Operand, Stop, coerce::lower_truthy,
        control::lower_scope, expr::lower_expr, inspect, operand::lower_value,
    },
    program::Instance,
};

enum Check {
    Assert(&'static str),
    Assume,
}

// A call's contract, with the callee's parameters bound to the argument values
pub(super) struct CallContract {
    frame: usize,
    contract: HMIRContract,
}

fn contract_of(cx: &FunctionLowering<'_, '_>, instance: &Instance) -> HMIRContract {
    let unit = cx.program.unit(instance.0.unit());
    match unit.def(instance.0.def()).kind() {
        HMIRDefKind::Function(function) => function.signature().contract().clone(),
        _ => HMIRContract::default(),
    }
}

fn lower_condition(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    condition: HMIRExprID,
    check: Check,
) -> LowerResult<()> {
    let span = cx.span(frame, condition);
    let safe = cx.safe;
    let unsafe_depth = cx.unsafe_depth;
    cx.safe = contract_of(cx, &cx.frames[frame].owner).is_safe();
    cx.unsafe_depth = 0;
    let result = lower_scope(cx, &span, |cx| {
        let value = match lower_expr(cx, frame, condition, Expect::Any) {
            Err(Stop::Diverged) => {
                return cx.error(
                    &span,
                    &typecheck::TYPE_REQUIREMENT,
                    ("function condition".into(), "a value".into(), None),
                );
            }
            result => result?,
        };
        let value = lower_truthy(cx, value, &span)?;
        let condition = lower_value(cx, value, &span)?;
        cx.intrinsic(
            match check {
                Check::Assert(message) => MIRInternalIntrinsic::Assert {
                    condition,
                    message: Some(message.into()),
                },
                Check::Assume => MIRInternalIntrinsic::Assume { condition },
            },
            &span,
        );
        Ok(())
    });
    cx.safe = safe;
    cx.unsafe_depth = unsafe_depth;
    result
}

pub(super) fn check_contract(
    cx: &mut FunctionLowering<'_, '_>,
) -> Result<(), cx_log::error::CXError> {
    let contract = contract_of(cx, &cx.frames[0].owner);
    let result = (|| -> LowerResult<()> {
        if let Some(condition) = contract.precondition() {
            lower_condition(cx, 0, condition, Check::Assume)?;
        }
        if let Some((binding, condition)) = contract.postcondition() {
            if let Some(binding) = binding {
                if cx.program.types().is_void(cx.ret) {
                    return cx.error(
                        &cx.span(0, condition),
                        &typecheck::VOID_POSTCONDITION_BINDING,
                        (),
                    );
                }
                let value = inspect::binding(cx, cx.ret, &cx.span(0, condition))?;
                cx.bind(0, binding, value);
            }
            lower_condition(cx, 0, condition, Check::Assume)?;
        }
        Ok(())
    })();
    match result {
        Ok(()) => Ok(()),
        Err(Stop::Error(error)) => Err(error),
        Err(Stop::Diverged) => unreachable!(),
    }
}

pub(super) fn lower_call_precondition(
    cx: &mut FunctionLowering<'_, '_>,
    instance: &Instance,
    signature: &Signature,
    args: &[MIRValue],
) -> LowerResult<Option<CallContract>> {
    let contract = contract_of(cx, instance);
    if cx.unevaluated || (contract.precondition().is_none() && contract.postcondition().is_none()) {
        return Ok(None);
    }
    let unit = cx.program.unit(instance.0.unit());
    let mut callee = Frame::new(unit, instance.0, Rc::new(instance.clone()));
    callee.statics = eval_frame_for(cx.program, instance).locals().clone();
    cx.frames.push(callee);
    let frame = cx.frames.len() - 1;
    for ((local, (_, ty)), value) in signature.runtime().iter().zip(signature.params()).zip(args) {
        cx.bind(frame, *local, Operand::value(value.clone(), *ty));
    }
    if let Some(condition) = contract.precondition() {
        lower_condition(cx, frame, condition, Check::Assert("Precondition failed"))?;
    }
    Ok(Some(CallContract { frame, contract }))
}

pub(super) fn lower_call_postcondition(
    cx: &mut FunctionLowering<'_, '_>,
    call: Option<CallContract>,
    result: &Operand,
) -> LowerResult<()> {
    let Some(CallContract { frame, contract }) = call else {
        return Ok(());
    };
    let Some((binding, condition)) = contract.postcondition() else {
        return Ok(());
    };
    if let Some(binding) = binding {
        cx.bind(frame, binding, result.clone());
    }
    lower_condition(cx, frame, condition, Check::Assume)
}

pub(super) fn lower_return_postcondition(
    cx: &mut FunctionLowering<'_, '_>,
    value: Option<&MIRValue>,
) -> LowerResult<()> {
    let instance = cx.frames[0].owner.clone();
    let Some((binding, condition)) = contract_of(cx, &instance).postcondition() else {
        return Ok(());
    };
    if let (Some(binding), Some(value)) = (binding, value) {
        cx.bind(0, binding, Operand::value(value.clone(), cx.ret));
    }
    lower_condition(cx, 0, condition, Check::Assert("Postcondition Failed!"))
}
