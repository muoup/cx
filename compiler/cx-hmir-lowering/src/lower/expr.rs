pub(crate) mod op;
pub(crate) mod storage;
pub(crate) mod value;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_intrinsics::VAIntrinsic;
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRInstructionKind, MIRInternalIntrinsic,
    MIRIntrinsic, MIRPlaceID, MIRRegister, MIRRegisterDecl, MIRTarget, MIRVAIntrinsic, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use self::storage::slot;
use crate::{
    env::function::{
        LocalLoweringContext,
        runtime::{Emission, RuntimeScope},
    },
    lower::types::lower_type,
    value::{Bindings, HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

pub fn function_address(
    context: &mut LocalLoweringContext,
    function: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::Constant(function_constant) = function.kind() else {
        unreachable!("a function designator is constant")
    };

    let cx_mir::MIRConstant::Function(fn_id) =
        value::constant(context, function_constant, function.ty(), span)?
    else {
        unreachable!("a function designator lowers to a function")
    };

    let (out, result) = output(context, ty, span)?;
    intrinsic(context, MIRInternalIntrinsic::GetFnPtr { out, fn_id }, span);
    Ok(result)
}

pub fn block(context: &mut LocalLoweringContext, name: &str) -> MIRBasicBlockID {
    context
        .runtime_mut()
        .builder_mut()
        .add_block(Some(CXIdent::new(name)))
}

pub fn current_block(context: &LocalLoweringContext) -> MIRBasicBlockID {
    context.runtime().builder().current_block()
}

pub fn switch_to(context: &mut LocalLoweringContext, block: MIRBasicBlockID) {
    let runtime = context.runtime_mut();

    runtime.builder_mut().switch_to(block);
    if runtime.emission() != Emission::Unevaluated {
        runtime.set_emission(Emission::Enabled);
    }
}

pub fn jump(context: &mut LocalLoweringContext, target: MIRBasicBlockID, span: &TokenRange) {
    let target = MIRBlockTarget::new(target);
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Jump { target }, span.clone())
}

pub fn branch(
    context: &mut LocalLoweringContext,
    cond: MIRValue,
    true_target: MIRBasicBlockID,
    false_target: MIRBasicBlockID,
    span: &TokenRange,
) {
    let branch = MIRInstructionKind::Branch {
        cond,
        true_target: MIRBlockTarget::new(true_target),
        false_target: MIRBlockTarget::new(false_target),
    };
    context.runtime_mut().emit(branch, span.clone())
}

fn intrinsic(
    context: &mut LocalLoweringContext,
    intrinsic: impl Into<MIRIntrinsic>,
    span: &TokenRange,
) {
    context.runtime_mut().emit(
        MIRInstructionKind::IntrinsicOp(intrinsic.into()),
        span.clone(),
    )
}

pub fn assert(
    context: &mut LocalLoweringContext,
    condition: &HMIRValue,
    message: Option<&str>,
    span: &TokenRange,
) -> CXResult<()> {
    if message.is_some() && !context.env().settings().assertions() {
        return Ok(());
    }

    let condition = value::standard(context, condition, span)?;
    let kind = match message {
        Some(message) => MIRInternalIntrinsic::Assert {
            condition,
            message: Some(message.into()),
        },
        None => MIRInternalIntrinsic::Assume { condition },
    };
    intrinsic(context, kind, span);
    Ok(())
}

fn register(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let ty = lower_type(context.env_mut(), ty, span)?;

    Ok(context
        .runtime_mut()
        .builder_mut()
        .allocate_register(MIRRegisterDecl::new(ty, None)))
}

// A fresh register for the result of an operation, which holds it once the operation that writes it is emitted
fn output(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, HMIRValue)> {
    let register = register(context, ty, span)?;

    Ok((
        MIRTarget::Register(register),
        slot(context, ty, MIRBindable::Register(register)),
    ))
}

pub fn placeholder(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    Ok(output(context, ty, span)?.1)
}

pub fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let callee = match callee.as_function() {
        Some(function) => {
            let HMIRValueKind::Constant(constant_value) = function.kind() else {
                unreachable!("a known function is constant")
            };
            MIRValue::Constant(value::constant(
                context,
                constant_value,
                function.ty(),
                span,
            )?)
        }
        None => value::standard(context, callee, span)?,
    };
    let args = args
        .iter()
        .map(|arg| value::standard(context, arg, span))
        .collect::<CXResult<_>>()?;

    let returns = context.env().resolve_kind(result);
    let (out, value) = match matches!(returns, HMIRTypeKind::Void | HMIRTypeKind::Unreachable) {
        true => (None, HMIRValue::constant(result, HMIRImmediate::Unit)),
        false => {
            let register = register(context, result, span)?;
            (
                Some(register),
                slot(context, result, MIRBindable::Register(register)),
            )
        }
    };
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Call { out, callee, args }, span.clone());

    Ok(value)
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    op: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    let (instruction, result) = match op {
        VAIntrinsic::Start { list, last } => {
            let list = value::standard(context, list, span)?;
            let last = value::standard(context, last, span)?;
            (MIRVAIntrinsic::VaStart { list, last }, None)
        }
        VAIntrinsic::End { list } => {
            let list = value::standard(context, list, span)?;
            (MIRVAIntrinsic::VaEnd { list }, None)
        }
        VAIntrinsic::Arg { list, ty } => {
            let list = value::standard(context, list, span)?;
            let mir_ty = lower_type(context.env_mut(), *ty, span)?;
            let (out, result) = output(context, *ty, span)?;
            (
                MIRVAIntrinsic::VaArg {
                    out,
                    list,
                    ty: mir_ty,
                },
                Some(result),
            )
        }
    };
    intrinsic(context, instruction, span);
    Ok(result)
}
