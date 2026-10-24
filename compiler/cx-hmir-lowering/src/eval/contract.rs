use cx_hmir::unit::function::HMIRContract;
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

use super::{coerce, eval, flow, storage, types};

pub(crate) fn check(
    context: &mut LocalLoweringContext,
    contract: &HMIRContract,
    args: &[HMIRValue],
    result: Option<&HMIRValue>,
    name: &str,
    span: &TokenRange,
) -> CXResult<()> {
    let (local, expression, label) = match result {
        None => match contract.precondition() {
            Some(expression) => (None, expression, "precondition"),
            None => return Ok(()),
        },
        Some(_) => match contract.postcondition() {
            Some((local, expression)) => (local, expression, "postcondition"),
            None => return Ok(()),
        },
    };
    let evaluation = context.evaluation();
    let mut child = context.child(
        None,
        FnContext::Comptime(ComptimeContext::new()),
        Some(evaluation),
    );
    for (local, value) in contract
        .params()
        .iter()
        .zip(args)
        .map(|(id, value)| (*id, value))
        .chain(local.zip(result))
    {
        let declaration = contract.body().local(local);
        let target = emit::allocate(&mut child, value.ty(), declaration.name().cloned(), span)?;
        storage::initialize(&mut child, &target, value, span)?;
        let reference = types::reference(child.env_mut(), value.ty(), span);
        emit::storage::bind_local(&mut child, local, target.with_ty(reference));
    }
    let value = flow::top(eval(&mut child, contract.body(), expression))?;
    let value = coerce::condition(&mut child, value, span)?;
    if !matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) if *value != 0)
    {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_ASSERTION,
            Some(format!("{label} of '{name}' does not hold at compile time")),
        ));
    }
    Ok(())
}
