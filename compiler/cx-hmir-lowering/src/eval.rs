mod flow;

pub(crate) use flow::EvalResult;

use cx_hmir::{
    HMIRBody, HMIRFunction,
    body::HMIRLocalID,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::HMIRTypeID,
    value::HMIRValue,
};
use cx_log::CXResult;
use cx_mir::MIRFunction;
use cx_tokens::TokenRange;

use crate::env::{HMIREnvironment, function::FnLoweringContext};

pub(crate) fn lower_runtime_function(
    _env: &mut HMIREnvironment,
    _function: &HMIRFunction,
) -> CXResult<MIRFunction> {
    todo!("Lower a runtime function through the shared evaluator")
}

pub(crate) fn eval_comptime_function<'hmir>(
    _parent: &mut FnLoweringContext<'_, '_, 'hmir>,
    _function: &'hmir HMIRFunction,
    _args: &[HMIRConstant],
) -> CXResult<HMIRConstant> {
    todo!("Evaluate a comptime child, catch its return, and freeze its result")
}

pub(crate) fn eval(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    todo!("Dispatch HMIR expressions using the active execution context")
}

pub(crate) fn eval_comptime(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _expr: HMIRExprID,
) -> CXResult<HMIRConstant> {
    todo!("Evaluate an isolated comptime expression with frozen inputs")
}

pub(crate) fn eval_type(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _expr: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    todo!("Evaluate a type expression using the available comptime bindings")
}

pub(crate) fn operand(
    _context: &mut FnLoweringContext,
    _result: EvalResult<HMIRValue>,
    _span: &TokenRange,
) -> EvalResult<HMIRValue> {
    todo!("Propagate comptime exits; suppress runtime emission and provide a bottom operand")
}

pub(crate) fn quote(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _params: &[HMIRLocalID],
    _root: HMIRExprID,
) -> EvalResult<HMIRValue> {
    todo!("Capture an unevaluated HMIR body and frozen values as a staged DAG")
}

pub(crate) fn splice(
    _context: &mut FnLoweringContext,
    _quote: HMIRValue,
    _args: &[HMIRValue],
) -> EvalResult<HMIRValue> {
    todo!("Materialize staged code into the runtime context with fresh local bindings")
}
