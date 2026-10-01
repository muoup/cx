use cx_hmir::HMIRExprID;
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    function::{Expect, Frame, FunctionLowering, Operand, Stop, expr::lower_expr},
    program::Program,
    ty::{TypeID, TypeKind},
};

pub(crate) fn inspect(
    program: &mut Program<'_>,
    frame: &EvalFrame,
    id: HMIRExprID,
    expect: Option<TypeID>,
) -> CXResult<TypeID> {
    let span = frame.body().expr(id).span().clone();
    let ret = program.types_mut().void();
    let serial = program.next_serial();
    let mut lowering = FunctionLowering::new(program, serial, ret, &span);
    lowering.unevaluated = true;
    let mut source = Frame::new(frame.unit().clone(), frame.def(), frame.owner().clone());
    source.statics = frame.locals().clone();
    lowering.frames.push(source);
    for (local, ty) in frame.runtime_types() {
        let operand = binding(&mut lowering, ty, &span)?;
        lowering.bind(0, local, operand);
    }
    match lower_expr(&mut lowering, 0, id, Expect::of(expect)) {
        Ok(value) => Ok(value.ty()),
        Err(Stop::Diverged) => Ok(lowering.program.types_mut().intern(TypeKind::Unreachable)),
        Err(Stop::Error(error)) => Err(error),
    }
}

pub(super) fn binding(
    lowering: &mut FunctionLowering<'_, '_>,
    ty: TypeID,
    span: &TokenRange,
) -> CXResult<Operand> {
    if matches!(
        lowering.program.types().kind(ty),
        TypeKind::Type | TypeKind::Expr { .. }
    ) {
        return Ok(Operand::value(MIRValue::Constant(MIRConstant::Unit), ty));
    }
    match lowering.place(ty, None, span) {
        Ok(place) => Ok(Operand::place(place, ty)),
        Err(Stop::Error(error)) => Err(error),
        Err(Stop::Diverged) => unreachable!(),
    }
}
