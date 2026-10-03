use cx_hmir::{HMIRBlockKind, HMIRExprID};
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    function::{Expect, Frame, FunctionLowering, Operand, Stop, expr::lower_expr},
    program::Program,
    ty::{TypeID, TypeKind},
};

pub(super) enum Check<'a> {
    Contract,
    Deferred(HMIRExprID),
    Sequence {
        statements: &'a [HMIRExprID],
        tail: Option<HMIRExprID>,
        expect: Expect,
    },
}

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
    lowering.check_return = false;
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

pub(super) fn check(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    request: Check<'_>,
) -> CXResult<()> {
    let mut checking = FunctionLowering {
        program: cx.program,
        serial: cx.serial,
        body: cx.body.clone(),
        current: cx.current,
        frames: cx.frames.clone(),
        bindings: cx.bindings.clone(),
        scopes: cx.scopes.clone(),
        controls: cx.controls.clone(),
        merges: cx.merges.clone(),
        labels: cx.labels.clone(),
        pattern_bindings: cx.pattern_bindings.clone(),
        ret: cx.ret,
        check_return: cx.check_return,
        unevaluated: true,
        safe: cx.safe,
        unsafe_depth: cx.unsafe_depth,
        defer_boundary: cx.defer_boundary,
    };
    let block = checking.new_block("check.unreachable");
    checking.set_block(block);
    let (result, span, deferred) = match request {
        Check::Contract => return super::contract::check_contract(&mut checking),
        Check::Deferred(body) => {
            checking.defer_boundary = Some(checking.controls.len());
            let span = checking.span(frame, body);
            (
                lower_expr(&mut checking, frame, body, Expect::Discard),
                span,
                true,
            )
        }
        Check::Sequence {
            statements,
            tail,
            expect,
        } => {
            let Some(first) = statements.first().copied().or(tail) else {
                return Ok(());
            };
            let span = checking.span(frame, first);
            (
                super::control::lower_block(
                    &mut checking,
                    frame,
                    HMIRBlockKind::Sequence,
                    statements,
                    tail,
                    expect,
                    &span,
                ),
                span,
                false,
            )
        }
    };
    match result {
        Ok(value) if deferred && !checking.program.types().is_void(value.ty()) => Err(
            crate::staging_error(&span, "defer requires a void expression".into()),
        ),
        Err(Stop::Diverged) if deferred => Err(crate::staging_error(
            &span,
            "deferred expression must fall through".into(),
        )),
        Ok(_) | Err(Stop::Diverged) => Ok(()),
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
