use cx_hmir::{HMIRExprID, HMIRExprKind};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, control, def_value, exec, exec_native, expr, liveness},
    function::{
        Expect, FunctionLowering, LowerResult, Operand, Stop,
        call::lower_call,
        control::{
            lower_block, lower_for, lower_if, lower_label, lower_match, lower_switch, lower_while,
        },
        expr::{
            lower_expr, lower_intrinsic_expr, lower_let, lower_local, lower_native, lower_splice,
            lower_static_operand,
        },
        lower_eval, lower_eval_frame,
    },
    program::Program,
    staging_error,
    value::StaticValue,
};

pub(crate) enum LowerContext<'a, 'p, 'l> {
    Runtime(&'a mut FunctionLowering<'p, 'l>, usize),
    Comptime(&'a mut Program<'l>, &'a mut EvalFrame),
}

pub(crate) enum LowerOutput {
    Runtime(Operand),
    Comptime(Flow),
}

impl<'l> LowerContext<'_, '_, 'l> {
    fn program(&mut self) -> &mut Program<'l> {
        match self {
            Self::Runtime(lowering, _) => lowering.program,
            Self::Comptime(program, _) => program,
        }
    }
}

pub(crate) fn lower(
    mut cx: LowerContext<'_, '_, '_>,
    id: HMIRExprID,
    expect: Expect,
) -> LowerResult<LowerOutput> {
    let (unit, kind, span) = match &cx {
        LowerContext::Runtime(lowering, frame) => (
            lowering.frames[*frame].def.unit(),
            lowering.kind(*frame, id),
            lowering.span(*frame, id),
        ),
        LowerContext::Comptime(_, frame) => (
            frame.def().unit(),
            frame.body().expr(id).kind().clone(),
            frame.body().expr(id).span().clone(),
        ),
    };
    match kind {
        HMIRExprKind::Constant(constant) => {
            let imported = cx.program().import_constant(unit, &constant, &span)?;
            value(cx, imported, &span)
        }
        HMIRExprKind::Def(def) => {
            let key = cx.program().resolve(unit, &def, &span)?;
            let resolved = def_value(cx.program(), key, &span)?;
            value(cx, resolved, &span)
        }
        HMIRExprKind::Local(local) => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_local(lowering, frame, local, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, frame) => {
                liveness::require_live(frame, local, &span)?;
                let value = frame.local(local).cloned().ok_or_else(|| {
                    let name = frame
                        .body()
                        .local(local)
                        .name()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| local.to_string());
                    staging_error(&span, format!("'{name}' is not available at compile time"))
                })?;
                Ok(LowerOutput::Comptime(Flow::Normal(value)))
            }
        },
        HMIRExprKind::Hole(_) => match cx {
            LowerContext::Comptime(_, _) if expect.ty().is_some() => Ok(LowerOutput::Comptime(
                Flow::Normal(StaticValue::Type(expect.ty().unwrap())),
            )),
            _ => Err(staging_error(&span, "cannot infer this type".into()).into()),
        },
        HMIRExprKind::Error => Err(staging_error(&span, "erroneous expression".into()).into()),
        HMIRExprKind::Comptime(inner) => match cx {
            LowerContext::Runtime(lowering, frame) => {
                if lowering.unevaluated {
                    return lower_expr(lowering, frame, inner, expect).map(LowerOutput::Runtime);
                }
                let value = lower_eval(lowering, frame, inner, expect)?;
                lower_static_operand(lowering, value, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => exec(program, frame, inner, expect.ty())
                .map(LowerOutput::Comptime)
                .map_err(Stop::Error),
        },
        HMIRExprKind::Quote { params, body } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                let value = lower_eval_frame(lowering, frame).quote(params, body);
                lower_static_operand(lowering, value, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, frame) => Ok(LowerOutput::Comptime(Flow::Normal(
                frame.quote(params, body),
            ))),
        },
        HMIRExprKind::Splice { quote, args } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_splice(lowering, frame, quote, &args, expect, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, _) => {
                Err(staging_error(&span, "splice inside a comptime context".into()).into())
            }
        },
        HMIRExprKind::Intrinsic(intrinsic) => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_intrinsic_expr(lowering, frame, intrinsic, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, _) => {
                Err(staging_error(&span, "intrinsic at compile time".into()).into())
            }
        },
        HMIRExprKind::Native(op) => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_native(lowering, frame, id, op, expect, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => {
                exec_native(program, frame, id, &op, &span, expect.ty())
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::Let { local, initializer } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_let(lowering, frame, local, initializer, &span)?;
                Ok(LowerOutput::Runtime(Operand::unit(
                    lowering.program.types_mut(),
                )))
            }
            LowerContext::Comptime(program, frame) => {
                expr::bind(program, frame, local, initializer, &span)
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::Call { callee, args } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_call(lowering, frame, callee, &args, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => {
                expr::call(program, frame, callee, &args, &span)
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::Block {
            kind,
            statements,
            tail,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_block(lowering, frame, kind, &statements, tail, expect, &span)
                    .map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => {
                control::block(program, frame, kind, &statements, tail)
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => lower_if(
                lowering,
                frame,
                condition,
                then_branch,
                else_branch,
                expect,
                &span,
            )
            .map(LowerOutput::Runtime),
            LowerContext::Comptime(program, frame) => control::conditional(
                program,
                frame,
                condition,
                then_branch,
                else_branch,
                expect.ty(),
                &span,
            )
            .map(LowerOutput::Comptime)
            .map_err(Stop::Error),
        },
        HMIRExprKind::While {
            condition,
            body,
            pre_eval,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_while(lowering, frame, condition, body, pre_eval, &span)
                    .map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => {
                control::while_loop(program, frame, condition, body, pre_eval, &span)
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::For {
            init,
            condition,
            increment,
            body,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_for(lowering, frame, init, condition, increment, body, &span)
                    .map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => {
                control::for_loop(program, frame, init, condition, increment, body, &span)
                    .map(LowerOutput::Comptime)
                    .map_err(Stop::Error)
            }
        },
        HMIRExprKind::Switch {
            condition,
            cases,
            default,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_switch(lowering, frame, condition, &cases, default, &span)
                    .map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, _) => {
                Err(staging_error(&span, "switch at compile time".into()).into())
            }
        },
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_match(lowering, frame, scrutinee, subject, &arms, expect, &span)
                    .map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(_, _) => {
                Err(staging_error(&span, "match at compile time".into()).into())
            }
        },
        HMIRExprKind::Label { name, body } => match cx {
            LowerContext::Runtime(lowering, frame) => {
                lower_label(lowering, frame, name, body, expect, &span).map(LowerOutput::Runtime)
            }
            LowerContext::Comptime(program, frame) => exec(program, frame, body, expect.ty())
                .map(LowerOutput::Comptime)
                .map_err(Stop::Error),
        },
    }
}

fn value(
    cx: LowerContext<'_, '_, '_>,
    value: StaticValue,
    span: &TokenRange,
) -> LowerResult<LowerOutput> {
    match cx {
        LowerContext::Runtime(lowering, _) => {
            lower_static_operand(lowering, value, span).map(LowerOutput::Runtime)
        }
        LowerContext::Comptime(_, _) => Ok(LowerOutput::Comptime(Flow::Normal(value))),
    }
}
