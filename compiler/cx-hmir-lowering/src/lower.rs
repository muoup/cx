use cx_hmir::{HMIRExprID, HMIRExprKind};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, control, expr},
    function::{Expect, FunctionLowering, Lower, Operand},
    program::Program,
    staging_error,
    value::StaticValue,
};

pub(crate) enum Context<'a, 'p, 'l> {
    Runtime(&'a mut FunctionLowering<'p, 'l>, usize),
    Comptime(&'a mut Program<'l>, &'a mut EvalFrame),
}

pub(crate) enum Output {
    Runtime(Operand),
    Comptime(Flow),
}

impl<'l> Context<'_, '_, 'l> {
    fn program(&mut self) -> &mut Program<'l> {
        match self {
            Self::Runtime(lowering, _) => lowering.program,
            Self::Comptime(program, _) => program,
        }
    }
}

pub(crate) fn lower(mut cx: Context<'_, '_, '_>, id: HMIRExprID, expect: Expect) -> Lower<Output> {
    let (unit, kind, span) = match &cx {
        Context::Runtime(lowering, frame) => (
            lowering.frames[*frame].def.unit(),
            lowering.kind(*frame, id),
            lowering.span(*frame, id),
        ),
        Context::Comptime(_, frame) => (
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
            let resolved = cx.program().def_value(key, &span)?;
            value(cx, resolved, &span)
        }
        HMIRExprKind::Local(local) => match cx {
            Context::Runtime(lowering, frame) => {
                lowering.local(frame, local, &span).map(Output::Runtime)
            }
            Context::Comptime(_, frame) => {
                let value = frame.local(local).cloned().ok_or_else(|| {
                    let name = frame
                        .body()
                        .local(local)
                        .name()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| local.to_string());
                    staging_error(&span, format!("'{name}' is not available at compile time"))
                })?;
                Ok(Output::Comptime(Flow::Normal(value)))
            }
        },
        HMIRExprKind::Hole(_) => match cx {
            Context::Comptime(_, _) if expect.ty().is_some() => Ok(Output::Comptime(Flow::Normal(
                StaticValue::Type(expect.ty().unwrap()),
            ))),
            _ => Err(staging_error(&span, "cannot infer this type".into()).into()),
        },
        HMIRExprKind::Error => Err(staging_error(&span, "erroneous expression".into()).into()),
        HMIRExprKind::Comptime(inner) => match cx {
            Context::Runtime(lowering, frame) => {
                if lowering.unevaluated {
                    return lowering.expr(frame, inner, expect).map(Output::Runtime);
                }
                let value = lowering.eval(frame, inner, expect)?;
                lowering.static_operand(value, &span).map(Output::Runtime)
            }
            Context::Comptime(program, frame) => program
                .exec(frame, inner, expect.ty())
                .map(Output::Comptime)
                .map_err(Into::into),
        },
        HMIRExprKind::Quote { params, body } => match cx {
            Context::Runtime(lowering, frame) => {
                let value = lowering.eval_frame(frame).quote(params, body);
                lowering.static_operand(value, &span).map(Output::Runtime)
            }
            Context::Comptime(_, frame) => {
                Ok(Output::Comptime(Flow::Normal(frame.quote(params, body))))
            }
        },
        HMIRExprKind::Splice { quote, args } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .splice(frame, quote, &args, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(_, _) => {
                Err(staging_error(&span, "splice inside a comptime context".into()).into())
            }
        },
        HMIRExprKind::Intrinsic(intrinsic) => match cx {
            Context::Runtime(lowering, frame) => lowering
                .intrinsic_expr(frame, intrinsic, &span)
                .map(Output::Runtime),
            Context::Comptime(_, _) => {
                Err(staging_error(&span, "intrinsic at compile time".into()).into())
            }
        },
        HMIRExprKind::Native(op) => match cx {
            Context::Runtime(lowering, frame) => lowering
                .native(frame, id, op, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => program
                .exec_native(frame, id, &op, &span, expect.ty())
                .map(Output::Comptime)
                .map_err(Into::into),
        },
        HMIRExprKind::Let { local, initializer } => match cx {
            Context::Runtime(lowering, frame) => {
                lowering.lower_let(frame, local, initializer, &span)?;
                Ok(Output::Runtime(Operand::unit(lowering.program.types_mut())))
            }
            Context::Comptime(program, frame) => {
                expr::bind(program, frame, local, initializer, &span)
                    .map(Output::Comptime)
                    .map_err(Into::into)
            }
        },
        HMIRExprKind::Call { callee, args } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .call(frame, callee, &args, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => expr::call(program, frame, callee, &args, &span)
                .map(Output::Comptime)
                .map_err(Into::into),
        },
        HMIRExprKind::Block {
            kind,
            statements,
            tail,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .block(frame, kind, &statements, tail, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => {
                control::block(program, frame, kind, &statements, tail)
                    .map(Output::Comptime)
                    .map_err(Into::into)
            }
        },
        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_if(frame, condition, then_branch, else_branch, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => control::conditional(
                program,
                frame,
                condition,
                then_branch,
                else_branch,
                expect.ty(),
                &span,
            )
            .map(Output::Comptime)
            .map_err(Into::into),
        },
        HMIRExprKind::While {
            condition,
            body,
            pre_eval,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_while(frame, condition, body, pre_eval, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => {
                control::while_loop(program, frame, condition, body, pre_eval, &span)
                    .map(Output::Comptime)
                    .map_err(Into::into)
            }
        },
        HMIRExprKind::For {
            init,
            condition,
            increment,
            body,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_for(frame, init, condition, increment, body, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => {
                control::for_loop(program, frame, init, condition, increment, body, &span)
                    .map(Output::Comptime)
                    .map_err(Into::into)
            }
        },
        HMIRExprKind::Switch {
            condition,
            cases,
            default,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_switch(frame, condition, &cases, default, &span)
                .map(Output::Runtime),
            Context::Comptime(_, _) => {
                Err(staging_error(&span, "switch at compile time".into()).into())
            }
        },
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_match(frame, scrutinee, subject, &arms, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(_, _) => {
                Err(staging_error(&span, "match at compile time".into()).into())
            }
        },
        HMIRExprKind::Label { name, body } => match cx {
            Context::Runtime(lowering, frame) => lowering
                .lower_label(frame, name, body, expect, &span)
                .map(Output::Runtime),
            Context::Comptime(program, frame) => program
                .exec(frame, body, expect.ty())
                .map(Output::Comptime)
                .map_err(Into::into),
        },
    }
}

fn value(cx: Context<'_, '_, '_>, value: StaticValue, span: &TokenRange) -> Lower<Output> {
    match cx {
        Context::Runtime(lowering, _) => lowering.static_operand(value, span).map(Output::Runtime),
        Context::Comptime(_, _) => Ok(Output::Comptime(Flow::Normal(value))),
    }
}
