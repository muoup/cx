use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBlockTarget, MIRConstant, MIRFunctionID, MIRFunctionMode,
    MIRInstrKind, MIRIntType, MIRParameterID, MIRPlace, MIRUnaryOp, MIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    error::comptime_error,
    interpretable::{ComptimeInterpretable, InterpretedFunction},
    value::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue},
};

use super::{
    MIRComptimeEngine, memory, ops,
    state::{Frame, PathSeg},
};

pub(super) fn run<'ctx>(
    engine: &mut MIRComptimeEngine<'ctx>,
    entry: InterpretedFunction<'ctx>,
    args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    push_frame(engine, entry, args);
    run_top_frame(engine)
}

fn push_frame<'ctx>(
    engine: &mut MIRComptimeEngine<'ctx>,
    code: InterpretedFunction<'ctx>,
    args: &[MIRComptimeValue],
) {
    let mut frame = Frame::new(code);
    for (index, value) in args.iter().enumerate() {
        frame.cells.insert(
            MIRPlace::Parameter(MIRParameterID::new(index)),
            value.clone(),
        );
    }

    let entry_params = frame.code.block_params(frame.code.current_block()).to_vec();
    for (register, value) in entry_params.into_iter().zip(args.iter()) {
        frame.registers.insert(register, value.clone());
    }

    engine.frames.push(frame);
}

fn run_top_frame(engine: &mut MIRComptimeEngine<'_>) -> CXResult<MIRComptimeValue> {
    loop {
        engine.steps += 1;
        if engine.steps > engine.limits.max_steps {
            return comptime_error(
                TokenRange::internal(),
                format!(
                    "comptime evaluation exceeded {} steps",
                    engine.limits.max_steps
                ),
            );
        }

        let (kind, range) = {
            let frame = engine
                .frames
                .last_mut()
                .expect("engine ran without a frame");
            match frame.code.next_instruction() {
                Some(instruction) => (instruction.kind.clone(), instruction.token_range.clone()),
                None => {
                    return comptime_error(
                        TokenRange::internal(),
                        "block fell through without a terminating instruction",
                    );
                }
            }
        };

        match kind {
            MIRInstrKind::ScopeEnter { .. } | MIRInstrKind::ScopeExit { .. } => {}
            MIRInstrKind::Initialize { .. } | MIRInstrKind::Leak { .. } => {}
            MIRInstrKind::Create { out, ty } => {
                let frame = engine.frames.last_mut().expect("active frame");
                frame
                    .cells
                    .insert(out, MIRComptimeValue::Constant(MIRConstant::Undefined));
                let _ = ty;
            }
            MIRInstrKind::Assign { target, value, ty } => {
                let value = memory::read_value(engine, &value)?;
                match target {
                    MIRAssignTarget::Register(register) => {
                        let frame = engine.frames.last_mut().expect("active frame");
                        frame.registers.insert(register, value);
                    }
                    MIRAssignTarget::Place(place) => {
                        memory::write_place(engine, place, value, Some(ty))?;
                    }
                }
            }
            MIRInstrKind::AddressOf { out, place } => {
                let constant = memory::address_of(engine, place, &range)?;
                let frame = engine.frames.last_mut().expect("active frame");
                frame
                    .registers
                    .insert(out, MIRComptimeValue::Constant(constant));
            }
            MIRInstrKind::Dereference { .. } => {
                return comptime_error(
                    range,
                    "dereference is not supported in a comptime context yet",
                );
            }
            MIRInstrKind::AggregateOp(op) => execute_aggregate_op(engine, op)?,
            MIRInstrKind::Call {
                out, callee, args, ..
            } => {
                let callee_value = memory::read_constant(engine, &callee, &range)?;
                let function_id = match callee_value {
                    MIRConstant::Function(id) => id,
                    other => {
                        return comptime_error(
                            range,
                            format!("cannot call non-function comptime value {other:?}"),
                        );
                    }
                };
                let mut arguments = Vec::with_capacity(args.len());
                for argument in args {
                    arguments.push(memory::read_value(engine, &argument)?);
                }
                let result = call_function(engine, function_id, &arguments)?;
                if let Some(out) = out {
                    let frame = engine.frames.last_mut().expect("active frame");
                    frame.registers.insert(out, result);
                }
            }
            MIRInstrKind::VaStart { .. }
            | MIRInstrKind::VaEnd { .. }
            | MIRInstrKind::VaArg { .. } => {
                return comptime_error(range, "variadic operations are not comptime-capable");
            }
            MIRInstrKind::BinOp { out, op, lhs, rhs } => {
                let lhs = memory::read_constant(engine, &lhs, &range)?;
                let rhs = memory::read_constant(engine, &rhs, &range)?;
                let result = ops::evaluate_binop(engine, op, lhs, rhs)?;
                let frame = engine.frames.last_mut().expect("active frame");
                frame
                    .registers
                    .insert(out, MIRComptimeValue::Constant(result));
            }
            MIRInstrKind::UnOp { out, op, operand } => {
                if let MIRUnaryOp::Increment { amount, post } = op {
                    let old = memory::read_constant(engine, &operand, &range)?;
                    let updated = ops::increment_constant(old.clone(), amount)?;
                    match operand {
                        MIRValue::Register(register) => {
                            let frame = engine.frames.last_mut().expect("active frame");
                            frame
                                .registers
                                .insert(register, MIRComptimeValue::Constant(updated.clone()));
                        }
                        MIRValue::PlaceRef(place)
                        | MIRValue::Copy(place)
                        | MIRValue::Move(place) => {
                            memory::write_direct_cell(
                                engine,
                                place,
                                MIRComptimeValue::Constant(updated.clone()),
                            );
                        }
                        MIRValue::Constant(_) => {}
                    }
                    let exposed = if post { old } else { updated };
                    let frame = engine.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Constant(exposed));
                } else {
                    let operand = memory::read_constant(engine, &operand, &range)?;
                    let result = ops::evaluate_unop(op, operand)?;
                    let frame = engine.frames.last_mut().expect("active frame");
                    frame
                        .registers
                        .insert(out, MIRComptimeValue::Constant(result));
                }
            }
            MIRInstrKind::Coerce {
                out,
                operand,
                coercion,
                to_type,
            } => {
                let result = if matches!(
                    coercion,
                    cx_mir::MIRCoercion::TypeChange | cx_mir::MIRCoercion::ReinterpretBits
                ) {
                    match memory::coerce_global_special(engine, &operand, to_type)? {
                        Some(constant) => constant,
                        None => {
                            let operand = memory::read_constant(engine, &operand, &range)?;
                            ops::evaluate_coercion(coercion, operand, to_type)?
                        }
                    }
                } else {
                    let operand = memory::read_constant(engine, &operand, &range)?;
                    ops::evaluate_coercion(coercion, operand, to_type)?
                };
                let frame = engine.frames.last_mut().expect("active frame");
                frame
                    .registers
                    .insert(out, MIRComptimeValue::Constant(result));
            }
            MIRInstrKind::Assert { condition, message } => {
                let condition = memory::read_constant(engine, &condition, &range)?;
                if !ops::is_truthy(&condition) {
                    return comptime_error(
                        range,
                        message.unwrap_or_else(|| "assertion failed at compile time".into()),
                    );
                }
            }
            MIRInstrKind::Assume { condition } => {
                let _ = memory::read_constant(engine, &condition, &range)?;
            }
            MIRInstrKind::Return { value } => {
                let constant = match value {
                    Some(value) => memory::read_value(engine, &value)?,
                    None => MIRComptimeValue::Constant(MIRConstant::Unit),
                };
                engine.frames.pop();
                return Ok(constant);
            }
            MIRInstrKind::Jump { target } => jump_to(engine, target)?,
            MIRInstrKind::Branch {
                cond,
                true_target,
                false_target,
            } => {
                let condition = memory::read_constant(engine, &cond, &range)?;
                let target = if ops::is_truthy(&condition) {
                    true_target
                } else {
                    false_target
                };
                jump_to(engine, target)?;
            }
            MIRInstrKind::IntSwitch {
                value,
                cases,
                default,
            } => {
                let subject = memory::read_constant(engine, &value, &range)?;
                let mut taken = default;
                for (case, target) in cases {
                    if ops::constant_equals(&subject, &case) {
                        taken = Some(target.clone());
                        break;
                    }
                }
                match taken {
                    Some(target) => jump_to(engine, target)?,
                    None => {
                        return comptime_error(range, "integer switch fell through all cases");
                    }
                }
            }
            MIRInstrKind::VariantSwitch {
                subject,
                cases,
                default,
                ..
            } => {
                let subject = memory::read_constant(engine, &subject, &range)?;
                let discriminant = ops::variant_discriminant(&subject);
                let mut taken = default;
                for (variant, target) in cases {
                    if discriminant == Some(variant) {
                        taken = Some(target.clone());
                        break;
                    }
                }
                match taken {
                    Some(target) => jump_to(engine, target)?,
                    None => {
                        return comptime_error(range, "variant switch fell through all cases");
                    }
                }
            }
            MIRInstrKind::Unreachable => {
                return comptime_error(range, "unreachable code executed at compile time");
            }
            MIRInstrKind::MakeStaged {
                out,
                template,
                captures,
            } => {
                let mut bindings = Vec::with_capacity(captures.len());
                for capture in captures {
                    bindings.push(MIRStagedBinding::Comptime(memory::read_value(
                        engine, &capture,
                    )?));
                }
                let staged = MIRStagedValue::new(template, bindings, Vec::new(), None);
                let frame = engine.frames.last_mut().expect("active frame");
                frame
                    .registers
                    .insert(out, MIRComptimeValue::Staged(std::sync::Arc::new(staged)));
            }
            MIRInstrKind::ApplyStaged {
                out, staged, args, ..
            } => {
                let MIRComptimeValue::Staged(staged) = memory::read_value(engine, &staged)? else {
                    return comptime_error(range, "attempted to apply a non-staged value");
                };
                let mut bindings = Vec::with_capacity(args.len());
                for arg in args {
                    bindings.push(MIRStagedBinding::Comptime(memory::read_value(
                        engine, &arg,
                    )?));
                }
                if let Some(out) = out {
                    let frame = engine.frames.last_mut().expect("active frame");
                    frame.registers.insert(
                        out,
                        MIRComptimeValue::Staged(std::sync::Arc::new(staged.apply(bindings))),
                    );
                }
            }
            MIRInstrKind::StagedReturn { .. } => {
                return comptime_error(range, "staged template executed as a function");
            }
            MIRInstrKind::StagedExit { .. } => {
                return comptime_error(range, "staged exit executed as a function");
            }
            MIRInstrKind::StagedYield { .. } => {
                return comptime_error(range, "staged yield executed as a function");
            }
            MIRInstrKind::StagedMove { .. } => {
                return comptime_error(range, "staged move executed as a function");
            }
            MIRInstrKind::StagedUse { .. } => {
                return comptime_error(range, "staged use executed as a function");
            }
        }
    }
}

fn jump_to(engine: &mut MIRComptimeEngine<'_>, target: MIRBlockTarget) -> CXResult<()> {
    let params = {
        let frame = engine.frames.last().expect("active frame");
        frame.code.block_params(target.block).to_vec()
    };

    let mut values = Vec::with_capacity(target.args.len());
    for argument in &target.args {
        values.push(memory::read_value(engine, argument)?);
    }

    let frame = engine.frames.last_mut().expect("active frame");
    for (register, value) in params.into_iter().zip(values.into_iter()) {
        frame.registers.insert(register, value);
    }
    frame.code.jump_to_block(target.block);
    Ok(())
}

pub(super) fn call_function(
    engine: &mut MIRComptimeEngine<'_>,
    function_id: MIRFunctionID,
    args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    if engine.frames.len() >= engine.limits.max_call_depth {
        return comptime_error(
            TokenRange::internal(),
            format!(
                "comptime call depth exceeded {}",
                engine.limits.max_call_depth
            ),
        );
    }

    let resolver = engine.resolver;
    let Some(function) = resolver.resolve(function_id) else {
        return comptime_error(
            TokenRange::internal(),
            format!("function {function_id:?} is not available during comptime evaluation"),
        );
    };

    match function.mode() {
        MIRFunctionMode::Runtime => {
            return comptime_error(
                TokenRange::internal(),
                "runtime functions cannot be executed at compile time",
            );
        }
        MIRFunctionMode::Constexpr | MIRFunctionMode::Comptime => {}
    }

    let Some(entry) = InterpretedFunction::new(function) else {
        return comptime_error(
            TokenRange::internal(),
            format!("function {function_id:?} has no definition to interpret"),
        );
    };

    push_frame(engine, entry, args);
    run_top_frame(engine)
}

fn execute_aggregate_op(engine: &mut MIRComptimeEngine<'_>, op: MIRAggregateOp) -> CXResult<()> {
    match op {
        MIRAggregateOp::Place { out, op } => {
            use cx_mir::MIRPlaceAggregateOp as Op;

            let (root, path) = match op {
                Op::Field { base, field, .. } => {
                    let (root, mut path) = memory::resolve_projection(engine, base);
                    path.push(PathSeg::Field(field));
                    (root, path)
                }
                Op::Index { base, index, .. } => {
                    let index = memory::read_constant(engine, &index, &TokenRange::internal())?;
                    let index = match index {
                        MIRConstant::Integer { value, .. } => value,
                        other => {
                            return comptime_error(
                                TokenRange::internal(),
                                format!("array index is not an integer constant: {other:?}"),
                            );
                        }
                    };
                    let (root, mut path) = memory::resolve_projection(engine, base);
                    path.push(PathSeg::Index(index));
                    (root, path)
                }
                Op::Variant { base, variant, .. } => {
                    let (root, mut path) = memory::resolve_projection(engine, base);
                    path.push(PathSeg::Variant(variant));
                    (root, path)
                }
            };

            let frame = engine.frames.last_mut().expect("active frame");
            frame.derived.insert(out, (root, path));
            Ok(())
        }
        MIRAggregateOp::Value { out, op } => {
            use cx_mir::MIRValueAggregateOp as Op;

            let constant = match op {
                Op::Discriminant { value, .. } => {
                    let value = memory::read_constant(engine, &value, &TokenRange::internal())?;
                    match ops::variant_discriminant(&value) {
                        Some(discriminant) => MIRConstant::Integer {
                            value: discriminant as i128,
                            ty: MIRIntType::I64,
                            signed: false,
                        },
                        None => MIRConstant::Undefined,
                    }
                }
                Op::Construct { ty, fields } => {
                    let mut evaluated = Vec::with_capacity(fields.len());
                    for (index, field) in fields {
                        evaluated.push((
                            index,
                            memory::read_constant(engine, &field, &TokenRange::internal())?,
                        ));
                    }
                    MIRConstant::Aggregate {
                        ty,
                        fields: evaluated,
                    }
                }
                Op::Variant {
                    variant,
                    value,
                    sum_type,
                } => MIRConstant::Aggregate {
                    ty: sum_type,
                    fields: vec![(
                        variant,
                        memory::read_constant(engine, &value, &TokenRange::internal())?,
                    )],
                },
                Op::ProjectVariant { variant, value, .. } => {
                    let value = memory::read_constant(engine, &value, &TokenRange::internal())?;
                    memory::read_path(&value, &[PathSeg::Variant(variant)])
                }
            };

            let frame = engine.frames.last_mut().expect("active frame");
            frame
                .registers
                .insert(out, MIRComptimeValue::Constant(constant));
            Ok(())
        }
    }
}
