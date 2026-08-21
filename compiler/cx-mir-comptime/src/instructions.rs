use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRCallKind, MIRConstant, MIRDiagnosticLocation,
    MIRFunctionDefinition, MIRInstrKind, MIRPlace, MIRTypeID, MIRValueAggregateOp,
    ty::interface::MTRegistry,
};

use crate::{
    aggregate,
    engine::MIRComptimeEngine,
    error::{self, MIRComptimeError},
    execution::{self, Step},
    frame::ExecutionFrame,
    scalar,
    value::RuntimeValue,
};

pub(crate) fn execute(
    engine: &MIRComptimeEngine<'_>,
    frame: &mut ExecutionFrame,
    definition: &MIRFunctionDefinition,
    instruction: &MIRInstrKind,
    return_type: MIRTypeID,
    location: &MIRDiagnosticLocation,
) -> Result<Step, MIRComptimeError> {
    match instruction {
        MIRInstrKind::ScopeEnter { .. } | MIRInstrKind::ScopeExit { .. } => Ok(Step::Continue),
        MIRInstrKind::Initialize { place } => {
            let ty = definition
                .place(match place {
                    MIRPlace::FunctionLocal(place) => *place,
                    _ => {
                        return Err(engine.error(
                            "only function-local places can be initialized during comptime",
                            location.clone(),
                        ));
                    }
                })
                .ok_or_else(|| engine.error("invalid initialized place", location.clone()))?
                .ty;
            let value = crate::value::zero(engine.unit(), ty)
                .map_err(|message| engine.error(message, location.clone()))?;
            frame
                .set_place(*place, RuntimeValue::Constant(value))
                .map_err(|message| engine.error(message, location.clone()))?;
            Ok(Step::Continue)
        }
        MIRInstrKind::Create { out, ty } => {
            let value = crate::value::zero(engine.unit(), *ty)
                .map_err(|message| engine.error(message, location.clone()))?;
            frame
                .set_place(*out, RuntimeValue::Constant(value))
                .map_err(|message| engine.error(message, location.clone()))?;
            Ok(Step::Continue)
        }
        MIRInstrKind::Leak { .. } => Ok(Step::Continue),
        MIRInstrKind::Assign { target, value, ty } => {
            let value = engine.eval_value(frame, value, Some(*ty), location)?;
            match target {
                MIRAssignTarget::Register(register) => frame.set_register(*register, value),
                MIRAssignTarget::Place(place) => frame
                    .set_place(*place, value)
                    .map_err(|message| engine.error(message, location.clone()))?,
            }
            Ok(Step::Continue)
        }
        MIRInstrKind::AddressOf { out, place } => {
            let ty = definition
                .register(*out)
                .ok_or_else(|| {
                    engine.error("invalid address-of result register", location.clone())
                })?
                .ty;
            frame.set_register(*out, engine.address(*place, ty, location)?);
            Ok(Step::Continue)
        }
        MIRInstrKind::Dereference { .. } => Err(engine.error(
            "dereference is not supported during MIR comptime evaluation",
            location.clone(),
        )),
        MIRInstrKind::AggregateOp(operation) => {
            aggregate_operation(engine, frame, definition, operation, location)
        }
        MIRInstrKind::Call {
            out,
            kind,
            callee,
            args,
        } => {
            if *kind != MIRCallKind::Comptime {
                return Err(engine.error(
                    "runtime calls are not supported during MIR comptime evaluation",
                    location.clone(),
                ));
            }
            let callee = engine.eval_value(frame, callee, None, location)?;
            let callee = engine.constant(frame, callee, None, location)?;
            let MIRConstant::Function(callee) = callee else {
                return Err(engine.error(
                    "comptime call target is not a function reference",
                    location.clone(),
                ));
            };
            let arguments = args
                .iter()
                .map(|argument| {
                    engine
                        .eval_value(frame, argument, None, location)
                        .and_then(|value| engine.constant(frame, value, None, location))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let value = engine
                .evaluate(callee, &arguments)
                .map_err(|error| error::with_frame(error, location.clone()))?;
            if let Some(out) = out {
                frame.set_register(*out, RuntimeValue::Constant(value));
            }
            Ok(Step::Continue)
        }
        MIRInstrKind::VaStart { .. } | MIRInstrKind::VaEnd { .. } | MIRInstrKind::VaArg { .. } => {
            Err(engine.error(
                "variadic operations are not supported during MIR comptime evaluation",
                location.clone(),
            ))
        }
        MIRInstrKind::BinOp { out, op, lhs, rhs } => {
            let result_type = definition
                .register(*out)
                .ok_or_else(|| {
                    engine.error("invalid binary operation result register", location.clone())
                })?
                .ty;
            let lhs = engine.eval_value(
                frame,
                lhs,
                if matches!(
                    op,
                    cx_mir::MIRBinaryOp::PointerOffset { .. } | cx_mir::MIRBinaryOp::Pointer(_)
                ) {
                    Some(result_type)
                } else {
                    None
                },
                location,
            )?;
            let rhs = engine.eval_value(frame, rhs, None, location)?;
            let lhs = engine.constant(frame, lhs, Some(result_type), location)?;
            let rhs = engine.constant(frame, rhs, None, location)?;
            let value = scalar::binary(engine.unit(), op, &lhs, &rhs, result_type)
                .map_err(|message| engine.error(message, location.clone()))?;
            frame.set_register(*out, RuntimeValue::Constant(value));
            Ok(Step::Continue)
        }
        MIRInstrKind::UnOp { out, op, operand } => {
            let operand = engine.eval_value(frame, operand, None, location)?;
            let operand = engine.constant(frame, operand, None, location)?;
            let value = scalar::unary(op, &operand)
                .map_err(|message| engine.error(message, location.clone()))?;
            frame.set_register(*out, RuntimeValue::Constant(value));
            Ok(Step::Continue)
        }
        MIRInstrKind::Coerce {
            out,
            operand,
            coercion,
            to_type,
        } => {
            let operand = engine.eval_value(frame, operand, Some(*to_type), location)?;
            let operand = match (&operand, coercion) {
                (
                    RuntimeValue::Place(MIRPlace::Global(global)),
                    cx_mir::MIRCoercion::ReinterpretBits,
                ) if matches!(
                    engine.unit().types().kind(*to_type),
                    Ok(cx_mir::MIRTypeKind::Array { .. })
                ) =>
                {
                    aggregate::string_literal(engine.unit(), *global, *to_type)
                        .map_err(|message| engine.error(message, location.clone()))?
                }
                _ => engine.constant(frame, operand, Some(*to_type), location)?,
            };
            let value = scalar::coerce(engine.unit(), *coercion, &operand, *to_type)
                .map_err(|message| engine.error(message, location.clone()))?;
            frame.set_register(*out, RuntimeValue::Constant(value));
            Ok(Step::Continue)
        }
        MIRInstrKind::Assert { condition, message } => {
            let condition = engine.eval_value(frame, condition, None, location)?;
            let condition = engine.constant(frame, condition, None, location)?;
            if !engine
                .condition(&condition)
                .map_err(|message| engine.error(message, location.clone()))?
            {
                return Err(engine.error(
                    message.as_deref().unwrap_or("comptime assertion failed"),
                    location.clone(),
                ));
            }
            Ok(Step::Continue)
        }
        MIRInstrKind::Assume { condition } => {
            let condition = engine.eval_value(frame, condition, None, location)?;
            let condition = engine.constant(frame, condition, None, location)?;
            if !engine
                .condition(&condition)
                .map_err(|message| engine.error(message, location.clone()))?
            {
                return Err(engine.error("comptime assumption is false", location.clone()));
            }
            Ok(Step::Continue)
        }
        MIRInstrKind::Return { value } => {
            let value = match value {
                Some(value) => {
                    let value = engine.eval_value(frame, value, Some(return_type), location)?;
                    engine.constant(frame, value, Some(return_type), location)?
                }
                None => MIRConstant::Unit,
            };
            Ok(Step::Return(value))
        }
        MIRInstrKind::Jump { target } => Ok(Step::Jump(execution::bind_target(
            engine, frame, definition, target, location,
        )?)),
        MIRInstrKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            let cond = engine.eval_value(frame, cond, None, location)?;
            let cond = engine.constant(frame, cond, None, location)?;
            let target = crate::control_flow::branch(&cond, true_target, false_target)
                .map_err(|message| engine.error(message, location.clone()))?;
            Ok(Step::Jump(execution::bind_target(
                engine, frame, definition, target, location,
            )?))
        }
        MIRInstrKind::IntSwitch { value, .. } => {
            let value = engine.eval_value(frame, value, None, location)?;
            let value = engine.constant(frame, value, None, location)?;
            let target = crate::control_flow::switch(instruction, &value)
                .map_err(|message| engine.error(message, location.clone()))?
                .ok_or_else(|| {
                    engine.error("integer switch has no matching target", location.clone())
                })?;
            Ok(Step::Jump(execution::bind_target(
                engine, frame, definition, target, location,
            )?))
        }
        MIRInstrKind::VariantSwitch {
            subject,
            cases,
            default,
            ..
        } => {
            let subject = engine.eval_value(frame, subject, None, location)?;
            let subject = engine.constant(frame, subject, None, location)?;
            let variant = match subject {
                MIRConstant::Aggregate { fields, .. } => fields.first().map(|(field, _)| *field),
                _ => None,
            };
            let target = variant
                .and_then(|variant| {
                    cases
                        .iter()
                        .find(|(case, _)| *case == variant)
                        .map(|(_, target)| target)
                })
                .or(default.as_ref())
                .ok_or_else(|| {
                    engine.error("variant switch has no matching target", location.clone())
                })?;
            Ok(Step::Jump(execution::bind_target(
                engine, frame, definition, target, location,
            )?))
        }
        MIRInstrKind::Unreachable => Err(engine.error(
            "reached unreachable during comptime evaluation",
            location.clone(),
        )),
        MIRInstrKind::Emit { .. } => Err(engine.error(
            "emit is not supported during MIR comptime evaluation",
            location.clone(),
        )),
    }
}

fn aggregate_operation(
    engine: &MIRComptimeEngine<'_>,
    frame: &mut ExecutionFrame,
    definition: &MIRFunctionDefinition,
    operation: &MIRAggregateOp,
    location: &MIRDiagnosticLocation,
) -> Result<Step, MIRComptimeError> {
    let MIRAggregateOp::Value { out, op } = operation else {
        return Err(engine.error(
            "place aggregate operations are not supported during MIR comptime evaluation",
            location.clone(),
        ));
    };
    let value = match op {
        MIRValueAggregateOp::Construct { ty, fields } => {
            let fields = fields
                .iter()
                .map(|(field, value)| {
                    let ty = aggregate::field_type(engine.unit(), *ty, *field)
                        .map_err(|message| engine.error(message, location.clone()))?;
                    let value = engine.eval_value(frame, value, Some(ty), location)?;
                    let value = engine.constant(frame, value, Some(ty), location)?;
                    Ok((*field, value))
                })
                .collect::<Result<Vec<_>, MIRComptimeError>>()?;
            aggregate::construct(engine.unit(), *ty, fields)
                .map_err(|message| engine.error(message, location.clone()))?
        }
        MIRValueAggregateOp::Variant {
            variant,
            value,
            sum_type,
        } => {
            let ty = aggregate::field_type(engine.unit(), *sum_type, *variant)
                .map_err(|message| engine.error(message, location.clone()))?;
            let value = engine.eval_value(frame, value, Some(ty), location)?;
            let value = engine.constant(frame, value, Some(ty), location)?;
            MIRConstant::Aggregate {
                ty: *sum_type,
                fields: vec![(*variant, value)],
            }
        }
        MIRValueAggregateOp::ProjectVariant { variant, value, .. } => {
            let value = engine.eval_value(frame, value, None, location)?;
            let value = engine.constant(frame, value, None, location)?;
            let MIRConstant::Aggregate { fields, .. } = value else {
                return Err(
                    engine.error("variant projection requires an aggregate", location.clone())
                );
            };
            fields
                .into_iter()
                .find_map(|(field, value)| (field == *variant).then_some(value))
                .ok_or_else(|| {
                    engine.error("requested variant is not initialized", location.clone())
                })?
        }
        MIRValueAggregateOp::Discriminant { value, .. } => {
            let value = engine.eval_value(frame, value, None, location)?;
            let value = engine.constant(frame, value, None, location)?;
            let MIRConstant::Aggregate { fields, .. } = value else {
                return Err(engine.error("discriminant requires an aggregate", location.clone()));
            };
            let discriminant = fields
                .first()
                .map(|(field, _)| *field)
                .ok_or_else(|| engine.error("aggregate has no active variant", location.clone()))?;
            MIRConstant::Integer {
                value: discriminant as i128,
                ty: cx_mir::MIRIntType::I32,
                signed: false,
            }
        }
    };
    if definition.register(*out).is_none() {
        return Err(engine.error("invalid aggregate result register", location.clone()));
    }
    frame.set_register(*out, RuntimeValue::Constant(value));
    Ok(Step::Continue)
}
