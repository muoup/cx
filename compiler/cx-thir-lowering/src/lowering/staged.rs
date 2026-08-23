use std::collections::{HashMap, HashSet};

use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBasicBlockID, MIRBlockTarget, MIRInstr, MIRInstrKind,
    MIRPlace, MIRPlaceAggregateOp, MIRPlaceID, MIRRegister, MIRScopeID, MIRTypeKind, MIRValue,
    MIRValueAggregateOp, ty::interface::MTRegistry,
};
use cx_mir_comptime::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue};

use crate::builder::MIRBuilder;

fn staged_error(message: impl Into<String>) -> CXErr {
    CXErr::new(
        CXStdErrMessage::error("COMPTIME ERROR", message.into()),
        CXInternalContext::error("failed to instantiate a staged MIR template"),
    )
}

pub(crate) fn instantiate(
    builder: &mut MIRBuilder<'_>,
    staged: &MIRStagedValue,
) -> CXResult<MIRValue> {
    instantiate_inner(builder, staged, &[])
}

fn instantiate_inner(
    builder: &mut MIRBuilder<'_>,
    staged: &MIRStagedValue,
    return_prefix: &[MIRInstr],
) -> CXResult<MIRValue> {
    if let Some(origin) = staged.runtime_origin()
        && origin != builder.fun().id()
    {
        return Err(staged_error(
            "a staged value with runtime captures escaped its originating function",
        ));
    }

    let template = staged.template();
    if template.captures().len() != staged.captures().len()
        || template.params().len() != staged.args().len()
    {
        return Err(staged_error(
            "staged value binding count does not match its template",
        ));
    }

    let mut values = HashMap::new();
    let mut staged_inputs = HashMap::new();
    for (input, binding) in template
        .captures()
        .iter()
        .zip(staged.captures())
        .chain(template.params().iter().zip(staged.args()))
    {
        bind_input(*input, binding, &mut values, &mut staged_inputs)?;
    }

    let body = template.body();
    let mut scopes = HashMap::new();
    for scope in body.scopes() {
        let mapped = builder
            .fun_mut()
            .body_mut()
            .add_scope(scope.token_range.clone());
        scopes.insert(scope.id, mapped);
    }

    let mut places = HashMap::new();
    let mut omitted_places = HashSet::new();
    for place in body.places() {
        if matches!(builder.types().kind(place.ty), Ok(MIRTypeKind::Void)) {
            omitted_places.insert(place.id);
            continue;
        }
        let scope = scopes
            .get(&place.scope)
            .copied()
            .ok_or_else(|| staged_error("template place refers to an unknown scope"))?;
        let mapped = builder.fun_mut().body_mut().add_place(
            place.ty,
            place.debug_name.clone(),
            place.nodrop,
            scope,
        );
        places.insert(place.id, mapped);
    }

    for register in body.registers() {
        if values.contains_key(&register.id) || staged_inputs.contains_key(&register.id) {
            continue;
        }
        if matches!(builder.types().kind(register.ty), Ok(MIRTypeKind::Void)) {
            values.insert(register.id, MIRValue::Constant(cx_mir::MIRConstant::Unit));
            continue;
        }
        let mapped = builder
            .fun_mut()
            .new_register(register.ty, register.debug_name.clone());
        values.insert(register.id, MIRValue::Register(mapped));
    }

    let mut blocks = HashMap::new();
    let mut block_params = HashMap::new();
    for block in body.blocks() {
        let mapped = builder.fun_mut().new_block(
            block
                .debug_name
                .clone()
                .unwrap_or_else(|| format!("staged_{}", block.id.index()).into()),
        );
        let mut retained_params = Vec::with_capacity(block.params.len());
        for source_param in &block.params {
            let declaration = body
                .register(*source_param)
                .ok_or_else(|| staged_error("template block parameter has no declaration"))?;
            if matches!(builder.types().kind(declaration.ty), Ok(MIRTypeKind::Void)) {
                values.insert(*source_param, MIRValue::Constant(cx_mir::MIRConstant::Unit));
                retained_params.push(false);
                continue;
            }
            let mapped_param = builder.fun_mut().block_param(
                mapped,
                declaration.ty,
                declaration.debug_name.clone(),
            );
            values.insert(*source_param, MIRValue::Register(mapped_param));
            retained_params.push(true);
        }
        blocks.insert(block.id, mapped);
        block_params.insert(block.id, retained_params);
    }

    let is_void = matches!(
        builder.types().kind(template.result_type()),
        Ok(MIRTypeKind::Void)
    );
    let continuation = builder.fun_mut().new_block("staged_continuation");
    let result = if is_void {
        None
    } else {
        Some(
            builder
                .fun_mut()
                .block_param(continuation, template.result_type(), None),
        )
    };

    let entry = blocks
        .get(&body.entry())
        .copied()
        .ok_or_else(|| staged_error("staged template has no entry block"))?;
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(entry),
    });

    for block in body.blocks() {
        let mapped_block = blocks[&block.id];
        builder.fun_mut().set_current_block(mapped_block);
        for (instruction_index, instruction) in block.instrs.iter().enumerate() {
            let deferred_callee = match &instruction.kind {
                MIRInstrKind::ApplyStaged {
                    staged: MIRValue::Register(register),
                    ..
                } => Some(*register),
                _ => None,
            };
            resolve_dependencies(
                builder,
                &instruction.kind,
                &mut values,
                &mut staged_inputs,
                return_prefix,
                deferred_callee,
            )?;
            match &instruction.kind {
                MIRInstrKind::StagedReturn { value } => {
                    let args = if is_void {
                        Vec::new()
                    } else {
                        vec![map_value(value, &values, &places, &omitted_places)?]
                    };
                    builder.fun_mut().emit(
                        MIRInstrKind::Jump {
                            target: MIRBlockTarget::with_args(continuation, args),
                        },
                        instruction.token_range.clone(),
                    );
                }
                MIRInstrKind::ApplyStaged { out, staged, args } => {
                    let MIRValue::Register(source) = staged else {
                        return Err(staged_error("staged callee is not a template input"));
                    };
                    let dependency = staged_inputs
                        .get(source)
                        .cloned()
                        .ok_or_else(|| staged_error("staged callee has no dependency binding"))?;
                    let args = args
                        .iter()
                        .map(|arg| {
                            map_value(arg, &values, &places, &omitted_places)
                                .map(MIRStagedBinding::Value)
                        })
                        .collect::<CXResult<Vec<_>>>()?;
                    let mut child_return_prefix = Vec::new();
                    for suffix in &block.instrs[instruction_index + 1..] {
                        if matches!(suffix.kind, MIRInstrKind::StagedReturn { .. }) {
                            break;
                        }
                        if matches!(
                            suffix.kind,
                            MIRInstrKind::MakeStaged { .. } | MIRInstrKind::ApplyStaged { .. }
                        ) {
                            return Err(staged_error(
                                "nested staged application in a deferred return suffix",
                            ));
                        }
                        if writes_omitted_value(&suffix.kind, &values, &omitted_places) {
                            continue;
                        }
                        child_return_prefix.push(MIRInstr::new(
                            map_instruction(
                                &suffix.kind,
                                &values,
                                &places,
                                &omitted_places,
                                &blocks,
                                &block_params,
                                &scopes,
                            )?,
                            suffix.token_range.clone(),
                        ));
                    }
                    child_return_prefix.extend_from_slice(return_prefix);
                    let applied = dependency.apply(args);
                    let value =
                        instantiate_inner(builder, &applied, &child_return_prefix)?;
                    if let Some(out) = out {
                        values.insert(*out, value);
                    }
                }
                MIRInstrKind::MakeStaged { .. } => {
                    return Err(staged_error(
                        "a staged template attempted to construct another staged value at runtime",
                    ));
                }
                MIRInstrKind::StagedMove { out, value } => {
                    let mapped = map_value(value, &values, &places, &omitted_places)?;
                    let mapped = match mapped {
                        MIRValue::PlaceRef(place)
                        | MIRValue::Copy(place)
                        | MIRValue::Move(place) => MIRValue::Move(place),
                        value => value,
                    };
                    values.insert(*out, mapped);
                }
                MIRInstrKind::StagedUse { .. } => {}
                MIRInstrKind::Return { .. } => {
                    for prefix in return_prefix {
                        builder
                            .fun_mut()
                            .emit(prefix.kind.clone(), prefix.token_range.clone());
                    }
                    let mapped = map_instruction(
                        &instruction.kind,
                        &values,
                        &places,
                        &omitted_places,
                        &blocks,
                        &block_params,
                        &scopes,
                    )?;
                    builder
                        .fun_mut()
                        .emit(mapped, instruction.token_range.clone());
                }
                kind => {
                    if writes_omitted_value(kind, &values, &omitted_places) {
                        continue;
                    }
                    let mapped = map_instruction(
                        kind,
                        &values,
                        &places,
                        &omitted_places,
                        &blocks,
                        &block_params,
                        &scopes,
                    )?;
                    builder
                        .fun_mut()
                        .emit(mapped, instruction.token_range.clone());
                }
            }
        }
    }

    builder.fun_mut().set_current_block(continuation);
    if template.diverges() {
        builder.emit(MIRInstrKind::Unreachable);
    }
    Ok(result
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(cx_mir::MIRConstant::Unit)))
}

fn bind_input(
    input: MIRRegister,
    binding: &MIRStagedBinding,
    values: &mut HashMap<MIRRegister, MIRValue>,
    staged_inputs: &mut HashMap<MIRRegister, std::sync::Arc<MIRStagedValue>>,
) -> CXResult<()> {
    match binding {
        MIRStagedBinding::Value(value) => {
            values.insert(input, value.clone());
        }
        MIRStagedBinding::Comptime(MIRComptimeValue::Constant(value)) => {
            values.insert(input, MIRValue::Constant(value.clone()));
        }
        MIRStagedBinding::Comptime(MIRComptimeValue::Staged(staged)) => {
            staged_inputs.insert(input, staged.clone());
        }
    }
    Ok(())
}

fn resolve_dependencies(
    builder: &mut MIRBuilder<'_>,
    instruction: &MIRInstrKind,
    values: &mut HashMap<MIRRegister, MIRValue>,
    staged_inputs: &mut HashMap<MIRRegister, std::sync::Arc<MIRStagedValue>>,
    return_prefix: &[MIRInstr],
    deferred: Option<MIRRegister>,
) -> CXResult<()> {
    let mut inputs = Vec::new();
    MIRInstr::new(instruction.clone(), cx_tokens::TokenRange::internal()).visit_operands(
        |operand| {
            if let Some(register) = operand.register()
                && Some(register) != deferred
                && staged_inputs.contains_key(&register)
                && !inputs.contains(&register)
            {
                inputs.push(register);
            }
        },
    );

    for input in inputs {
        let staged = staged_inputs
            .remove(&input)
            .expect("collected staged input exists");
        if !staged.template().params().is_empty() {
            return Err(staged_error(
                "parameterized staged value used without an application",
            ));
        }
        let value = instantiate_inner(builder, &staged, return_prefix)?;
        values.insert(input, value);
    }
    Ok(())
}

fn map_value(
    value: &MIRValue,
    registers: &HashMap<MIRRegister, MIRValue>,
    places: &HashMap<MIRPlaceID, MIRPlace>,
    omitted_places: &HashSet<MIRPlaceID>,
) -> CXResult<MIRValue> {
    Ok(match value {
        MIRValue::Register(register) => registers.get(register).cloned().ok_or_else(|| {
            staged_error(format!(
                "template register {register:?} has no rewrite; available rewrites: {:?}",
                registers.keys().collect::<Vec<_>>()
            ))
        })?,
        MIRValue::PlaceRef(MIRPlace::FunctionLocal(id))
        | MIRValue::Copy(MIRPlace::FunctionLocal(id))
        | MIRValue::Move(MIRPlace::FunctionLocal(id))
            if omitted_places.contains(id) =>
        {
            MIRValue::Constant(cx_mir::MIRConstant::Unit)
        }
        MIRValue::PlaceRef(place) => MIRValue::PlaceRef(map_place(*place, places)?),
        MIRValue::Copy(place) => MIRValue::Copy(map_place(*place, places)?),
        MIRValue::Move(place) => MIRValue::Move(map_place(*place, places)?),
        MIRValue::Constant(value) => MIRValue::Constant(value.clone()),
    })
}

fn map_place(place: MIRPlace, places: &HashMap<MIRPlaceID, MIRPlace>) -> CXResult<MIRPlace> {
    match place {
        MIRPlace::FunctionLocal(id) => places
            .get(&id)
            .copied()
            .ok_or_else(|| staged_error("template place has no rewrite")),
        MIRPlace::Parameter(_) => Err(staged_error(
            "staged template retained a comptime function parameter",
        )),
        MIRPlace::Global(id) => Ok(MIRPlace::Global(id)),
    }
}

fn map_target(
    target: &MIRBlockTarget,
    registers: &HashMap<MIRRegister, MIRValue>,
    places: &HashMap<MIRPlaceID, MIRPlace>,
    omitted_places: &HashSet<MIRPlaceID>,
    blocks: &HashMap<MIRBasicBlockID, MIRBasicBlockID>,
    block_params: &HashMap<MIRBasicBlockID, Vec<bool>>,
) -> CXResult<MIRBlockTarget> {
    Ok(MIRBlockTarget::with_args(
        *blocks
            .get(&target.block)
            .ok_or_else(|| staged_error("template block target has no rewrite"))?,
        target
            .args
            .iter()
            .enumerate()
            .filter(|(index, _)| {
                block_params
                    .get(&target.block)
                    .and_then(|params| params.get(*index))
                    .copied()
                    .unwrap_or(false)
            })
            .map(|(_, value)| map_value(value, registers, places, omitted_places))
            .collect::<CXResult<Vec<_>>>()?,
    ))
}

fn mapped_register(
    register: MIRRegister,
    registers: &HashMap<MIRRegister, MIRValue>,
) -> CXResult<MIRRegister> {
    match registers.get(&register) {
        Some(MIRValue::Register(register)) => Ok(*register),
        _ => Err(staged_error(
            "instruction output register has no concrete rewrite",
        )),
    }
}

fn mapped_optional_register(
    register: MIRRegister,
    registers: &HashMap<MIRRegister, MIRValue>,
) -> CXResult<Option<MIRRegister>> {
    match registers.get(&register) {
        Some(MIRValue::Register(register)) => Ok(Some(*register)),
        Some(MIRValue::Constant(cx_mir::MIRConstant::Unit)) => Ok(None),
        _ => Err(staged_error(
            "instruction output register has no concrete rewrite",
        )),
    }
}

fn omitted_place(place: MIRPlace, omitted_places: &HashSet<MIRPlaceID>) -> bool {
    matches!(place, MIRPlace::FunctionLocal(id) if omitted_places.contains(&id))
}

fn omitted_register(register: MIRRegister, registers: &HashMap<MIRRegister, MIRValue>) -> bool {
    matches!(
        registers.get(&register),
        Some(MIRValue::Constant(cx_mir::MIRConstant::Unit))
    )
}

fn writes_omitted_value(
    kind: &MIRInstrKind,
    registers: &HashMap<MIRRegister, MIRValue>,
    omitted_places: &HashSet<MIRPlaceID>,
) -> bool {
    match kind {
        MIRInstrKind::Initialize { place }
        | MIRInstrKind::Leak { place }
        | MIRInstrKind::Create { out: place, .. }
        | MIRInstrKind::Dereference { out: place, .. } => omitted_place(*place, omitted_places),
        MIRInstrKind::Assign { target, .. } => match target {
            MIRAssignTarget::Place(place) => omitted_place(*place, omitted_places),
            MIRAssignTarget::Register(register) => omitted_register(*register, registers),
        },
        MIRInstrKind::AddressOf { out, .. }
        | MIRInstrKind::VaArg { out, .. }
        | MIRInstrKind::BinOp { out, .. }
        | MIRInstrKind::UnOp { out, .. }
        | MIRInstrKind::Coerce { out, .. } => omitted_register(*out, registers),
        MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, .. }) => {
            omitted_place(*out, omitted_places)
        }
        MIRInstrKind::AggregateOp(MIRAggregateOp::Value { out, .. }) => {
            omitted_register(*out, registers)
        }
        _ => false,
    }
}

fn map_instruction(
    kind: &MIRInstrKind,
    registers: &HashMap<MIRRegister, MIRValue>,
    places: &HashMap<MIRPlaceID, MIRPlace>,
    omitted_places: &HashSet<MIRPlaceID>,
    blocks: &HashMap<MIRBasicBlockID, MIRBasicBlockID>,
    block_params: &HashMap<MIRBasicBlockID, Vec<bool>>,
    scopes: &HashMap<MIRScopeID, MIRScopeID>,
) -> CXResult<MIRInstrKind> {
    let value = |value| map_value(value, registers, places, omitted_places);
    let place = |place| map_place(place, places);
    let register = |register| mapped_register(register, registers);
    let target = |target| {
        map_target(
            target,
            registers,
            places,
            omitted_places,
            blocks,
            block_params,
        )
    };
    Ok(match kind {
        MIRInstrKind::ScopeEnter { scope } => MIRInstrKind::ScopeEnter {
            scope: scopes[scope],
        },
        MIRInstrKind::ScopeExit { scope } => MIRInstrKind::ScopeExit {
            scope: scopes[scope],
        },
        MIRInstrKind::Initialize { place: output } => MIRInstrKind::Initialize {
            place: place(*output)?,
        },
        MIRInstrKind::Leak { place: output } => MIRInstrKind::Leak {
            place: place(*output)?,
        },
        MIRInstrKind::Create { out, ty } => MIRInstrKind::Create {
            out: place(*out)?,
            ty: *ty,
        },
        MIRInstrKind::Assign {
            target: output,
            value: input,
            ty,
        } => MIRInstrKind::Assign {
            target: match output {
                MIRAssignTarget::Place(output) => MIRAssignTarget::Place(place(*output)?),
                MIRAssignTarget::Register(output) => MIRAssignTarget::Register(register(*output)?),
            },
            value: value(input)?,
            ty: *ty,
        },
        MIRInstrKind::AddressOf { out, place: input } => MIRInstrKind::AddressOf {
            out: register(*out)?,
            place: place(*input)?,
        },
        MIRInstrKind::Dereference {
            out,
            pointer,
            pointee_type,
        } => MIRInstrKind::Dereference {
            out: place(*out)?,
            pointer: value(pointer)?,
            pointee_type: *pointee_type,
        },
        MIRInstrKind::AggregateOp(operation) => MIRInstrKind::AggregateOp(match operation {
            MIRAggregateOp::Place { out, op } => MIRAggregateOp::Place {
                out: place(*out)?,
                op: match op {
                    MIRPlaceAggregateOp::Field {
                        base,
                        field,
                        aggregate_type,
                    } => MIRPlaceAggregateOp::Field {
                        base: place(*base)?,
                        field: *field,
                        aggregate_type: *aggregate_type,
                    },
                    MIRPlaceAggregateOp::Index {
                        base,
                        index,
                        element_type,
                    } => MIRPlaceAggregateOp::Index {
                        base: place(*base)?,
                        index: value(index)?,
                        element_type: *element_type,
                    },
                    MIRPlaceAggregateOp::Variant {
                        base,
                        variant,
                        sum_type,
                    } => MIRPlaceAggregateOp::Variant {
                        base: place(*base)?,
                        variant: *variant,
                        sum_type: *sum_type,
                    },
                },
            },
            MIRAggregateOp::Value { out, op } => MIRAggregateOp::Value {
                out: register(*out)?,
                op: match op {
                    MIRValueAggregateOp::Discriminant {
                        value: input,
                        sum_type,
                    } => MIRValueAggregateOp::Discriminant {
                        value: value(input)?,
                        sum_type: *sum_type,
                    },
                    MIRValueAggregateOp::Construct { ty, fields } => {
                        MIRValueAggregateOp::Construct {
                            ty: *ty,
                            fields: fields
                                .iter()
                                .map(|(index, field)| Ok((*index, value(field)?)))
                                .collect::<CXResult<Vec<_>>>()?,
                        }
                    }
                    MIRValueAggregateOp::Variant {
                        variant,
                        value: input,
                        sum_type,
                    } => MIRValueAggregateOp::Variant {
                        variant: *variant,
                        value: value(input)?,
                        sum_type: *sum_type,
                    },
                    MIRValueAggregateOp::ProjectVariant {
                        variant,
                        value: input,
                        sum_type,
                    } => MIRValueAggregateOp::ProjectVariant {
                        variant: *variant,
                        value: value(input)?,
                        sum_type: *sum_type,
                    },
                },
            },
        }),
        MIRInstrKind::Call {
            out,
            kind,
            callee,
            args,
        } => MIRInstrKind::Call {
            out: out
                .map(|out| mapped_optional_register(out, registers))
                .transpose()?
                .flatten(),
            kind: *kind,
            callee: value(callee)?,
            args: args.iter().map(value).collect::<CXResult<Vec<_>>>()?,
        },
        MIRInstrKind::VaStart { list, last } => MIRInstrKind::VaStart {
            list: value(list)?,
            last: value(last)?,
        },
        MIRInstrKind::VaEnd { list } => MIRInstrKind::VaEnd { list: value(list)? },
        MIRInstrKind::VaArg { out, list, ty } => MIRInstrKind::VaArg {
            out: register(*out)?,
            list: value(list)?,
            ty: *ty,
        },
        MIRInstrKind::BinOp { out, op, lhs, rhs } => MIRInstrKind::BinOp {
            out: register(*out)?,
            op: op.clone(),
            lhs: value(lhs)?,
            rhs: value(rhs)?,
        },
        MIRInstrKind::UnOp { out, op, operand } => MIRInstrKind::UnOp {
            out: register(*out)?,
            op: op.clone(),
            operand: value(operand)?,
        },
        MIRInstrKind::Coerce {
            out,
            operand,
            coercion,
            to_type,
        } => MIRInstrKind::Coerce {
            out: register(*out)?,
            operand: value(operand)?,
            coercion: coercion.clone(),
            to_type: *to_type,
        },
        MIRInstrKind::Assert { condition, message } => MIRInstrKind::Assert {
            condition: value(condition)?,
            message: message.clone(),
        },
        MIRInstrKind::Assume { condition } => MIRInstrKind::Assume {
            condition: value(condition)?,
        },
        MIRInstrKind::Return { value: returned } => MIRInstrKind::Return {
            value: returned.as_ref().map(value).transpose()?,
        },
        MIRInstrKind::Jump {
            target: destination,
        } => MIRInstrKind::Jump {
            target: target(destination)?,
        },
        MIRInstrKind::Branch {
            cond,
            true_target,
            false_target,
        } => MIRInstrKind::Branch {
            cond: value(cond)?,
            true_target: target(true_target)?,
            false_target: target(false_target)?,
        },
        MIRInstrKind::IntSwitch {
            value: subject,
            cases,
            default,
        } => MIRInstrKind::IntSwitch {
            value: value(subject)?,
            cases: cases
                .iter()
                .map(|(case, destination)| Ok((case.clone(), target(destination)?)))
                .collect::<CXResult<Vec<_>>>()?,
            default: default.as_ref().map(target).transpose()?,
        },
        MIRInstrKind::VariantSwitch {
            subject,
            sum_type,
            cases,
            default,
        } => MIRInstrKind::VariantSwitch {
            subject: value(subject)?,
            sum_type: *sum_type,
            cases: cases
                .iter()
                .map(|(case, destination)| Ok((*case, target(destination)?)))
                .collect::<CXResult<Vec<_>>>()?,
            default: default.as_ref().map(target).transpose()?,
        },
        MIRInstrKind::Unreachable => MIRInstrKind::Unreachable,
        MIRInstrKind::MakeStaged { .. }
        | MIRInstrKind::ApplyStaged { .. }
        | MIRInstrKind::StagedReturn { .. }
        | MIRInstrKind::StagedMove { .. }
        | MIRInstrKind::StagedUse { .. } => {
            return Err(staged_error("nested staged instruction was not expanded"));
        }
    })
}
