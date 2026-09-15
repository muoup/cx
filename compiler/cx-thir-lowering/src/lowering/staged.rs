pub(crate) mod exits;
mod remap;
use remap::Remap;

use std::collections::{HashMap, HashSet};

use crate::log::mir_error;
use cx_log::CXResult;
use cx_log::catalogue::mir as catalogue;
use cx_mir::{
    MIRBasicBlockID, MIRBlockTarget, MIRComptimeOp, MIRInstrKind, MIRRegister,
    MIRStagedCapture, MIRStagedInstrKind, MIRStagedTargets, MIRTypeKind, MIRValue,
    ty::interface::MTRegistry,
};
use cx_mir::visit::{MIRVisitRole, MIRVisitor, MIRWalk};
use cx_mir_comptime::{
    MIRComptimeValue, MIRStagedBinding, MIRStagedValue, evaluate_comptime_function,
};

use crate::builder::MIRBuilder;
use crate::lowering::control_flow::auto_cleanup;

pub(crate) fn instantiate(
    builder: &mut MIRBuilder<'_>,
    staged: &MIRStagedValue,
) -> CXResult<MIRValue> {
    instantiate_inner(
        builder,
        staged,
        MIRStagedTargets::default(),
        &mut HashSet::new(),
    )
}

fn instantiate_inner(
    builder: &mut MIRBuilder<'_>,
    staged: &MIRStagedValue,
    targets: MIRStagedTargets,
    used_targets: &mut HashSet<MIRBasicBlockID>,
) -> CXResult<MIRValue> {
    let range = builder.source_range().clone();
    if let Some(origin) = staged.runtime_origin()
        && origin != builder.fun().id()
    {
        return Err(mir_error(
            &range,
            (&catalogue::RUNTIME_CAPTURE_ESCAPE, ()),
        ));
    }

    let template = staged.template();
    if template.captures().len() != staged.captures().len()
        || template.params().len() != staged.args().len()
    {
        return Err(mir_error(
            &range,
            (
                &catalogue::ENTITY_REQUIREMENT,
                (
                    "staged value bindings".into(),
                    format!("{} captures and {} parameters", template.captures().len(), template.params().len()),
                    Some(format!("{} captures and {} parameters", staged.captures().len(), staged.args().len())),
                ),
            ),
        ));
    }

    let mut values = HashMap::new();
    let mut staged_inputs = HashMap::new();
    let mut places = HashMap::new();
    for (input, binding) in template.captures().iter().zip(staged.captures()) {
        match input {
            MIRStagedCapture::Register(input) => {
                bind_input(*input, binding, &mut values, &mut staged_inputs)?;
            }
            MIRStagedCapture::Place(input) => match binding {
                MIRStagedBinding::Value(
                    MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place),
                ) => {
                    places.insert(*input, *place);
                }
                _ => {
                    return Err(mir_error(
                        &range,
                        (
                            &catalogue::ENTITY_REQUIREMENT,
                            ("staged capture".into(), "a place reference".into(), None),
                        ),
                    ));
                }
            },
        }
    }
    for (input, binding) in template.params().iter().zip(staged.args()) {
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

    let mut omitted_places = HashSet::new();
    for place in body.places() {
        if places.contains_key(&place.id) {
            continue;
        }
        if matches!(builder.types().kind(place.ty), Ok(MIRTypeKind::Void)) {
            omitted_places.insert(place.id);
            continue;
        }
        let scope = scopes
            .get(&place.scope)
            .copied()
            .ok_or_else(|| {
                mir_error(
                    &range,
                    (&catalogue::MISSING_ENTITY, ("template scope".into(), "staged template".into())),
                )
            })?;
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
            let declaration = body.register(*source_param).ok_or_else(|| {
                mir_error(
                    &range,
                    (
                        &catalogue::MISSING_ENTITY,
                        ("template block parameter declaration".into(), "staged template".into()),
                    ),
                )
            })?;
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

    let continuation = blocks[&template.result_block()];
    let result = builder
        .fun()
        .body()
        .block(continuation)
        .and_then(|block| block.params.first())
        .copied();

    let entry = blocks
        .get(&body.entry())
        .copied()
        .ok_or_else(|| mir_error(&range, (&catalogue::MISSING_ENTITY, ("entry block".into(), "staged template".into()))))?;
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(entry),
    });

    let mut pending = vec![body.entry()];
    let mut visited = HashSet::new();
    while let Some(source_block) = pending.pop() {
        if source_block == template.result_block() || !visited.insert(source_block) {
            continue;
        }
        let block = body
            .block(source_block)
            .ok_or_else(|| mir_error(&range, (&catalogue::MISSING_ENTITY, ("staged block".into(), "staged template".into()))))?;
        let mapped_block = blocks[&block.id];
        builder.fun_mut().set_current_block(mapped_block);
        for instruction in &block.instrs {
            let range = &instruction.token_range;
            let deferred_callee = match &instruction.kind {
                MIRStagedInstrKind::Comptime(MIRComptimeOp::ApplyStaged {
                    staged: MIRValue::Register(register),
                    ..
                }) => Some(*register),
                _ => None,
            };
            let dependency_targets = match &instruction.kind {
                MIRStagedInstrKind::Comptime(MIRComptimeOp::ApplyStaged { targets: local, .. })
                | MIRStagedInstrKind::Use { targets: local, .. } => {
                    map_targets(*local, targets, &blocks, range)?
                }
                _ => targets,
            };
            if !matches!(
                instruction.kind,
                MIRStagedInstrKind::Comptime(MIRComptimeOp::MakeStaged { .. })
                    | MIRStagedInstrKind::Comptime(MIRComptimeOp::Call { .. })
            ) {
                resolve_dependencies(
                    builder,
                    &instruction.kind,
                    &mut values,
                    &mut staged_inputs,
                    deferred_callee,
                    dependency_targets,
                    used_targets,
                )?;
            }
            if builder.fun().current_block_terminated() {
                break;
            }
            let remap = Remap {
                registers: &values,
                places: &places,
                omitted_places: &omitted_places,
                blocks: &blocks,
                block_params: &block_params,
                scopes: &scopes,
                range,
            };
            match &instruction.kind {
                MIRStagedInstrKind::ScopeExit { kind } => {
                    let local_target = match kind {
                        cx_mir::MIRStagedExitKind::Break => targets.break_target,
                        cx_mir::MIRStagedExitKind::Continue => targets.continue_target,
                        cx_mir::MIRStagedExitKind::Expr => Some(continuation),
                    };
                    if matches!(kind, cx_mir::MIRStagedExitKind::Expr) && result.is_some() {
                        return Err(mir_error(
                            &range,
                            (&catalogue::REQUIRED_CONTEXT, ("staged expression exit".into(), "a staged return value".into())),
                        ));
                    }
                    let block = if let Some(block) = local_target {
                        block
                    } else if let Some((scope, block)) = builder.fun().exit_target(*kind) {
                        auto_cleanup(builder, scope)?;
                        block
                    } else {
                        let name = match kind {
                            cx_mir::MIRStagedExitKind::Break => "break",
                            cx_mir::MIRStagedExitKind::Continue => "continue",
                            cx_mir::MIRStagedExitKind::Expr => "expression",
                        };
                        return Err(mir_error(
                            &range,
                            (&catalogue::MISSING_ENTITY, (format!("{name} target"), "staged materialization context".into())),
                        ));
                    };
                    used_targets.insert(block);
                    builder.fun_mut().emit(
                        MIRInstrKind::Jump {
                            target: MIRBlockTarget::new(block),
                        },
                        instruction.token_range.clone(),
                    );
                }
                MIRStagedInstrKind::Yield { value, ty } => {
                    let block = if let Some(block) = targets.yield_target {
                        block
                    } else if let Some((scope, block)) = builder
                        .fun()
                        .scope_stack()
                        .iter()
                        .rev()
                        .find_map(|scope| scope.yield_target.map(|block| (scope.id(), block)))
                    {
                        auto_cleanup(builder, scope)?;
                        block
                    } else {
                        return Err(mir_error(&range, (&catalogue::MISSING_ENTITY, ("yield target".into(), "staged materialization context".into()))));
                    };
                    let args: Vec<MIRValue> = value
                        .as_ref()
                        .map(|value| remap.value(value))
                        .transpose()?
                        .into_iter()
                        .collect();
                    validate_yield(builder, block, *ty)?;
                    used_targets.insert(block);
                    builder.fun_mut().emit(
                        MIRInstrKind::Jump {
                            target: MIRBlockTarget::with_args(block, args),
                        },
                        instruction.token_range.clone(),
                    );
                }
                MIRStagedInstrKind::Comptime(MIRComptimeOp::ApplyStaged {
                    out,
                    staged,
                    args,
                    targets: local_targets,
                }) => {
                    let MIRValue::Register(source) = staged else {
                        return Err(mir_error(&range, (&catalogue::ENTITY_REQUIREMENT, ("staged callee".into(), "a template input".into(), None))));
                    };
                    let dependency = staged_inputs.get(source).cloned().ok_or_else(|| {
                        mir_error(&range, (&catalogue::MISSING_ENTITY, ("staged dependency".into(), "staged callee".into())))
                    })?;
                    let args = args
                        .iter()
                        .map(|arg| remap.value(arg).map(MIRStagedBinding::Value))
                        .collect::<CXResult<Vec<_>>>()?;
                    let applied = dependency.apply(args);
                    let targets = map_targets(*local_targets, targets, &blocks, range)?;
                    let value = instantiate_inner(builder, &applied, targets, used_targets)?;
                    if let Some(out) = out {
                        values.insert(*out, value);
                    }
                }
                MIRStagedInstrKind::Comptime(MIRComptimeOp::MakeStaged {
                    out,
                    template,
                    captures,
                }) => {
                    let captures = captures
                        .iter()
                        .map(|value| {
                            if let MIRValue::Register(register) = value
                                && let Some(staged) = staged_inputs.get(register)
                            {
                                return Ok(MIRStagedBinding::Comptime(MIRComptimeValue::Staged(
                                    staged.clone(),
                                )));
                            }
                            remap.value(value).map(MIRStagedBinding::Value)
                        })
                        .collect::<CXResult<Vec<_>>>()?;
                    let value = MIRStagedValue::new(
                        template.clone(),
                        captures,
                        Vec::new(),
                        Some(builder.fun().id()),
                    );
                    staged_inputs.insert(*out, std::sync::Arc::new(value));
                    values.remove(out);
                }
                MIRStagedInstrKind::Comptime(MIRComptimeOp::Call {
                    out,
                    callee,
                    args,
                }) => {
                    let MIRValue::Constant(cx_mir::MIRConstant::Function(function)) =
                        remap.value(callee)?
                    else {
                        return Err(mir_error(
                            range,
                            (&catalogue::MISSING_ENTITY, ("comptime callee binding".into(), "staged template".into())),
                        ));
                    };
                    let args = args
                        .iter()
                        .map(|value| {
                            if let MIRValue::Register(register) = value
                                && let Some(staged) = staged_inputs.get(register)
                            {
                                return Ok(MIRComptimeValue::Staged(staged.clone()));
                            }
                            match remap.value(value)? {
                                MIRValue::Constant(value) => Ok(MIRComptimeValue::Constant(value)),
                                _ => Err(mir_error(
                                    range,
                                    (&catalogue::ENTITY_REQUIREMENT, ("comptime argument".into(), "a compile-time value".into(), Some("runtime value".into()))),
                                )),
                            }
                        })
                        .collect::<CXResult<Vec<_>>>()?;
                    let function = builder.module().function(function).ok_or_else(|| {
                        mir_error(range, (&catalogue::MISSING_ENTITY, ("comptime function definition".into(), "MIR module".into())))
                    })?;

                    let value = evaluate_comptime_function(builder, function, &args)?;
                    if let Some(out) = out {
                        match value {
                            MIRComptimeValue::Constant(value) => {
                                values.insert(*out, MIRValue::Constant(value));
                            }

                            MIRComptimeValue::Staged(value) => {
                                staged_inputs.insert(*out, value);
                                values.remove(&out);
                            }
                        }
                    }
                }
                MIRStagedInstrKind::Move { out, value } => {
                    let mapped = remap.value(value)?;
                    let mapped = match mapped {
                        MIRValue::PlaceRef(place)
                        | MIRValue::Copy(place)
                        | MIRValue::Move(place) => MIRValue::Move(place),
                        value => value,
                    };
                    values.insert(*out, mapped);
                }
                MIRStagedInstrKind::Use { .. } => {}
                MIRStagedInstrKind::CallerReturn { value } => {
                    let value = value.as_ref().map(|value| remap.value(value)).transpose()?;
                    if let Some(block) = targets.return_target {
                        used_targets.insert(block);
                        builder.fun_mut().emit(
                            MIRInstrKind::Jump {
                                target: MIRBlockTarget::with_args(
                                    block,
                                    value.into_iter().collect(),
                                ),
                            },
                            instruction.token_range.clone(),
                        );
                    } else {
                        auto_cleanup(builder, builder.fun().scope_stack().first().unwrap().id())?;
                        builder.fun_mut().emit(
                            MIRInstrKind::Return { value },
                            instruction.token_range.clone(),
                        );
                    }
                }
                MIRStagedInstrKind::Standard(kind) => {
                    if remap.omitted(kind) {
                        continue;
                    }
                    let mapped = remap.instruction(kind)?;
                    builder
                        .fun_mut()
                        .emit(mapped, instruction.token_range.clone());
                }
            }
            pending.extend(instruction.successors());
        }
        pending.extend(body.blocks().iter().filter_map(|block| {
            (used_targets.contains(&blocks[&block.id]) && !visited.contains(&block.id))
                .then_some(block.id)
        }));
    }
    for (source, mapped) in &blocks {
        if *source != template.result_block() && !visited.contains(source) {
            builder.fun_mut().set_current_block(*mapped);
            builder.emit(MIRInstrKind::Unreachable);
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
    instruction: &MIRStagedInstrKind,
    values: &mut HashMap<MIRRegister, MIRValue>,
    staged_inputs: &mut HashMap<MIRRegister, std::sync::Arc<MIRStagedValue>>,
    deferred: Option<MIRRegister>,
    targets: MIRStagedTargets,
    used_targets: &mut HashSet<MIRBasicBlockID>,
) -> CXResult<()> {
    let range = builder.source_range().clone();
    struct DependencyVisitor<'a> {
        staged_inputs: &'a HashMap<MIRRegister, std::sync::Arc<MIRStagedValue>>,
        deferred: Option<MIRRegister>,
        inputs: Vec<MIRRegister>,
    }
    impl MIRVisitor<'_> for DependencyVisitor<'_> {
        type Error = std::convert::Infallible;

        fn register(&mut self, register: &MIRRegister, role: MIRVisitRole) -> Result<(), Self::Error> {
            if matches!(role, MIRVisitRole::Read | MIRVisitRole::Copy | MIRVisitRole::Move)
                && Some(*register) != self.deferred
                && self.staged_inputs.contains_key(register)
                && !self.inputs.contains(register)
            {
                self.inputs.push(*register);
            }
            Ok(())
        }
    }

    let mut visitor = DependencyVisitor {
        staged_inputs,
        deferred,
        inputs: Vec::new(),
    };
    instruction.visit(&mut visitor).expect("dependency visitor is infallible");
    let inputs = visitor.inputs;

    for input in inputs {
        let staged = staged_inputs
            .remove(&input)
            .expect("collected staged input exists");
        if !staged.template().params().is_empty() {
            return Err(mir_error(
                &range,
                (&catalogue::REQUIRED_CONTEXT, ("parameterized staged values".into(), "an application".into())),
            ));
        }
        let value = instantiate_inner(builder, &staged, targets, used_targets)?;
        values.insert(input, value);
    }
    Ok(())
}

fn map_targets(
    local: MIRStagedTargets,
    inherited: MIRStagedTargets,
    blocks: &HashMap<MIRBasicBlockID, MIRBasicBlockID>,
    range: &cx_tokens::TokenRange,
) -> CXResult<MIRStagedTargets> {
    let map = |target: Option<MIRBasicBlockID>| {
        target
            .map(|target| {
                blocks
                    .get(&target)
                    .copied()
                    .ok_or_else(|| mir_error(&range, (&catalogue::MISSING_ENTITY, ("staged target block".into(), "staged materialization context".into()))))
            })
            .transpose()
    };
    Ok(MIRStagedTargets {
        return_target: map(local.return_target)?.or(inherited.return_target),
        break_target: map(local.break_target)?.or(inherited.break_target),
        continue_target: map(local.continue_target)?.or(inherited.continue_target),
        yield_target: map(local.yield_target)?.or(inherited.yield_target),
    })
}

fn validate_yield(
    builder: &MIRBuilder<'_>,
    target: MIRBasicBlockID,
    actual: Option<cx_mir::MIRTypeID>,
) -> CXResult<()> {
    let range = builder.source_range();
    let expected = builder
        .fun()
        .body()
        .block(target)
        .and_then(|block| block.params.first())
        .and_then(|register| builder.fun().register_type(*register));
    if expected.is_some() != actual.is_some() {
        return Err(mir_error(&range, (&catalogue::ENTITY_MISMATCH, ("staged yield value".into(), "materialization context".into()))));
    }
    if let (Some(expected), Some(actual)) = (expected, actual)
        && !builder.types().same_type(expected, actual)
    {
        return Err(mir_error(&range, (&catalogue::ENTITY_MISMATCH, ("staged yield type".into(), "yield target type".into()))));
    }
    Ok(())
}
