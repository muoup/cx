use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRConstant, MIRFieldLayout, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRTarget, MIRTypeID,
    MIRTypeKind, MIRValue,
    ty::{
        interface::MTRegistry,
        layout::{field_layout, layout_of},
    },
};
use cx_tokens::TokenRange;

use crate::{ComptimeContext, log::comptime_error, value::MIRComptimeValue};

use super::{MIRComptimeEngine, execution, state::PathSeg};
use crate::interpretable::ComptimeInterpretable;

pub(super) fn resolve_projection(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRTarget,
) -> (MIRTarget, Vec<PathSeg>) {
    engine
        .frames
        .last()
        .and_then(|frame| frame.derived.get(&place).cloned())
        .unwrap_or((place, Vec::new()))
}

pub(super) fn coerce_global_special(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    operand: &MIRValue,
    to_type: MIRTypeID,
) -> CXResult<Option<MIRConstant>> {
    let MIRValue::Reference(MIRTarget::Global(global_id)) = operand else {
        return Ok(None);
    };

    let Ok(target_kind) = engine.context.types().kind(to_type) else {
        return Ok(None);
    };

    let Some(global) = engine.context.global(*global_id) else {
        return Ok(None);
    };

    match &global.kind {
        MIRGlobalKind::Variable { ty, .. } => {
            let decays = matches!(
                target_kind,
                MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. }
            ) && matches!(
                engine.context.types().kind(*ty),
                Ok(MIRTypeKind::Array { .. })
            );

            if decays {
                return Ok(Some(MIRConstant::Global {
                    global: *global_id,
                    offset: 0,
                    ty: *ty,
                }));
            }

            Ok(None)
        }

        MIRGlobalKind::StringLiteral { value } => {
            if let MIRTypeKind::Array { length, inner } = target_kind {
                if let Ok(MIRTypeKind::Integer { ty, signed }) = engine.context.types().kind(*inner)
                {
                    if ty.bytes() == 1 {
                        let bytes = value.as_bytes();
                        let fields = (0..*length)
                            .map(|index| {
                                let byte = bytes.get(index).copied().unwrap_or(0);
                                (
                                    index,
                                    MIRConstant::Integer {
                                        value: byte as i128,
                                        ty: *ty,
                                        signed: *signed,
                                    },
                                )
                            })
                            .collect();
                        return Ok(Some(MIRConstant::Aggregate {
                            ty: to_type,
                            fields,
                        }));
                    }
                }
            }

            Ok(None)
        }
    }
}

pub(super) fn address_of(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let (root, path) = resolve_projection(engine, place);
    let MIRTarget::Global(global) = root else {
        return comptime_error(
            range.clone(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "address of a local value".into(),
            ),
        );
    };

    if path.is_empty() {
        let ty = global_address_type(engine, global, range)?;
        return Ok(MIRConstant::Global {
            global,
            offset: 0,
            ty,
        });
    }

    let Some(MIRGlobalKind::Variable { ty: start, .. }) =
        engine.context.global(global).map(|g| &g.kind)
    else {
        return comptime_error(
            range.clone(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "projection into a global".into(),
            ),
        );
    };

    let mut offset: i64 = 0;
    let mut ty = *start;
    for segment in &path {
        match segment {
            PathSeg::Field(index) => match field_layout(engine.context.types(), ty, *index) {
                Ok(MIRFieldLayout::Standard {
                    offset: field_offset,
                    ty: field_ty,
                }) => {
                    offset += field_offset as i64;
                    ty = field_ty;
                }
                Ok(MIRFieldLayout::Bitfield { .. }) => {
                    return comptime_error(
                        range.clone(),
                        (
                            &catalogue::COMPTIME_INVALID_OPERATION,
                            "address of a bitfield".into(),
                        ),
                    );
                }
                Err(_) => {
                    return comptime_error(
                        range.clone(),
                        (
                            &catalogue::INVALID_LAYOUT,
                            ("field projection".into(), "a valid layout".into(), None),
                        ),
                    );
                }
            },
            PathSeg::Index(index) => {
                let inner = match engine.context.types().kind(ty) {
                    Ok(MIRTypeKind::Array { inner, .. }) => *inner,
                    _ => {
                        return comptime_error(
                            range.clone(),
                            (
                                &catalogue::COMPTIME_INVALID_OPERATION,
                                "index projection on a non-array".into(),
                            ),
                        );
                    }
                };
                if *index < 0 {
                    return comptime_error(
                        range.clone(),
                        (
                            &catalogue::INDEX_BOUNDS,
                            ("array".into(), index.to_string()),
                        ),
                    );
                }
                let stride = match layout_of(engine.context.types(), inner) {
                    Ok(layout) => layout.size as i64,
                    Err(_) => {
                        return comptime_error(
                            range.clone(),
                            (
                                &catalogue::INVALID_LAYOUT,
                                ("array element".into(), "a valid layout".into(), None),
                            ),
                        );
                    }
                };
                offset += stride * *index as i64;
                ty = inner;
            }
            PathSeg::Variant(_) => {
                return comptime_error(
                    range.clone(),
                    (
                        &catalogue::COMPTIME_INVALID_OPERATION,
                        "variant projection in an address-of computation".into(),
                    ),
                );
            }
        }
    }

    Ok(MIRConstant::Global { global, offset, ty })
}

pub(super) fn global_address_type(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    global: MIRGlobalID,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    let Some(global) = engine.context.global(global) else {
        return comptime_error(
            range.clone(),
            (
                &catalogue::MISSING_ENTITY,
                ("global".into(), "comptime address computation".into()),
            ),
        );
    };

    match global.kind {
        MIRGlobalKind::Variable { ty, .. } => Ok(ty),
        MIRGlobalKind::StringLiteral { .. } => {
            let Some(ty) = engine.context.types().find_kind(&MIRTypeKind::Str) else {
                return comptime_error(
                    range.clone(),
                    (&catalogue::COMPTIME_UNAVAILABLE, "string type".into()),
                );
            };
            Ok(ty)
        }
    }
}

pub(super) fn read_value(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    value: &MIRValue,
) -> CXResult<MIRComptimeValue> {
    Ok(match value {
        MIRValue::Constant(constant) => MIRComptimeValue::Constant(constant.clone()),
        MIRValue::Register(register) => engine
            .frames
            .last()
            .and_then(|frame| frame.registers.get(register))
            .cloned()
            .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
        MIRValue::Reference(target) => {
            let frame = engine.frames.len() - 1;
            if reference_parameter(engine, frame, *target) {
                return Ok(engine.frames[frame].cells[target].clone());
            }
            let (root, _) = resolve_projection(engine, *target);
            if matches!(root, MIRTarget::Global(_)) {
                MIRComptimeValue::Constant(address_of(engine, *target, &TokenRange::internal())?)
            } else {
                MIRComptimeValue::Reference {
                    frame: engine.frames.last().expect("active frame").id,
                    target: *target,
                }
            }
        }
    })
}

pub(super) fn read_target(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    target: MIRTarget,
) -> CXResult<MIRComptimeValue> {
    read_at(engine, engine.frames.len() - 1, target)
}

pub(super) fn write_target(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    target: MIRTarget,
    value: MIRComptimeValue,
    aggregate_type: Option<MIRTypeID>,
) -> CXResult<()> {
    write_at(
        engine,
        engine.frames.len() - 1,
        target,
        value,
        aggregate_type,
    )
}

pub(super) fn read_aggregate(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    value: &MIRValue,
) -> CXResult<MIRConstant> {
    let value = match value {
        MIRValue::Reference(target) => read_target(engine, *target)?,
        _ => read_value(engine, value)?,
    };
    match value {
        MIRComptimeValue::Constant(value) => Ok(value),
        _ => comptime_error(
            TokenRange::internal(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "read a nonconstant aggregate".into(),
            ),
        ),
    }
}

pub(super) fn read_constant(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    value: &MIRValue,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    match read_value(engine, value)? {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) | MIRComptimeValue::Reference { .. } => comptime_error(
            range.clone(),
            (
                &catalogue::ENTITY_REQUIREMENT,
                (
                    "staged value".into(),
                    "a concrete value".into(),
                    Some("nonconstant value".into()),
                ),
            ),
        ),
    }
}

pub(super) fn reference_parameter(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    frame: usize,
    target: MIRTarget,
) -> bool {
    let MIRTarget::Place(place) = target else {
        return false;
    };
    let Some(state) = engine.frames.get(frame) else {
        return false;
    };
    let Some(index) = state
        .code
        .parameters()
        .iter()
        .position(|parameter| *parameter == place)
    else {
        return false;
    };
    state.code.prototype().signature.params[index]
        .staged_params
        .is_none()
        && state.code.places().get(place.index()).is_some_and(|decl| {
            engine
                .context
                .types()
                .is_reference_type(decl.ty)
                .unwrap_or(false)
        })
}

fn reference_target(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    frame: usize,
    value: Option<&MIRComptimeValue>,
) -> CXResult<(usize, MIRTarget)> {
    match value {
        Some(MIRComptimeValue::Reference { frame, target }) => {
            match engine.frames.iter().position(|state| state.id == *frame) {
                Some(index) => Ok((index, *target)),
                None => comptime_error(
                    TokenRange::internal(),
                    (
                        &catalogue::COMPTIME_INVALID_OPERATION,
                        "dereference an expired comptime frame".into(),
                    ),
                ),
            }
        }
        Some(MIRComptimeValue::Constant(MIRConstant::Global {
            global, offset: 0, ..
        })) => Ok((frame, MIRTarget::Global(*global))),
        _ => comptime_error(
            TokenRange::internal(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "dereference an unsupported comptime target".into(),
            ),
        ),
    }
}

fn indirect_target(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    frame: usize,
    register: cx_mir::MIRRegister,
) -> CXResult<(usize, MIRTarget)> {
    reference_target(
        engine,
        frame,
        engine
            .frames
            .get(frame)
            .and_then(|frame| frame.registers.get(&register)),
    )
}

fn read_at(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    frame: usize,
    target: MIRTarget,
) -> CXResult<MIRComptimeValue> {
    let Some(state) = engine.frames.get(frame) else {
        return comptime_error(
            TokenRange::internal(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "read a reference to an expired comptime frame".into(),
            ),
        );
    };
    if reference_parameter(engine, frame, target) {
        let (frame, target) = reference_target(engine, frame, state.cells.get(&target))?;
        return read_at(engine, frame, target);
    }
    if let Some((root, path)) = state.derived.get(&target).cloned() {
        let MIRComptimeValue::Constant(value) = read_at(engine, frame, root)? else {
            return comptime_error(
                TokenRange::internal(),
                (
                    &catalogue::COMPTIME_INVALID_OPERATION,
                    "project a nonconstant comptime value".into(),
                ),
            );
        };
        return Ok(MIRComptimeValue::Constant(read_path(&value, &path)));
    }
    match target {
        MIRTarget::Global(global) => Ok(MIRComptimeValue::Constant(read_global(engine, global)?)),
        MIRTarget::Place(_) => Ok(state
            .cells
            .get(&target)
            .cloned()
            .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined))),
        MIRTarget::Register(register) => {
            let (frame, target) = indirect_target(engine, frame, register)?;
            read_at(engine, frame, target)
        }
    }
}

fn write_at(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    frame: usize,
    target: MIRTarget,
    value: MIRComptimeValue,
    aggregate_type: Option<MIRTypeID>,
) -> CXResult<()> {
    let Some(state) = engine.frames.get(frame) else {
        return comptime_error(
            TokenRange::internal(),
            (
                &catalogue::COMPTIME_INVALID_OPERATION,
                "write a reference to an expired comptime frame".into(),
            ),
        );
    };
    if reference_parameter(engine, frame, target) {
        let (frame, target) = reference_target(engine, frame, state.cells.get(&target))?;
        return write_at(engine, frame, target, value, aggregate_type);
    }
    if let Some((root, path)) = state.derived.get(&target).cloned() {
        let MIRComptimeValue::Constant(current) = read_at(engine, frame, root)? else {
            return comptime_error(
                TokenRange::internal(),
                (
                    &catalogue::COMPTIME_INVALID_OPERATION,
                    "assign through a nonconstant comptime value".into(),
                ),
            );
        };
        let MIRComptimeValue::Constant(value) = value else {
            return comptime_error(
                TokenRange::internal(),
                (
                    &catalogue::COMPTIME_INVALID_OPERATION,
                    "store a nonconstant value in an aggregate projection".into(),
                ),
            );
        };
        return write_at(
            engine,
            frame,
            root,
            MIRComptimeValue::Constant(write_path(&current, &path, value, aggregate_type)),
            aggregate_type,
        );
    }
    match target {
        MIRTarget::Global(global) => {
            let MIRComptimeValue::Constant(value) = value else {
                return comptime_error(
                    TokenRange::internal(),
                    (
                        &catalogue::COMPTIME_INVALID_OPERATION,
                        "store a nonconstant value in a global".into(),
                    ),
                );
            };
            engine.globals.insert(global, value);
        }
        MIRTarget::Place(_) => {
            engine.frames[frame].cells.insert(target, value);
        }
        MIRTarget::Register(register) => {
            let (frame, target) = indirect_target(engine, frame, register)?;
            write_at(engine, frame, target, value, aggregate_type)?;
        }
    }
    Ok(())
}

fn read_global(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    global: MIRGlobalID,
) -> CXResult<MIRConstant> {
    if let Some(cached) = engine.globals.get(&global) {
        return Ok(cached.clone());
    }
    if !engine.evaluating_globals.insert(global) {
        return comptime_error(
            TokenRange::internal(),
            (&catalogue::COMPTIME_GLOBAL_CYCLE, ()),
        );
    }

    let result = (|| {
        let resolver = engine.context;

        if let Some(initializer) = resolver.global_initializer(global) {
            return match execution::call_function(engine, initializer, &[])? {
                MIRComptimeValue::Constant(value) => Ok(value),
                MIRComptimeValue::Staged(_) | MIRComptimeValue::Reference { .. } => comptime_error(
                    TokenRange::internal(),
                    (
                        &catalogue::ENTITY_REQUIREMENT,
                        (
                            "global initializer".into(),
                            "a compile-time value".into(),
                            Some("nonconstant value".into()),
                        ),
                    ),
                ),
            };
        }

        let Some(var) = resolver.global(global) else {
            return comptime_error(
                TokenRange::internal(),
                (&catalogue::COMPTIME_UNAVAILABLE, "global".into()),
            );
        };

        match &var.kind {
            MIRGlobalKind::StringLiteral { .. } => {
                let range = TokenRange::internal();
                let ty = global_address_type(engine, global, &range)?;
                return Ok(MIRConstant::Global {
                    global,
                    offset: 0,
                    ty,
                });
            }

            MIRGlobalKind::Variable { ty, state, .. } => match state {
                MIRGlobalState::External => {
                    return comptime_error(
                        TokenRange::internal(),
                        (&catalogue::COMPTIME_UNAVAILABLE, "global".into()),
                    );
                }
                MIRGlobalState::ZeroInitialized => {
                    return Ok(MIRConstant::Global {
                        global,
                        offset: 0,
                        ty: *ty,
                    });
                }
                MIRGlobalState::Initialized(constant) => {
                    return Ok(constant.clone());
                }
            },
        }
    })();

    engine.evaluating_globals.remove(&global);

    let constant = result?;
    engine.globals.insert(global, constant.clone());

    Ok(constant)
}

pub(super) fn read_path(root: &MIRConstant, path: &[PathSeg]) -> MIRConstant {
    let mut current = root.clone();
    for segment in path {
        let fields = match &current {
            MIRConstant::Aggregate { fields, .. } => fields.clone(),
            _ => return MIRConstant::Undefined,
        };
        let key = segment.key();
        current = fields
            .iter()
            .find(|(index, _)| *index == key)
            .map(|(_, value)| value.clone())
            .unwrap_or(MIRConstant::Undefined);
    }
    current
}

fn write_path(
    root: &MIRConstant,
    path: &[PathSeg],
    value: MIRConstant,
    aggregate_type: Option<MIRTypeID>,
) -> MIRConstant {
    let Some((head, tail)) = path.split_first() else {
        return value;
    };

    let key = head.key();

    let (ty, mut fields) = match root {
        MIRConstant::Aggregate { ty, fields } => (*ty, fields.clone()),
        _ => match aggregate_type {
            Some(ty) => (ty, Vec::new()),
            None => {
                return MIRConstant::Undefined;
            }
        },
    };

    match fields.iter().position(|(index, _)| *index == key) {
        Some(position) => {
            if tail.is_empty() {
                fields[position] = (key, value);
            } else {
                let child = fields[position].1.clone();
                let child = write_path(&child, tail, value, Some(ty));
                fields[position] = (key, child);
            }
        }
        None => {
            if tail.is_empty() {
                fields.push((key, value));
            } else {
                let child = write_path(&MIRConstant::Undefined, tail, value, Some(ty));
                fields.push((key, child));
            }
        }
    }

    MIRConstant::Aggregate { ty, fields }
}
