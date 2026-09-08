use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRConstant, MIRFieldLayout, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRPlace, MIRTypeID,
    MIRTypeKind, MIRValue,
    ty::{
        interface::MTRegistry,
        layout::{field_layout, layout_of},
    },
};
use cx_tokens::TokenRange;

use crate::{ComptimeContext, log::comptime_error, value::MIRComptimeValue};

use super::{MIRComptimeEngine, execution, ops, state::PathSeg};

pub(super) fn resolve_projection(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRPlace,
) -> (MIRPlace, Vec<PathSeg>) {
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
    let MIRValue::PlaceRef(MIRPlace::Global(global_id)) = operand else {
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
                return Ok(Some(ops::relocation_constant(*global_id, 0, *ty)));
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
    place: MIRPlace,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let (root, path) = resolve_projection(engine, place);
    let MIRPlace::Global(global) = root else {
        return comptime_error(range.clone(), (&catalogue::COMPTIME_LOCAL_ADDRESS, ()));
    };

    if path.is_empty() {
        let ty = global_address_type(engine, global, range)?;
        return Ok(ops::relocation_constant(global, 0, ty));
    }

    let Some(MIRGlobalKind::Variable { ty: start, .. }) =
        engine.context.global(global).map(|g| &g.kind)
    else {
        return comptime_error(range.clone(), (&catalogue::COMPTIME_GLOBAL_PROJECTION, ()));
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
                        (&catalogue::COMPTIME_BITFIELD_ADDRESS, ()),
                    );
                }
                Err(_) => {
                    return comptime_error(
                        range.clone(),
                        (&catalogue::COMPTIME_INVALID_FIELD_PROJECTION, ()),
                    );
                }
            },
            PathSeg::Index(index) => {
                let inner = match engine.context.types().kind(ty) {
                    Ok(MIRTypeKind::Array { inner, .. }) => *inner,
                    _ => {
                        return comptime_error(
                            range.clone(),
                            (&catalogue::COMPTIME_NON_ARRAY_INDEX, ()),
                        );
                    }
                };
                if *index < 0 {
                    return comptime_error(
                        range.clone(),
                        (&catalogue::COMPTIME_NEGATIVE_INDEX, ()),
                    );
                }
                let stride = match layout_of(engine.context.types(), inner) {
                    Ok(layout) => layout.size as i64,
                    Err(_) => {
                        return comptime_error(
                            range.clone(),
                            (&catalogue::COMPTIME_INVALID_ELEMENT_LAYOUT, ()),
                        );
                    }
                };
                offset += stride * *index as i64;
                ty = inner;
            }
            PathSeg::Variant(_) => {
                return comptime_error(range.clone(), (&catalogue::COMPTIME_VARIANT_ADDRESS, ()));
            }
        }
    }

    Ok(ops::relocation_constant(global, offset, ty))
}

pub(super) fn global_address_type(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    global: MIRGlobalID,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    let Some(global) = engine.context.global(global) else {
        return comptime_error(
            range.clone(),
            (&catalogue::COMPTIME_UNKNOWN_GLOBAL_ADDRESS, ()),
        );
    };

    match global.kind {
        MIRGlobalKind::Variable { ty, .. } => Ok(ty),
        MIRGlobalKind::StringLiteral { .. } => {
            let Some(ty) = engine.context.types().find_kind(&MIRTypeKind::Str) else {
                return comptime_error(
                    range.clone(),
                    (&catalogue::COMPTIME_STRING_TYPE_UNAVAILABLE, ()),
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
        MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
            if let MIRPlace::Global(global) = place {
                MIRComptimeValue::Constant(read_global_rvalue(engine, *global)?)
            } else {
                read_place(engine, *place)?
            }
        }
    })
}

pub(super) fn read_constant(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    value: &MIRValue,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    match read_value(engine, value)? {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) => {
            comptime_error(range.clone(), (&catalogue::COMPTIME_STAGED_CONCRETE, ()))
        }
    }
}

fn read_global_rvalue(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    global: MIRGlobalID,
) -> CXResult<MIRConstant> {
    let Some(global) = engine.context.global(global) else {
        return comptime_error(
            TokenRange::internal(),
            (&catalogue::COMPTIME_UNKNOWN_GLOBAL_ADDRESS, ()),
        );
    };

    match &global.kind {
        MIRGlobalKind::Variable { ty, .. } => {
            if let Ok(MIRTypeKind::Array { inner, .. }) = engine.context.types().kind(*ty) {
                return Ok(ops::relocation_constant(global.id, 0, *inner));
            }
        }
        MIRGlobalKind::StringLiteral { .. } => {
            let ty = global_address_type(engine, global.id, &TokenRange::internal())?;
            return Ok(ops::relocation_constant(global.id, 0, ty));
        }
    }

    read_global(engine, global.id)
}

fn read_place(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRPlace,
) -> CXResult<MIRComptimeValue> {
    if let MIRPlace::Global(global) = place {
        return Ok(MIRComptimeValue::Constant(read_global(engine, global)?));
    }

    let projection = resolve_projection(engine, place);
    if projection.1.is_empty() {
        return Ok(engine
            .frames
            .last()
            .and_then(|frame| frame.cells.get(&place))
            .cloned()
            .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)));
    }

    let root = match &projection.0 {
        MIRPlace::Global(global) => MIRComptimeValue::Constant(read_global(engine, *global)?),
        other => engine
            .frames
            .last()
            .and_then(|frame| frame.cells.get(other))
            .cloned()
            .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
    };
    let MIRComptimeValue::Constant(root) = root else {
        return comptime_error(
            TokenRange::internal(),
            (&catalogue::COMPTIME_STAGED_PROJECTION, ()),
        );
    };
    Ok(MIRComptimeValue::Constant(read_path(&root, &projection.1)))
}

pub(super) fn write_place(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRPlace,
    value: MIRComptimeValue,
    aggregate_type: Option<MIRTypeID>,
) -> CXResult<()> {
    if let MIRPlace::Global(global) = place {
        let MIRComptimeValue::Constant(value) = value else {
            return comptime_error(
                TokenRange::internal(),
                (&catalogue::COMPTIME_STAGED_GLOBAL_STORE, ()),
            );
        };
        engine.globals.insert(global, value);
        return Ok(());
    }

    let projection = resolve_projection(engine, place);
    if projection.1.is_empty() {
        write_direct_cell(engine, place, value);
        return Ok(());
    }

    let (root, path) = projection;
    let current = match &root {
        MIRPlace::Global(global) => MIRComptimeValue::Constant(read_global(engine, *global)?),
        other => engine
            .frames
            .last()
            .and_then(|frame| frame.cells.get(other))
            .cloned()
            .unwrap_or(MIRComptimeValue::Constant(MIRConstant::Undefined)),
    };
    let MIRComptimeValue::Constant(current) = current else {
        return comptime_error(
            TokenRange::internal(),
            (&catalogue::COMPTIME_STAGED_ASSIGNMENT, ()),
        );
    };
    let MIRComptimeValue::Constant(value) = value else {
        return comptime_error(
            TokenRange::internal(),
            (&catalogue::COMPTIME_STAGED_AGGREGATE_STORE, ()),
        );
    };
    let updated = write_path(&current, &path, value, aggregate_type);
    match root {
        MIRPlace::Global(global) => {
            engine.globals.insert(global, updated);
        }
        other => {
            let frame = engine.frames.last_mut().expect("active frame");
            frame
                .cells
                .insert(other, MIRComptimeValue::Constant(updated));
        }
    }
    Ok(())
}

pub(super) fn write_direct_cell(
    engine: &mut MIRComptimeEngine<'_, impl ComptimeContext>,
    place: MIRPlace,
    value: MIRComptimeValue,
) {
    debug_assert!(
        !matches!(place, MIRPlace::Global(_)),
        "globals are handled by write_place"
    );
    let frame = engine.frames.last_mut().expect("active frame");
    frame.cells.insert(place, value);
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
                MIRComptimeValue::Staged(_) => comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_STAGED_GLOBAL_RESULT, ()),
                ),
            };
        }

        let Some(var) = resolver.global(global) else {
            return comptime_error(
                TokenRange::internal(),
                (&catalogue::COMPTIME_GLOBAL_UNAVAILABLE, ()),
            );
        };

        match &var.kind {
            MIRGlobalKind::StringLiteral { .. } => {
                let range = TokenRange::internal();
                let ty = global_address_type(engine, global, &range)?;
                return Ok(ops::relocation_constant(global, 0, ty));
            }

            MIRGlobalKind::Variable { ty, state, .. } => match state {
                MIRGlobalState::External => {
                    return comptime_error(
                        TokenRange::internal(),
                        (&catalogue::COMPTIME_GLOBAL_UNAVAILABLE, ()),
                    );
                }
                MIRGlobalState::ZeroInitialized => {
                    return Ok(ops::relocation_constant(global, 0, *ty));
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
