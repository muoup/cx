use cx_log::CXResult;
use cx_mir::{
    MIRConstant, MIRFieldLayout, MIRGlobalID, MIRGlobalKind, MIRPlace, MIRTypeID, MIRTypeKind,
    MIRValue,
    ty::interface::MTRegistry,
    ty::layout::{field_layout, layout_of},
};
use cx_tokens::TokenRange;

use crate::{error::comptime_error, value::MIRComptimeValue};

use super::{MIRComptimeEngine, execution, ops, state::PathSeg};

pub(super) fn resolve_projection(
    engine: &MIRComptimeEngine<'_>,
    place: MIRPlace,
) -> (MIRPlace, Vec<PathSeg>) {
    engine
        .frames
        .last()
        .and_then(|frame| frame.derived.get(&place).cloned())
        .unwrap_or((place, Vec::new()))
}

pub(super) fn coerce_global_special(
    engine: &MIRComptimeEngine<'_>,
    operand: &MIRValue,
    to_type: MIRTypeID,
) -> CXResult<Option<MIRConstant>> {
    let MIRValue::PlaceRef(MIRPlace::Global(global)) = operand else {
        return Ok(None);
    };
    let Some(registry) = engine.resolver.types() else {
        return Ok(None);
    };
    let Ok(target_kind) = registry.kind(to_type) else {
        return Ok(None);
    };

    match engine.resolver.global_kind(*global) {
        Some(MIRGlobalKind::Variable { ty, .. }) => {
            let decays = matches!(
                target_kind,
                MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. }
            ) && matches!(registry.kind(ty), Ok(MIRTypeKind::Array { .. }));
            if decays {
                return Ok(Some(ops::relocation_constant(*global, 0, ty)));
            }
            Ok(None)
        }
        Some(MIRGlobalKind::StringLiteral { value }) => {
            if let MIRTypeKind::Array { length, inner } = target_kind {
                if let Ok(MIRTypeKind::Integer { ty, signed }) = registry.kind(*inner) {
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
        _ => Ok(None),
    }
}

pub(super) fn address_of(
    engine: &MIRComptimeEngine<'_>,
    place: MIRPlace,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let (root, path) = resolve_projection(engine, place);
    let MIRPlace::Global(global) = root else {
        return comptime_error(
            range.clone(),
            "cannot take the address of a local value in a comptime context",
        );
    };

    if path.is_empty() {
        let ty = global_address_type(engine, global, range)?;
        return Ok(ops::relocation_constant(global, 0, ty));
    }

    let Some(registry) = engine.resolver.types() else {
        return comptime_error(
            range.clone(),
            "type layouts are unavailable during comptime evaluation",
        );
    };
    let Some(MIRGlobalKind::Variable { ty: start, .. }) = engine.resolver.global_kind(global)
    else {
        return comptime_error(
            range.clone(),
            "cannot project into this global in a comptime context",
        );
    };

    let mut offset: i64 = 0;
    let mut ty = start;
    for segment in &path {
        match segment {
            PathSeg::Field(index) => match field_layout(registry, ty, *index) {
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
                        "address-of a bitfield is not supported in a comptime context",
                    );
                }
                Err(_) => {
                    return comptime_error(
                        range.clone(),
                        "invalid field projection in an address-of computation",
                    );
                }
            },
            PathSeg::Index(index) => {
                let inner = match registry.kind(ty) {
                    Ok(MIRTypeKind::Array { inner, .. }) => *inner,
                    _ => {
                        return comptime_error(
                            range.clone(),
                            "index projection on a non-array in an address-of computation",
                        );
                    }
                };
                if *index < 0 {
                    return comptime_error(
                        range.clone(),
                        "negative array index in an address-of computation",
                    );
                }
                let stride = match layout_of(registry, inner) {
                    Ok(layout) => layout.size as i64,
                    Err(_) => {
                        return comptime_error(
                            range.clone(),
                            "invalid element layout in an address-of computation",
                        );
                    }
                };
                offset += stride * *index as i64;
                ty = inner;
            }
            PathSeg::Variant(_) => {
                return comptime_error(
                    range.clone(),
                    "variant projections are not supported in address-of computations",
                );
            }
        }
    }

    Ok(ops::relocation_constant(global, offset, ty))
}

pub(super) fn global_address_type(
    engine: &MIRComptimeEngine<'_>,
    global: MIRGlobalID,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    match engine.resolver.global_kind(global) {
        Some(MIRGlobalKind::Variable { ty, .. }) => Ok(ty),
        Some(MIRGlobalKind::StringLiteral { .. }) => {
            let Some(types) = engine.resolver.types() else {
                return comptime_error(
                    range.clone(),
                    "type layouts are unavailable during comptime evaluation",
                );
            };
            let Some(ty) = types.find_kind(&MIRTypeKind::Str) else {
                return comptime_error(
                    range.clone(),
                    "the string type is unavailable during comptime evaluation",
                );
            };
            Ok(ty)
        }
        None => comptime_error(range.clone(), "unknown global in an address-of computation"),
    }
}

pub(super) fn read_value(
    engine: &mut MIRComptimeEngine<'_>,
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
    engine: &mut MIRComptimeEngine<'_>,
    value: &MIRValue,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    match read_value(engine, value)? {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) => {
            comptime_error(range.clone(), "staged value used as a concrete value")
        }
    }
}

fn read_global_rvalue(
    engine: &mut MIRComptimeEngine<'_>,
    global: MIRGlobalID,
) -> CXResult<MIRConstant> {
    if let Some(MIRGlobalKind::Variable { ty, .. }) = engine.resolver.global_kind(global)
        && let Some(registry) = engine.resolver.types()
        && let Ok(MIRTypeKind::Array { inner, .. }) = registry.kind(ty)
    {
        return Ok(ops::relocation_constant(global, 0, *inner));
    }
    read_global(engine, global)
}

fn read_place(engine: &mut MIRComptimeEngine<'_>, place: MIRPlace) -> CXResult<MIRComptimeValue> {
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
            "cannot project through a staged value",
        );
    };
    Ok(MIRComptimeValue::Constant(read_path(&root, &projection.1)))
}

pub(super) fn write_place(
    engine: &mut MIRComptimeEngine<'_>,
    place: MIRPlace,
    value: MIRComptimeValue,
    aggregate_type: Option<MIRTypeID>,
) -> CXResult<()> {
    if let MIRPlace::Global(global) = place {
        let MIRComptimeValue::Constant(value) = value else {
            return comptime_error(
                TokenRange::internal(),
                "cannot store a staged value in a global",
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
            "cannot assign through a staged value",
        );
    };
    let MIRComptimeValue::Constant(value) = value else {
        return comptime_error(
            TokenRange::internal(),
            "cannot store a staged value in an aggregate projection",
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
    engine: &mut MIRComptimeEngine<'_>,
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

fn read_global(engine: &mut MIRComptimeEngine<'_>, global: MIRGlobalID) -> CXResult<MIRConstant> {
    if let Some(cached) = engine.globals.get(&global) {
        return Ok(cached.clone());
    }
    if !engine.evaluating_globals.insert(global) {
        return comptime_error(
            TokenRange::internal(),
            "cyclic dependency between global initializers",
        );
    }

    let result = (|| {
        let resolver = engine.resolver;
        if let Some(constant) = resolver.global_constant(global) {
            return Ok(constant);
        }
        if let Some(initializer) = resolver.global_initializer(global) {
            return match execution::call_function(engine, initializer, &[])? {
                MIRComptimeValue::Constant(value) => Ok(value),
                MIRComptimeValue::Staged(_) => comptime_error(
                    TokenRange::internal(),
                    "global initializer returned a staged value",
                ),
            };
        }
        if matches!(
            resolver.global_kind(global),
            Some(MIRGlobalKind::StringLiteral { .. })
        ) {
            let range = TokenRange::internal();
            let ty = global_address_type(engine, global, &range)?;
            return Ok(ops::relocation_constant(global, 0, ty));
        }
        comptime_error(
            TokenRange::internal(),
            "global is not available during comptime evaluation",
        )
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
