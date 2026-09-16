use cx_log::{CXResult, catalogue::mir};
use cx_mir::{MIRInstrKind, MIRRegister, MIRTarget, MIRType, MIRTypeID, MIRTypeKind, MIRValue};
use cx_thir::thir::data::THIRType;
use cx_thir::type_context::THIRTypeContext;

use crate::builder::MIRBuilder;
use crate::lowering::types::lower_type;

pub(super) fn assign_operand_to_place(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
    name: Option<cx_util::identifier::CXIdent>,
) -> CXResult<cx_mir::MIRPlaceID> {
    let type_id = lower_type(builder, ty)?;
    let place = builder.create(type_id, name, ty.is_nodrop());
    builder.emit(MIRInstrKind::Store {
        target: MIRTarget::Place(place),
        value,
        ty: type_id,
    });
    Ok(place)
}

pub(super) fn target_register(builder: &mut MIRBuilder<'_>, ty: MIRTypeID) -> MIRRegister {
    let reference = builder.types_mut().intern(MIRType {
        kind: MIRTypeKind::MemoryReference {
            inner: ty,
            bitfield: None,
        },
        layout: None,
    });
    builder.fun_mut().new_register(reference, None)
}

pub(super) fn ensure_place(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
) -> CXResult<MIRTarget> {
    match value {
        MIRValue::Reference(target) => Ok(target),
        value
            if ty.is_memory_reference()
                || matches!(ty.kind, cx_thir::thir::data::THIRTypeKind::PointerTo { .. }) =>
        {
            if let Some(inner) = ty.mem_ref_inner()
                && let MIRValue::Register(register) = &value
            {
                let pointee = builder.registry().resolve_type_id(inner).clone();
                let pointee_type = lower_type(builder, &pointee)?;
                if builder.fun().register_type(*register) == Some(pointee_type) {
                    return assign_operand_to_place(builder, value, &pointee, None)
                        .map(MIRTarget::Place);
                }
            }
            let type_id = lower_type(builder, ty)?;
            let register = match value {
                MIRValue::Register(register) => register,
                value => {
                    let register = builder.fun_mut().new_register(type_id, None);
                    builder.emit(MIRInstrKind::Let {
                        out: register,
                        value,
                    });
                    register
                }
            };
            Ok(MIRTarget::Register(register))
        }
        value => assign_operand_to_place(builder, value, ty, None).map(MIRTarget::Place),
    }
}

pub(super) fn move_value(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: MIRTypeID,
    range: &cx_tokens::TokenRange,
) -> CXResult<MIRValue> {
    let source = match value {
        MIRValue::Reference(source) => source,
        MIRValue::Register(_) => return Ok(value),
        _ => return crate::log::log_mir_error(range, (&mir::MOVE_VALUE, format!("{value:?}"))),
    };
    let root = root(builder, source);
    if matches!(root, Some(MIRTarget::Global(_))) {
        return crate::log::log_mir_error(range, (&mir::MOVE_VALUE, "global storage".into()));
    }
    let value = copy(builder, source, ty);
    if let Some(MIRTarget::Place(place)) = root {
        builder.emit(MIRInstrKind::Invalidate { place, leak: false });
    }
    Ok(value)
}

fn root(builder: &MIRBuilder<'_>, mut target: MIRTarget) -> Option<MIRTarget> {
    use cx_mir::{MIRAggregateOp, MIRStagedInstrKind, MIRTargetAggregateOp};
    let mut visited = std::collections::HashSet::new();
    while let MIRTarget::Register(register) = target {
        if !visited.insert(register) {
            return None;
        }
        target = builder
            .fun()
            .body()
            .blocks()
            .iter()
            .flat_map(|block| &block.instrs)
            .find_map(|instruction| match &instruction.kind {
                MIRStagedInstrKind::Standard(MIRInstrKind::AggregateOp(
                    MIRAggregateOp::Target { out, op },
                )) if *out == register => Some(match op {
                    MIRTargetAggregateOp::Field { base, .. }
                    | MIRTargetAggregateOp::Variant { base, .. }
                    | MIRTargetAggregateOp::Index { base, .. } => *base,
                }),
                MIRStagedInstrKind::Standard(MIRInstrKind::Let { out, value })
                | MIRStagedInstrKind::Standard(MIRInstrKind::Coerce {
                    out,
                    operand: value,
                    coercion: cx_mir::MIRCoercion::TypeChange,
                    ..
                }) if *out == register => match value {
                    MIRValue::Reference(target) => Some(*target),
                    MIRValue::Register(register) => Some(MIRTarget::Register(*register)),
                    _ => None,
                },
                _ => None,
            })?;
    }
    Some(target)
}
