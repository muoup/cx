use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_mir::{
    MIRBindable, MIRConstant, MIRInstruction, MIRInstructionKind, MIRPlaceID, MIRRegister,
    MIRTarget, MIRTypeID, MIRValue,
};
use cx_thir::thir::data::THIRType;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::builder::MIRBuilder;
use crate::lowering::types::lower_type;

pub(crate) fn allocate_variable<'thir>(
    builder: &mut MIRBuilder<'thir>,
    name: Option<CXIdent>,
    ty: &'thir THIRType,
    value: Option<MIRValue>,
    range: &TokenRange,
) -> CXResult<cx_mir::MIRPlaceID> {
    let type_id = lower_type(builder, ty)?;
    let place = builder.new_place(type_id, name, ty.is_nodrop());

    if let Some(value) = value {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Store {
                target: MIRTarget::Place(place),
                ty: type_id,
                value,
            },
            range.clone(),
        ));
    }

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Initialize {
            place: MIRBindable::Place(place),
        },
        range.clone(),
    ));

    Ok(place)
}

pub(crate) fn expect_target(value: &MIRValue) -> MIRTarget {
    match value {
        MIRValue::PlaceRef(place) => MIRTarget::Place(*place),
        MIRValue::Register(register) => MIRTarget::Indirect(*register),
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => MIRTarget::Global(*reference),
        _ => unreachable!("an lvalue expression must lower to an addressable value"),
    }
}

pub(crate) fn target_register(builder: &mut MIRBuilder<'_>, ty: MIRTypeID) -> MIRRegister {
    builder.fun_mut().new_register(ty, None)
}

pub(crate) fn assign_operand_to_place<'thir>(
    builder: &mut MIRBuilder<'thir>,
    value: MIRValue,
    ty: &'thir THIRType,
    name: Option<CXIdent>,
    range: &TokenRange,
) -> CXResult<MIRPlaceID> {
    allocate_variable(builder, name, ty, Some(value), range)
}

pub(crate) fn copy(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: MIRTypeID,
    range: &TokenRange,
) -> MIRValue {
    let target = expect_target(&value);
    let out = target_register(builder, ty);
    let kind = match target {
        MIRTarget::Place(place) => MIRInstructionKind::LiftPlace { out, place },
        _ => MIRInstructionKind::Store {
            target: MIRTarget::Register(out),
            value,
            ty,
        },
    };
    builder.emit(MIRInstruction::new(kind, range.clone()));

    MIRValue::Register(out)
}

pub(crate) fn move_value(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: MIRTypeID,
    range: &TokenRange,
) -> CXResult<MIRValue> {
    match value {
        MIRValue::PlaceRef(place) => {
            let out = target_register(builder, ty);
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::LiftPlace { out, place },
                range.clone(),
            ));
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Place(place),
                    kind: MIRInvalidationKind::Move,
                },
                range.clone(),
            ));
            Ok(MIRValue::Register(out))
        }

        MIRValue::Register(source) => {
            let out = target_register(builder, ty);
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Store {
                    target: MIRTarget::Register(out),
                    value: MIRValue::Register(source),
                    ty,
                },
                range.clone(),
            ));
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Register(source),
                    kind: MIRInvalidationKind::Move,
                },
                range.clone(),
            ));
            Ok(MIRValue::Register(out))
        }
        value => Ok(value),
    }
}
