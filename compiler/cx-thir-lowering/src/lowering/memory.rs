use cx_log::CXResult;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_mir::{
    MIRBindable, MIRInstruction, MIRInstructionKind, MIRPlaceID, MIRRegister, MIRTypeID, MIRValue,
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
                target: place,
                ty: type_id,
                value,
            },
            range.clone(),
        ));

        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            range.clone(),
        ));
    }

    Ok(place)
}

pub(crate) fn ensure_place(
    _builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    _ty: &THIRType,
) -> CXResult<MIRPlaceID> {
    match value {
        MIRValue::PlaceRef(place) => Ok(place),
        _ => unreachable!("an lvalue expression must lower to a place"),
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
    place: MIRPlaceID,
    ty: MIRTypeID,
    range: &TokenRange,
) -> MIRValue {
    let out = target_register(builder, ty);
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::LiftPlace { out, place },
        range.clone(),
    ));

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
                MIRInstructionKind::Forward { out, source },
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
