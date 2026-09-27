use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRBindable, MIRBitfieldAccess, MIRConstant, MIRInstruction, MIRInstructionKind, MIRPlaceID,
    MIRRegister, MIRStoreBitfield, MIRTarget, MIRTypeID, MIRValue,
};
use cx_thir::thir::data::THIRType;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::builder::MIRBuilder;
use crate::log::log_mir_error;
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
                bitfield: None,
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

pub(crate) fn check_block_argument(
    builder: &MIRBuilder<'_>,
    value: &MIRValue,
    expected: MIRTypeID,
    range: &TokenRange,
) -> CXResult<()> {
    let MIRValue::Register(register) = value else {
        return Ok(());
    };
    let actual = builder
        .fun()
        .register_type(*register)
        .expect("unknown block argument");
    let expected_kind = builder.types().definition(expected).unwrap().kind();
    let actual_kind = builder.types().definition(actual).unwrap().kind();
    if expected_kind != actual_kind {
        return log_mir_error(
            range,
            (
                &typecheck::TYPE_MISMATCH,
                (
                    "block argument".into(),
                    format!("{expected_kind:?}"),
                    format!("{actual_kind:?}"),
                ),
            ),
        );
    }
    Ok(())
}

pub(crate) fn target_register(builder: &mut MIRBuilder<'_>, ty: MIRTypeID) -> MIRRegister {
    builder.fun_mut().new_register(ty, None)
}

pub(crate) fn move_operand_to_place<'thir>(
    builder: &mut MIRBuilder<'thir>,
    value: MIRValue,
    ty: &'thir THIRType,
    name: Option<CXIdent>,
    range: &TokenRange,
) -> CXResult<MIRPlaceID> {
    let type_id = lower_type(builder, ty)?;
    let value = match value {
        value @ MIRValue::PlaceRef(_) => move_value(builder, value, type_id, range)?,
        value => value,
    };
    let place = allocate_variable(builder, name, ty, Some(value.clone()), range)?;
    if let MIRValue::Register(register) = value {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Register(register),
                kind: MIRInvalidationKind::Move,
            },
            range.clone(),
        ));
    }
    Ok(place)
}

/// Copies the value `value` refers to into a new register. `bitfield` is set when `value` is a
/// bitfield reference, in which case the extracted field is copied.
pub(crate) fn copy(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: MIRTypeID,
    bitfield: Option<MIRBitfieldAccess>,
    range: &TokenRange,
) -> MIRValue {
    let out = target_register(builder, ty);
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Store {
            target: MIRTarget::Register(out),
            value,
            ty,
            bitfield: bitfield.map(MIRStoreBitfield::Source),
        },
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
                MIRInstructionKind::Lift {
                    out,
                    source: MIRTarget::Place(place),
                },
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
                    bitfield: None,
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
