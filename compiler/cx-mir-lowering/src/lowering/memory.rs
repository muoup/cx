use cx_lmir::types::{LMIRIntegerType, LMIRType, TypeSize};
use cx_lmir::{LMIRInstructionKind, LMIRPtrBinOp, LMIRRegister, LMIRValue};
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRRegister, MIRTypeID};

use crate::context::FunctionContext;

pub(crate) fn void(context: &mut FunctionContext<'_, '_>, kind: LMIRInstructionKind) {
    context.emit(kind, LMIRType::unit(), None);
}

pub(crate) fn temp(
    context: &mut FunctionContext<'_, '_>,
    kind: LMIRInstructionKind,
    ty: LMIRType,
) -> LMIRValue {
    let register = LMIRRegister::new(format!("tmp.{}", context.next_register));
    context.next_register += 1;
    context.emit(kind, ty.clone(), Some(register.clone()));
    LMIRValue::Register { register, ty }
}

pub(crate) fn assign(
    context: &mut FunctionContext<'_, '_>,
    out: MIRRegister,
    kind: LMIRInstructionKind,
) {
    let LMIRValue::Register { register, ty } = context.reg(out) else {
        unreachable!()
    };
    context.emit(kind, ty, Some(register));
}

pub(crate) fn offset(
    context: &mut FunctionContext<'_, '_>,
    base: LMIRValue,
    offset: i64,
) -> LMIRValue {
    if offset == 0 {
        return base;
    }
    temp(
        context,
        LMIRInstructionKind::PointerBinOp {
            op: LMIRPtrBinOp::ADD,
            ptr_type: context.pointer(),
            type_size: TypeSize::from(1),
            left: base,
            right: context.integer(offset.into(), LMIRIntegerType::I64),
        },
        context.pointer(),
    )
}

pub(crate) fn allocate(context: &mut FunctionContext<'_, '_>, ty: MIRTypeID) -> LMIRValue {
    let layout = calculate_type_layout(context.types(), ty);
    temp(
        context,
        LMIRInstructionKind::Allocate {
            ty: context.ty(ty),
            alignment: layout.alignment() as u8,
        },
        context.pointer(),
    )
}

pub(crate) fn store(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    value: LMIRValue,
    ty: MIRTypeID,
) {
    let lowered = context.ty(ty);
    if lowered.is_void() {
        return;
    }
    if lowered.is_memory_resident() {
        let layout = calculate_type_layout(context.types(), ty);
        void(
            context,
            LMIRInstructionKind::Memcpy {
                dest: address,
                src: value,
                size: context.integer(layout.size() as i128, LMIRIntegerType::I64),
                alignment: layout.alignment() as u8,
            },
        );
    } else {
        void(
            context,
            LMIRInstructionKind::Store {
                memory: address,
                value,
                ty: lowered,
            },
        );
    }
}

pub(crate) fn load(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    ty: MIRTypeID,
) -> LMIRValue {
    let lowered = context.ty(ty);
    if lowered.is_void() {
        return LMIRValue::NULL;
    }
    if lowered.is_memory_resident() {
        address
    } else {
        temp(
            context,
            LMIRInstructionKind::Load {
                memory: address,
                ty: lowered.clone(),
            },
            lowered,
        )
    }
}
