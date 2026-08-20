use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{LMIRInstruction, LMIRInstructionKind, LMIRPtrBinOp, LMIRRegister, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{MIRBasicBlockID, MIRPlace, MIRRegister as MIRRegisterID, MIRTypeID};
use cx_util::identifier::CXIdent;

use crate::context::FunctionLoweringContext;

use super::typing::{convert_integer_type, convert_type};

pub(super) fn global_index(ctx: &FunctionLoweringContext<'_>, global: cx_mir::MIRGlobalID) -> u32 {
    *ctx.global_indices()
        .get(&global)
        .expect("MIR value references a filtered global")
}

pub(super) fn place_decl_type(ctx: &FunctionLoweringContext<'_>, place: MIRPlace) -> MIRTypeID {
    match place {
        MIRPlace::FunctionLocal(id) => ctx.function().place(id).unwrap().ty,
        MIRPlace::Parameter(id) => ctx.function().prototype.signature.params[id.index()].ty,
        MIRPlace::Global(id) => ctx.unit().global(id).unwrap().ty,
    }
}

pub(super) fn register_decl_type(
    ctx: &FunctionLoweringContext<'_>,
    register: MIRRegisterID,
) -> MIRTypeID {
    ctx.register_type(register)
}

pub(super) fn register_value(
    ctx: &FunctionLoweringContext<'_>,
    register: MIRRegisterID,
) -> LMIRValue {
    LMIRValue::Register {
        register: register_id(register),
        _type: lowered_type(ctx, register_decl_type(ctx, register)),
    }
}

pub(super) fn allocate_temp(
    ctx: &mut FunctionLoweringContext<'_>,
    ty: &LMIRType,
    alignment: u8,
) -> LMIRValue {
    emit_temp(
        ctx,
        LMIRInstructionKind::Allocate {
            _type: ty.clone(),
            alignment,
        },
        LMIRType::default_pointer(ctx.types().architecture()),
    )
}

pub(super) fn offset_address(
    ctx: &mut FunctionLoweringContext<'_>,
    base: LMIRValue,
    offset: usize,
    pointee: &LMIRType,
) -> LMIRValue {
    if offset == 0 {
        return base;
    }
    let right = int_constant(ctx, offset as i128, LMIRIntegerType::I64);
    emit_temp(
        ctx,
        LMIRInstructionKind::PointerBinOp {
            op: LMIRPtrBinOp::ADD,
            ptr_type: pointee.clone(),
            type_size: TypeSize::from(1),
            left: base,
            right,
        },
        LMIRType::default_pointer(ctx.types().architecture()),
    )
}

pub(super) fn emit_to(
    ctx: &mut FunctionLoweringContext<'_>,
    register: MIRRegisterID,
    kind: LMIRInstructionKind,
) {
    let ty = lowered_type(ctx, register_decl_type(ctx, register));
    emit_kind_to(ctx, register, kind, ty);
}

pub(super) fn emit_kind_to(
    ctx: &mut FunctionLoweringContext<'_>,
    register: MIRRegisterID,
    kind: LMIRInstructionKind,
    ty: LMIRType,
) {
    ctx.current_block_body_mut().push(LMIRInstruction {
        kind,
        value_type: ty,
        result: Some(register_id(register)),
    });
}

pub(super) fn emit_temp(
    ctx: &mut FunctionLoweringContext<'_>,
    kind: LMIRInstructionKind,
    ty: LMIRType,
) -> LMIRValue {
    let register = LMIRRegister::new(format!("t{}", ctx.next_temp()));
    ctx.current_block_body_mut().push(LMIRInstruction {
        kind,
        value_type: ty.clone(),
        result: Some(register.clone()),
    });
    LMIRValue::Register {
        register,
        _type: ty,
    }
}

pub(super) fn emit_void(ctx: &mut FunctionLoweringContext<'_>, kind: LMIRInstructionKind) {
    ctx.current_block_body_mut().push(LMIRInstruction {
        kind,
        value_type: LMIRType::unit(),
        result: None,
    });
}

pub(super) fn lowered_type(ctx: &FunctionLoweringContext<'_>, ty: MIRTypeID) -> LMIRType {
    convert_type(ty, ctx.types())
}

pub(super) fn mir_layout(
    ctx: &FunctionLoweringContext<'_>,
    ty: MIRTypeID,
) -> cx_mir::MIRTypeLayout {
    ctx.types().resolve_type_id(ty).unwrap().layout
}

pub(super) fn int_constant(
    ctx: &FunctionLoweringContext<'_>,
    value: i128,
    ty: LMIRIntegerType,
) -> LMIRValue {
    LMIRValue::IntImmediate {
        val: value as i64,
        _type: LMIRType::with_implicit_abi(ctx.types().architecture(), LMIRTypeKind::Integer(ty)),
    }
}

pub(super) fn integer_kind(ctx: &FunctionLoweringContext<'_>, ty: MIRTypeID) -> LMIRIntegerType {
    match ctx.types().kind(ty).unwrap() {
        cx_mir::MIRTypeKind::Integer { ty, .. } => convert_integer_type(*ty),
        _ => convert_integer_type(ctx.types().pointer_integer_type()),
    }
}

pub(super) fn block_id(block: MIRBasicBlockID) -> CXIdent {
    CXIdent::new(format!("b{}", block.index()))
}

pub(super) fn register_id(register: MIRRegisterID) -> LMIRRegister {
    LMIRRegister::new(format!("r{}", register.index()))
}
