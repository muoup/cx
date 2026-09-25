use crate::lowering::memory;
use cx_lmir::{LMIRBasicBlock, LMIRBlockTarget, LMIRInstruction, LMIRInstructionKind};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRConstant, MIRInstruction, MIRInstructionKind, MIRIntType, MIRTarget, MIRTypeKind, MIRValue,
};
use cx_util::identifier::CXIdent;

use crate::context::FunctionContext;

use super::{calls, intrinsics, values};

pub(super) fn lower_instruction(
    context: &mut FunctionContext<'_, '_>,
    instruction: &MIRInstruction,
) {
    match &instruction.kind {
        MIRInstructionKind::Initialize { .. }
        | MIRInstructionKind::Invalidate { .. }
        | MIRInstructionKind::BindLifetime { .. } => {}
        MIRInstructionKind::Lift { out, source } => {
            let ty = values::target_type(context, *source);
            if let MIRTarget::Indirect(register) = source {
                if context.bitfields.contains_key(register) {
                    let value = values::lower_read(context, &MIRValue::Register(*register));
                    values::write_target(context, MIRTarget::Register(*out), value);
                    return;
                }
            }
            let source = match source {
                MIRTarget::Place(place) => context.places[place].clone(),
                MIRTarget::Global(reference) => values::global_address(context, *reference),
                MIRTarget::Indirect(register) => context.reg(*register),
                MIRTarget::Register(_) => unreachable!("lift source must be addressable"),
            };
            if context.ty(ty).is_void() {
                memory::assign(
                    context,
                    *out,
                    LMIRInstructionKind::Alias {
                        value: cx_lmir::LMIRValue::NULL,
                    },
                );
            } else if context.ty(ty).is_memory_resident() {
                let copy = memory::allocate(context, ty);
                memory::store(context, copy.clone(), source, ty);
                memory::assign(context, *out, LMIRInstructionKind::Alias { value: copy });
            } else {
                memory::assign(
                    context,
                    *out,
                    LMIRInstructionKind::Load {
                        memory: source,
                        _type: context.ty(ty),
                    },
                );
            }
        }
        MIRInstructionKind::Store { target, value, ty } => {
            let source = values::lower_rvalue(context, value, *ty);
            if matches!(target, cx_mir::MIRTarget::Register(_))
                && context.ty(*ty).is_memory_resident()
            {
                let copy = memory::allocate(context, *ty);
                memory::store(context, copy.clone(), source, *ty);
                values::write_target(context, *target, copy);
            } else {
                values::write_target(context, *target, source);
            }
        }
        MIRInstructionKind::Call { out, callee, args } => {
            calls::lower_call(context, *out, callee, args)
        }
        MIRInstructionKind::IntrinsicOp(op) => intrinsics::lower_intrinsic(context, op),
        MIRInstructionKind::Return { value } => calls::lower_return(context, value.as_ref()),
        MIRInstructionKind::Jump { target } => {
            let target = context.target(target);
            memory::void(context, LMIRInstructionKind::Jump { target });
        }
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            let condition = values::lower_read(context, cond);
            let true_target = context.target(true_target);
            let false_target = context.target(false_target);
            memory::void(
                context,
                LMIRInstructionKind::Branch {
                    condition,
                    true_target,
                    false_target,
                },
            );
        }
        MIRInstructionKind::CaseBranch {
            value,
            cases,
            default,
        } => {
            let integer_type = case_integer_type(context, value);
            let value = values::lower_read(context, value);
            let targets = cases
                .iter()
                .filter_map(|(case, target)| {
                    if integer_type.is_some_and(|(ty, signed)| !case_fits(ty, signed, *case)) {
                        return None;
                    }
                    let case = integer_type
                        .map(|(ty, signed)| case_entry(ty, signed, *case))
                        .unwrap_or(*case as u64);
                    Some((case, context.target(target)))
                })
                .collect();
            let default = default
                .as_ref()
                .map(|target| context.target(target))
                .unwrap_or_else(|| unreachable_target(context));
            memory::void(
                context,
                LMIRInstructionKind::JumpTable {
                    value,
                    targets,
                    default,
                },
            );
        }
        MIRInstructionKind::Unreachable => memory::void(context, LMIRInstructionKind::Unreachable),
    }
}

fn case_integer_type(
    context: &FunctionContext<'_, '_>,
    value: &MIRValue,
) -> Option<(MIRIntType, bool)> {
    let mut ty = match value {
        MIRValue::Register(register) => context.body.register(*register)?.ty,
        MIRValue::PlaceRef(place) => context.body.place(*place)?.ty,
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => reference.ty,
        MIRValue::Constant(MIRConstant::Integer { .. }) => return None,
        MIRValue::Constant(_) => return None,
    };
    loop {
        match context.types().definition(ty)?.kind() {
            MIRTypeKind::MemoryReference { inner, .. } => ty = *inner,
            MIRTypeKind::Integer { ty, signed } => return Some((*ty, *signed)),
            _ => return None,
        }
    }
}

fn case_fits(ty: MIRIntType, signed: bool, case: i128) -> bool {
    let width = integer_width(ty);
    if width == 128 {
        return signed || case >= 0;
    }
    if signed {
        let limit = 1i128 << (width - 1);
        case >= -limit && case < limit
    } else {
        case >= 0 && case < (1i128 << width)
    }
}

fn case_entry(ty: MIRIntType, signed: bool, case: i128) -> u64 {
    let width = integer_width(ty);
    if signed && case < 0 && width < 64 {
        (case as u64) & ((1u64 << width) - 1)
    } else {
        case as u64
    }
}

fn integer_width(ty: MIRIntType) -> usize {
    match ty {
        MIRIntType::I1 => 1,
        MIRIntType::I8 => 8,
        MIRIntType::I16 => 16,
        MIRIntType::I32 => 32,
        MIRIntType::I64 => 64,
        MIRIntType::I128 => 128,
    }
}

fn unreachable_target(context: &mut FunctionContext<'_, '_>) -> LMIRBlockTarget {
    let id = CXIdent::new(format!("unreachable.{}", context.blocks.len()));
    context.blocks.push(LMIRBasicBlock {
        id: id.clone(),
        debug_name: None,
        params: Vec::new(),
        body: vec![LMIRInstruction {
            kind: LMIRInstructionKind::Unreachable,
            value_type: cx_lmir::types::LMIRType::unit(),
            result: None,
        }],
    });
    LMIRBlockTarget::new(id)
}
