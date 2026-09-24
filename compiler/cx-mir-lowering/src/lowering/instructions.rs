use crate::lowering::memory;
use cx_lmir::{LMIRBasicBlock, LMIRBlockTarget, LMIRInstruction, LMIRInstructionKind};
use cx_mir::{MIRInstruction, MIRInstructionKind, MIRTarget};
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
            let value = values::lower_read(context, value);
            let targets = cases
                .iter()
                .map(|(case, target)| (*case as u64, context.target(target)))
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
