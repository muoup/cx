use cx_lmir::{LMIRInstructionKind, LMIRValue};
use cx_mir::{MIRBindable, MIRRegister, MIRTypeKind, ty::interface::MTRegistry};

use crate::context::FunctionContext;

use super::memory;

// TODO: Elide aggregate lift copies whose source is not written before the lifted value's last use
pub(super) fn lower(context: &mut FunctionContext<'_, '_>, out: MIRRegister, source: &MIRBindable) {
    let ty = match source {
        MIRBindable::Place(place) => context.body.place(*place).ty(),
        MIRBindable::Register(register) => {
            let pointer = context.body.register(*register).ty();
            match context.types().definition(pointer).unwrap().kind() {
                MIRTypeKind::MemoryReference { inner, .. } | MIRTypeKind::PointerTo { inner } => {
                    *inner
                }
                _ => panic!("lift source is not a reference"),
            }
        }
    };
    let address = match source {
        MIRBindable::Place(place) => context.places[place].clone(),
        MIRBindable::Register(register) => context.reg(*register),
    };
    if context.ty(ty).is_void() {
        memory::assign(
            context,
            out,
            LMIRInstructionKind::Alias {
                value: LMIRValue::NULL,
            },
        );
    } else if context.ty(ty).is_memory_resident() {
        let copy = memory::allocate(context, ty);
        memory::store(context, copy.clone(), address, ty);
        memory::assign(context, out, LMIRInstructionKind::Alias { value: copy });
    } else {
        memory::assign(
            context,
            out,
            LMIRInstructionKind::Load {
                memory: address,
                ty: context.ty(ty),
            },
        );
    }
}
