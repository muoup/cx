use cx_lmir::{LMIRInstructionKind, LMIRValue};
use cx_mir::{MIRRegister, MIRTarget};

use crate::context::FunctionContext;

use super::{
    memory,
    values::{target_address, target_type},
};

// TODO: Elide aggregate lift copies whose source is not written before the lifted value's last use
pub(super) fn lower(context: &mut FunctionContext<'_, '_>, out: MIRRegister, source: &MIRTarget) {
    let ty = target_type(context, source);
    let address = target_address(context, source);
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
