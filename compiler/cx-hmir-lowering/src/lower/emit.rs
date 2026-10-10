// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
// 
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;

use crate::env::function::MIRFunctionBuilder;

pub fn jump(
    builder: &mut MIRFunctionBuilder,
    target: MIRBasicBlockID,
    args: Vec<MIRValue>,
    span: TokenRange,
) {
    builder.emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::with_args(target, args),
        },
        span,
    )
}
