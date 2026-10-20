use std::collections::HashMap;

use cx_log::{
    CXResult,
    catalogue::mir,
    error::{CXError, context::from_token_range},
};

use crate::{
    MIRBindable, MIRBody, MIRInstructionKind, MIRRegister, MIRTarget,
    expr::visit::visit_bindable_uses,
};

// Lifted values are immutable snapshots local to their defining block; unused snapshots are permitted.
pub fn lifts(body: &MIRBody) -> CXResult<()> {
    let mut definitions = HashMap::new();
    for (block, data) in body.blocks() {
        for (index, instruction) in data.instructions().iter().enumerate() {
            if let MIRInstructionKind::Lift { out, .. } = &instruction.kind {
                if definitions.insert(*out, (block, index)).is_some() {
                    return Err(CXError::new(
                        mir::MIR_LIFT_REDEFINED.bind(out.to_string()),
                        from_token_range(&instruction.token_range),
                    ));
                }
            }
        }
    }

    if definitions.is_empty() {
        return Ok(());
    }

    for (block, data) in body.blocks() {
        for (index, instruction) in data.instructions().iter().enumerate() {
            if let Some(out) = output(&instruction.kind)
                && let Some(definition) = definitions.get(&out)
                && *definition != (block, index)
            {
                return Err(CXError::new(
                    mir::MIR_LIFT_REDEFINED.bind(out.to_string()),
                    from_token_range(&instruction.token_range),
                ));
            }

            let mut unavailable = None;
            let mut check = |binding| {
                if let MIRBindable::Register(register) = binding
                    && let Some((owner, position)) = definitions.get(&register)
                    && (*owner != block || *position >= index)
                {
                    unavailable = Some(register);
                }
            };
            visit_bindable_uses(&instruction.kind, &mut check);
            match &instruction.kind {
                MIRInstructionKind::Initialize { place }
                | MIRInstructionKind::Invalidate { place, .. } => check(place.clone()),
                MIRInstructionKind::Lift { origin, .. } => check(origin.clone()),
                _ => {}
            }
            if let Some(register) = unavailable {
                return Err(CXError::new(
                    mir::MIR_UNDEFINED_REGISTER.bind(register.to_string()),
                    from_token_range(&instruction.token_range),
                ));
            }
        }
    }
    Ok(())
}

fn output(kind: &MIRInstructionKind) -> Option<MIRRegister> {
    match kind {
        MIRInstructionKind::Lift { out, .. }
        | MIRInstructionKind::Call { out: Some(out), .. }
        | MIRInstructionKind::Store {
            target: MIRTarget::Register(out),
            ..
        } => Some(*out),
        MIRInstructionKind::IntrinsicOp(op) => match op.output_target() {
            Some(MIRTarget::Register(out)) => Some(out),
            _ => None,
        },
        _ => None,
    }
}
