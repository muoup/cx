use std::collections::{HashMap, HashSet};

use cx_mir::{
    MIRBasicBlock, MIRBindable, MIRInstructionKind, MIRPlaceID, MIRRegisterID, MIRTarget,
    MIRValue, expr::visit::visit_bindable_uses,
};

pub(super) struct PreservationPlan {
    before: Vec<Vec<MIRRegisterID>>,
}

impl PreservationPlan {
    pub fn before(&self, instruction: usize) -> &[MIRRegisterID] {
        &self.before[instruction]
    }
}

pub(super) fn plan(block: &MIRBasicBlock) -> PreservationPlan {
    let instructions = block.instructions();
    let mut last_use = HashMap::new();
    for (index, instruction) in instructions.iter().enumerate() {
        visit_bindable_uses(&instruction.kind, |bindable| {
            if let MIRBindable::Register(register) = bindable {
                last_use.insert(register, index);
            }
        });
    }

    let mut active: HashMap<MIRRegisterID, (MIRPlaceID, bool)> = HashMap::new();
    let mut before = vec![Vec::new(); instructions.len()];
    for (index, instruction) in instructions.iter().enumerate() {
        let kind = &instruction.kind;
        let boundary_uses = block_argument_registers(kind);
        let touched = touched_places(kind);
        let aliases_all = matches!(
            kind,
            MIRInstructionKind::Store { .. } | MIRInstructionKind::Call { .. }
        ) || matches!(kind, MIRInstructionKind::IntrinsicOp(op)
                if matches!(op.output_target(), Some(MIRTarget::Place(_) | MIRTarget::Indirect(_))))
            || matches!(
                kind,
                MIRInstructionKind::IntrinsicOp(cx_mir::MIRIntrinsic::VA(_))
            );
        let write_may_precede_read = matches!(kind, MIRInstructionKind::Call { .. })
            || matches!(
                kind,
                MIRInstructionKind::IntrinsicOp(cx_mir::MIRIntrinsic::VA(_))
            );

        for (register, (place, preserved)) in &mut active {
            if *preserved {
                continue;
            }
            let used_later = last_use.get(register).is_some_and(|last| *last > index);
            let call_uses_now =
                write_may_precede_read && last_use.get(register).is_some_and(|last| *last >= index);
            let crosses_block = boundary_uses.contains(register);
            if (used_later && (aliases_all || touched.contains(place)))
                || call_uses_now
                || crosses_block
            {
                before[index].push(*register);
                *preserved = true;
            }
        }

        match kind {
            MIRInstructionKind::LiftPlace { out, place } => {
                active.insert(*out, (*place, false));
            }
            MIRInstructionKind::Forward { out, source } => {
                if let Some(backing) = active.get(source).copied() {
                    active.insert(*out, backing);
                }
            }
            _ => {}
        }
    }

    PreservationPlan { before }
}

fn touched_places(kind: &MIRInstructionKind) -> HashSet<MIRPlaceID> {
    match kind {
        MIRInstructionKind::Store { target, .. } => HashSet::from([*target]),
        MIRInstructionKind::Invalidate {
            place: cx_mir::MIRBindable::Place(place),
            kind: cx_mir::expr::instruction::MIRInvalidationKind::Drop,
        } => HashSet::from([*place]),
        MIRInstructionKind::IntrinsicOp(op) => match op.output_target() {
            Some(MIRTarget::Place(place)) => HashSet::from([place]),
            _ => HashSet::new(),
        },
        _ => HashSet::new(),
    }
}

fn block_argument_registers(kind: &MIRInstructionKind) -> HashSet<MIRRegisterID> {
    let mut registers = HashSet::new();
    let mut target = |args: &[MIRValue]| {
        for arg in args {
            if let MIRValue::Register(register) = arg {
                registers.insert(*register);
            }
        }
    };
    match kind {
        MIRInstructionKind::Jump { target: edge } => target(&edge.args),
        MIRInstructionKind::Branch {
            true_target,
            false_target,
            ..
        } => {
            target(&true_target.args);
            target(&false_target.args);
        }
        MIRInstructionKind::CaseBranch { cases, default, .. } => {
            for (_, edge) in cases {
                target(&edge.args);
            }
            if let Some(edge) = default {
                target(&edge.args);
            }
        }
        _ => {}
    }
    registers
}
