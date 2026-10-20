use std::collections::{BTreeMap, HashMap};

use cx_lmir::{LMIRInstructionKind, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    expr::visit::visit_bindable_uses, MIRBasicBlock, MIRBindable, MIRInstructionKind, MIRPlaceID,
    MIRRegister, MIRTarget, MIRValue,
};

use crate::context::FunctionContext;

use super::{memory, values};

struct Snapshot {
    source: Option<MIRPlaceID>,
    value: LMIRValue,
    last_use: usize,
}

#[derive(Default)]
pub(crate) struct Lifts {
    snapshots: BTreeMap<MIRRegister, Snapshot>,
    last_uses: HashMap<MIRRegister, usize>,
}

impl Lifts {
    pub fn new(block: &MIRBasicBlock) -> Self {
        let mut state = Self::default();
        for (index, instruction) in block.instructions().iter().enumerate() {
            if let MIRInstructionKind::Lift { out, .. } = &instruction.kind {
                state.last_uses.insert(*out, index);
            }
        }
        if state.last_uses.is_empty() {
            return state;
        }
        for (index, instruction) in block.instructions().iter().enumerate() {
            visit_bindable_uses(&instruction.kind, |binding| {
                if let MIRBindable::Register(register) = binding {
                    if let Some(last_use) = state.last_uses.get_mut(&register) {
                        *last_use = index;
                    }
                }
            });
        }
        state
    }

    pub fn value(&self, register: MIRRegister) -> Option<LMIRValue> {
        self.snapshots
            .get(&register)
            .map(|snapshot| snapshot.value.clone())
    }

    pub fn retire(&mut self, index: usize) {
        self.snapshots
            .retain(|_, snapshot| snapshot.last_use > index);
    }
}

// Direct aggregate lifts share their source storage until a write or an escaping use needs a stable buffer.
// Indirect and adopted sources are copied immediately; scalar lifts always load at their definition.
pub(super) fn lower(context: &mut FunctionContext<'_, '_>, out: MIRRegister, source: &MIRBindable) {
    let target = match source {
        MIRBindable::Place(place) => MIRTarget::Place(*place),
        MIRBindable::Register(register) => MIRTarget::Indirect(*register),
    };
    let ty = values::target_type(context, target);
    let address = match source {
        MIRBindable::Place(place) => context.places[place].clone(),
        MIRBindable::Register(register) => context.value(*register),
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
        match source {
            MIRBindable::Place(place) if !context.body.place(*place).adopted() => {
                if let Some(last_use) = context.lifts.last_uses.get(&out).copied() {
                    context.lifts.snapshots.insert(
                        out,
                        Snapshot {
                            source: Some(*place),
                            value: address,
                            last_use,
                        },
                    );
                }
            }
            _ => {
                let copy = memory::allocate(context, ty);
                memory::store(context, copy.clone(), address, ty);
                memory::assign(context, out, LMIRInstructionKind::Alias { value: copy });
            }
        }
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

// Preserve before the physical write, rather than the following Initialize. Calls and intrinsics are barriers
// because they can mutate aliases or retain an address derived from a snapshot beyond its last direct use.
pub(super) fn before(context: &mut FunctionContext<'_, '_>, kind: &MIRInstructionKind) {
    let target = match kind {
        MIRInstructionKind::Call { .. } | MIRInstructionKind::IntrinsicOp(_) => None,
        MIRInstructionKind::Store {
            target, value, ty, ..
        } => {
            if let MIRValue::Register(register) = value {
                if context.lifts.snapshots.contains_key(register)
                    && !context
                        .types()
                        .same_type(context.body.register(*register).ty(), *ty)
                {
                    preserve(context, None);
                }
            }
            match target {
                MIRTarget::Place(place) if !context.body.place(*place).adopted() => Some(*place),
                MIRTarget::Register(_) => return,
                _ => None,
            }
        }
        _ => return,
    };
    preserve(context, target);
}

fn preserve(context: &mut FunctionContext<'_, '_>, target: Option<MIRPlaceID>) {
    let affected = context
        .lifts
        .snapshots
        .iter()
        .filter(|(_, snapshot)| {
            snapshot
                .source
                .is_some_and(|source| target.is_none_or(|target| source == target))
        })
        .map(|(register, snapshot)| (*register, snapshot.value.clone()))
        .collect::<Vec<_>>();
    for (register, source) in affected {
        let ty = context.body.register(register).ty();
        let copy = memory::allocate(context, ty);
        memory::store(context, copy.clone(), source, ty);
        let snapshot = context.lifts.snapshots.get_mut(&register).unwrap();
        snapshot.source = None;
        snapshot.value = copy;
    }
}
