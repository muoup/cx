use cx_log::CXMaybeRawResult;
use cx_mir::MIRBasicBlockID;

/// Dense per-function dataflow state: one slot per tracked key, plus a snapshot of the
/// incoming state for every block that has been reached.
#[derive(Debug, Clone)]
pub struct StateTable<State: Copy + PartialEq> {
    states: Vec<State>,
    snapshots: Vec<Option<Vec<State>>>,
}

impl<State: Copy + PartialEq> StateTable<State> {
    pub fn new() -> Self {
        Self {
            states: Vec::new(),
            snapshots: Vec::new(),
        }
    }

    pub fn reset(&mut self, initial: State, key_count: usize, block_count: usize) {
        self.states.clear();
        self.states.resize(key_count, initial);
        self.snapshots.clear();
        self.snapshots.resize(block_count, None);
    }

    pub fn reload_block(&mut self, block: MIRBasicBlockID) {
        let Some(Some(snapshot)) = self.snapshots.get(block.index()) else {
            unreachable!("No snapshot found for block {:?}", block);
        };

        self.states.clone_from(snapshot);
    }

    /// Joins the current state into the snapshot of `block`, returning whether it changed.
    /// `join` is only invoked for keys whose states differ.
    pub fn merge_into(
        &mut self,
        block: MIRBasicBlockID,
        mut join: impl FnMut(usize, State, State) -> CXMaybeRawResult<State>,
    ) -> CXMaybeRawResult<bool> {
        let snapshot = &mut self.snapshots[block.index()];
        let Some(snapshot) = snapshot else {
            *snapshot = Some(self.states.clone());
            return Ok(true);
        };

        let mut changed = false;
        for (index, (existing, incoming)) in snapshot.iter_mut().zip(&self.states).enumerate() {
            if existing == incoming {
                continue;
            }

            let joined = join(index, *existing, *incoming)?;
            if joined != *existing {
                *existing = joined;
                changed = true;
            }
        }
        Ok(changed)
    }

    pub fn get(&self, key: usize) -> Option<State> {
        self.states.get(key).copied()
    }

    pub fn set(&mut self, key: usize, state: State) {
        self.states[key] = state;
    }
}
