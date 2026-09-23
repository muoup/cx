use std::{collections::HashMap, hash::Hash};

use cx_log::CXMaybeRawResult;
use cx_mir::MIRBasicBlockID;

use crate::framework::environment::AnalysisEnvironment;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LatticeState<Key: Clone, T: Clone> {
    Bottom,
    Known(T),
    Top,

    _PHANTOM(std::marker::PhantomData<Key>),
}

pub trait Mergeable<Key: Clone>: Clone {
    fn merge(
        &self,
        env: &AnalysisEnvironment,
        other: &Self,
        key: Key,
    ) -> CXMaybeRawResult<Option<LatticeState<Key, Self>>>
    where
        Self: Sized;
}

impl<Key: Clone, T: Mergeable<Key>> LatticeState<Key, T> {
    pub fn merge(
        &self,
        env: &AnalysisEnvironment,
        other: &LatticeState<Key, T>,
        place: Key,
    ) -> CXMaybeRawResult<Option<LatticeState<Key, T>>> {
        Ok(match (self, other) {
            (LatticeState::Bottom, LatticeState::Bottom) => None,
            (LatticeState::Top, _) => None,

            (LatticeState::Bottom | LatticeState::Known(_), LatticeState::Top) => {
                Some(LatticeState::Top)
            }

            (LatticeState::Bottom, known @ LatticeState::Known(_)) => Some(known.clone()),
            (known @ LatticeState::Known(_), LatticeState::Bottom) => Some(known.clone()),

            (LatticeState::Known(value), LatticeState::Known(other_value)) => {
                value.merge(env, other_value, place)?
            }

            _ => unreachable!("Invalid lattice state combination"),
        })
    }
}

#[derive(Debug, Clone)]
pub struct StateTable<Key: Hash + Eq + Clone, State: Mergeable<Key>> {
    snapshots: HashMap<MIRBasicBlockID, HashMap<Key, LatticeState<Key, State>>>,

    states: Vec<(Key, LatticeState<Key, State>)>,
    map: HashMap<Key, usize>,
}

impl<Key: Hash + Eq + Clone, State: Clone + Mergeable<Key>> StateTable<Key, State> {
    pub fn new() -> Self {
        Self {
            snapshots: HashMap::new(),
            states: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn reload_block(&mut self, block: MIRBasicBlockID) {
        let Some(snapshot) = self.snapshots.get(&block) else {
            unreachable!("No snapshot found for block {:?}", block);
        };

        self.states.clear();
        self.map.clear();

        for (key, state) in snapshot.iter() {
            let index = self.states.len();
            self.states.push((key.clone(), state.clone()));
            self.map.insert(key.clone(), index);
        }
    }

    pub fn merge_into(
        &mut self,
        env: &AnalysisEnvironment,
        other: MIRBasicBlockID,
    ) -> CXMaybeRawResult<bool> {
        let other = self
            .snapshots
            .entry(other)
            .or_insert_with(|| HashMap::new());

        let mut changed = false;
        for (key, state) in &self.states {
            let other_state = other.entry(key.clone()).or_insert(LatticeState::Bottom);
            if let Some(new_state) = other_state.merge(env, state, key.clone())? {
                *other_state = new_state;
                changed = true;
            }
        }
        Ok(changed)
    }

    pub fn get(&self, key: &Key) -> Option<&LatticeState<Key, State>> {
        let index = self.map.get(key)?;
        self.states.get(*index).map(|(_, state)| state)
    }

    pub fn get_mut(&mut self, key: Key) -> &mut LatticeState<Key, State> {
        let index = self.map.entry(key.clone()).or_insert_with(|| {
            let index = self.states.len();
            self.states.push((key, LatticeState::Bottom));
            index
        });

        &mut self.states[*index].1
    }

    pub fn set(&mut self, key: Key, state: LatticeState<Key, State>) {
        let index = self.map.entry(key.clone()).or_insert_with(|| {
            let index = self.states.len();
            self.states.push((key, LatticeState::Bottom));
            index
        });

        self.states[*index].1 = state;
    }
}
