use std::collections::HashMap;

use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRPlaceID};

use crate::framework::environment::AnalysisEnvironment;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LatticeState<T: Clone> {
    Bottom,
    Known(T),
    Top,
}

pub trait Mergeable: Clone {
    type Context;

    fn merge(
        &mut self,
        context: &Self::Context,
        other: &Self,
        place: MIRPlaceID,
    ) -> CXResult<LatticeState<Self>>
    where
        Self: Sized;
}

impl<T: Mergeable> LatticeState<T> {
    pub fn merge(
        &mut self,
        context: &T::Context,
        other: &LatticeState<T>,
        place: MIRPlaceID,
    ) -> CXResult<()> {
        match (self, other) {
            (LatticeState::Bottom, LatticeState::Bottom) => {}
            (LatticeState::Bottom, LatticeState::Known(value)) => {
                *self = LatticeState::Known(value.clone());
            }
            (LatticeState::Bottom, LatticeState::Top) => {
                *self = LatticeState::Top;
            }
            (LatticeState::Known(value), LatticeState::Bottom) => {}
            (LatticeState::Known(value), LatticeState::Known(other_value)) => {
                let merged = value.merge(context, other_value, place)?;
                *self = merged;
            }
            (LatticeState::Known(_), LatticeState::Top) => {
                *self = LatticeState::Top;
            }
            (LatticeState::Top, _) => {}
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StateTable<State: Mergeable> {
    snapshots: HashMap<MIRBasicBlockID, Box<[(MIRPlaceID, LatticeState<State>)]>>,

    places: Vec<(MIRPlaceID, LatticeState<State>)>,
    place_map: HashMap<MIRPlaceID, usize>,
}

impl<State: Clone + Mergeable> StateTable<State> {
    pub fn new() -> Self {
        Self
    }

    pub fn reload_block(&mut self, block: MIRBasicBlockID) {
        let Some(snapshot) = self.snapshots.get(&block) else {
            unreachable!("No snapshot found for block {:?}", block);
        };

        self.places.clear();
        self.place_map.clear();

        for (place, state) in snapshot.iter() {
            let index = self.places.len();
            self.places.push((*place, state.clone()));
            self.place_map.insert(*place, index);
        }
    }

    pub fn merge(
        &mut self,
        env: &AnalysisEnvironment,
        context: &State::Context,
        other: MIRBasicBlockID,
    ) -> CXResult<()> {
        let Some(other) = self.snapshots.get(&other) else {
            unreachable!("No snapshot found for block {:?}", other);
        };

        for (place, state) in &other.places {
            let index = self.place_map.entry(*place).or_insert_with(|| {
                let index = self.places.len();
                self.places.push((*place, LatticeState::Bottom));
                index
            });

            self.places[*index].1.merge(context, state, place)?;
        }

        Ok(())
    }

    pub fn get(&self, block: MIRPlaceID) -> Option<&LatticeState<State>> {
        let index = self.place_map.get(&block)?;
        self.places.get(*index).map(|(_, state)| state)
    }

    pub fn get_mut(&mut self, block: MIRPlaceID) -> &mut LatticeState<State> {
        let index = self.place_map.entry(block).or_insert_with(|| {
            let index = self.places.len();
            self.places.push((block, LatticeState::Bottom));
            index
        });

        &mut self.places[*index].1
    }
}
