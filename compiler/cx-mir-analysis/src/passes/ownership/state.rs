use crate::framework::{
    environment::Context,
    state::{Fact, Table},
};
use cx_mir::MIRPlaceID;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Availability {
    Uninitialized,
    Available,
    Moved,
}

pub struct OwnershipEnvironment {
    pub(super) places: Table<Availability>,
    local_count: usize,
}

impl OwnershipEnvironment {
    pub(super) fn new(context: &Context<'_>) -> Self {
        let mut environment = Self {
            places: Table::default(),
            local_count: context
                .function
                .body()
                .map_or(0, |body| body.places().len()),
        };
        for place in context
            .function
            .body()
            .map(|body| body.parameters())
            .into_iter()
            .flatten()
            .copied()
        {
            environment.insert(place, Fact::Known(Availability::Available));
        }
        environment
    }

    fn index(&self, place: MIRPlaceID) -> Option<usize> {
        (place.index() < self.local_count).then_some(place.index())
    }

    pub(super) fn place(&self, index: usize) -> MIRPlaceID {
        MIRPlaceID::new(index)
    }

    pub(super) fn get(&self, place: &MIRPlaceID) -> Option<&Fact<Availability>> {
        self.index(*place).map(|index| self.places.get(index))
    }

    pub(super) fn insert(&mut self, place: MIRPlaceID, state: Fact<Availability>) {
        if let Some(index) = self.index(place) {
            self.places.insert(index, state);
        }
    }

    pub(super) fn remove(&mut self, place: &MIRPlaceID) {
        self.insert(*place, Fact::Known(Availability::Uninitialized));
    }

    pub(super) fn mark_moved(&mut self, place: MIRPlaceID) {
        self.insert(place, Fact::Known(Availability::Moved));
    }
}
