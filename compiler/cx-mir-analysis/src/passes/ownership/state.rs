use crate::framework::{
    environment::Context,
    instruction::AnalysisInstruction,
    state::{Fact, Table},
};
use cx_mir::{
    MIRAggregateOp, MIRBody, MIRFunctionBody, MIRInstrKind, MIRParameterID, MIRPlace,
    MIRPlaceAggregateOp, MIRPlaceID,
};
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Availability {
    Uninitialized,
    Available,
    Moved,
}

pub struct OwnershipEnvironment {
    pub(super) places: Table<Availability>,
    local_count: usize,
    projections: BTreeMap<MIRPlace, MIRPlace>,
}

impl OwnershipEnvironment {
    pub(super) fn new(context: &Context<'_>) -> Self {
        let mut environment = Self {
            places: Table::default(),
            local_count: context
                .function
                .body()
                .map_or(0, |body| body.places().len()),
            projections: BTreeMap::new(),
        };
        match context.function.body() {
            Some(MIRFunctionBody::Runtime(body)) => environment.projections(body),
            Some(MIRFunctionBody::Comptime(body)) => environment.projections(body),
            None => {}
        }
        for index in 0..context.function.prototype().signature.params.len() {
            environment.insert(
                MIRPlace::Parameter(MIRParameterID::new(index)),
                Fact::Known(Availability::Available),
            );
        }
        environment
    }

    fn projections<K: AnalysisInstruction>(&mut self, body: &MIRBody<K>) {
        for block in body.blocks() {
            for instruction in &block.instrs {
                if let Some(MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, op })) =
                    instruction.kind.view().standard()
                {
                    let base = match op {
                        MIRPlaceAggregateOp::Field { base, .. }
                        | MIRPlaceAggregateOp::Index { base, .. }
                        | MIRPlaceAggregateOp::Variant { base, .. } => base,
                    };
                    self.projections.insert(*out, *base);
                }
            }
        }
    }

    fn index(&self, place: MIRPlace) -> Option<usize> {
        match place {
            MIRPlace::FunctionLocal(id) => Some(id.index()),
            MIRPlace::Parameter(id) => Some(self.local_count + id.index()),
            MIRPlace::Global(_) => None,
        }
    }

    pub(super) fn place(&self, index: usize) -> MIRPlace {
        if index < self.local_count {
            MIRPlace::FunctionLocal(MIRPlaceID::new(index))
        } else {
            MIRPlace::Parameter(MIRParameterID::new(index - self.local_count))
        }
    }

    pub(super) fn get(&self, place: &MIRPlace) -> Option<&Fact<Availability>> {
        self.index(*place).map(|index| self.places.get(index))
    }

    pub(super) fn insert(&mut self, place: MIRPlace, state: Fact<Availability>) {
        if let Some(index) = self.index(place) {
            self.places.insert(index, state);
        }
    }

    pub(super) fn remove(&mut self, place: &MIRPlace) {
        self.insert(*place, Fact::Known(Availability::Uninitialized));
    }

    pub(super) fn mark_moved(&mut self, place: MIRPlace) {
        self.insert(place, Fact::Known(Availability::Moved));
        let mut current = place;
        while let Some(base) = self.projections.get(&current).copied() {
            self.insert(base, Fact::Known(Availability::Moved));
            current = base;
        }
    }
}
