use super::{
    environment::{Analysis, Context, Environment, Location},
    instruction::Instruction,
    state::State,
};
use cx_log::CXResult;
use cx_mir::{MIRRegister, MIRValue};
use std::any::Any;

pub(crate) trait Snapshot {
    fn data(&self) -> &dyn Any;
    fn merge(&mut self, incoming: &dyn Snapshot) -> bool;
}

impl<S: State> Snapshot for S {
    fn data(&self) -> &dyn Any {
        self
    }
    fn merge(&mut self, incoming: &dyn Snapshot) -> bool {
        State::merge(
            self,
            incoming
                .data()
                .downcast_ref::<S>()
                .expect("analysis state type changed"),
        )
    }
}

pub(crate) trait Factory {
    fn create(&self, context: &Context<'_>) -> Option<Box<dyn Active>>;
}

impl<A: Analysis> Factory for A {
    fn create(&self, context: &Context<'_>) -> Option<Box<dyn Active>> {
        Analysis::create(self, context).map(|environment| Box::new(environment) as Box<dyn Active>)
    }
}

pub(crate) trait Active {
    fn snapshot(&self) -> Box<dyn Snapshot>;
    fn restore(&mut self, state: &dyn Snapshot);
    fn instruction(
        &mut self,
        context: &Context<'_>,
        location: Location,
        instruction: Instruction<'_>,
        diagnose: bool,
    ) -> CXResult<()>;
    fn edge(
        &mut self,
        context: &Context<'_>,
        location: Location,
        args: &[MIRValue],
        params: &[MIRRegister],
        diagnose: bool,
    ) -> CXResult<()>;
    fn validate(&self, context: &Context<'_>, location: Location) -> CXResult<()>;
}

impl<E: Environment> Active for E {
    fn snapshot(&self) -> Box<dyn Snapshot> {
        Box::new(Environment::snapshot(self))
    }
    fn restore(&mut self, state: &dyn Snapshot) {
        Environment::restore(
            self,
            state
                .data()
                .downcast_ref::<E::State>()
                .expect("analysis state type changed"),
        );
    }
    fn instruction(
        &mut self,
        context: &Context<'_>,
        location: Location,
        instruction: Instruction<'_>,
        diagnose: bool,
    ) -> CXResult<()> {
        Environment::instruction(self, context, location, instruction, diagnose)
    }
    fn edge(
        &mut self,
        context: &Context<'_>,
        location: Location,
        args: &[MIRValue],
        params: &[MIRRegister],
        diagnose: bool,
    ) -> CXResult<()> {
        Environment::edge(self, context, location, args, params, diagnose)
    }
    fn validate(&self, context: &Context<'_>, location: Location) -> CXResult<()> {
        Environment::validate(self, context, location)
    }
}
