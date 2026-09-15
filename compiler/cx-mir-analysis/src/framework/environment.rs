use cx_log::CXResult;
use cx_mir::{MIRBasicBlockID, MIRFunction, MIRPlace, MIRRegister, MIRUnit, MIRValue};

use super::{instruction::Instruction, state::State};

pub struct Context<'a> {
    pub unit: &'a MIRUnit,
    pub function: &'a MIRFunction,
}

impl Context<'_> {
    pub fn place_index(&self, place: MIRPlace) -> usize {
        let locals = self.function.body().map_or(0, |body| body.places().len());
        match place {
            MIRPlace::FunctionLocal(id) => id.index(),
            MIRPlace::Parameter(id) => locals + id.index(),
            MIRPlace::Global(id) => {
                locals + self.function.prototype().signature.params.len() + id.index()
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct Location {
    pub block: MIRBasicBlockID,
    pub instruction: usize,
}

pub trait Analysis: 'static {
    type Environment: Environment;
    fn create(&self, context: &Context<'_>) -> Option<Self::Environment>;
}

pub trait Environment: 'static {
    type State: State;
    fn snapshot(&self) -> Self::State;
    fn restore(&mut self, state: &Self::State);
    fn instruction(
        &mut self,
        context: &Context<'_>,
        location: Location,
        instruction: Instruction<'_>,
        diagnose: bool,
    ) -> CXResult<()>;
    fn edge(
        &mut self,
        _context: &Context<'_>,
        _location: Location,
        _args: &[MIRValue],
        _params: &[MIRRegister],
        _diagnose: bool,
    ) -> CXResult<()> {
        Ok(())
    }
    fn validate(&self, _context: &Context<'_>, _location: Location) -> CXResult<()> {
        Ok(())
    }
}
