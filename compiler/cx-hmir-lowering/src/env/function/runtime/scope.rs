use cx_hmir::{HMIRUnitID, expr::HMIRExprID, unit::def::HMIRDeclarationPoint};
use cx_mir::{MIRBasicBlockID, MIRScopeID};

use crate::{env::function::Expectations, value::HMIRValue};

#[derive(Debug)]
pub(crate) struct Exit {
    break_block: MIRBasicBlockID,
    continue_block: Option<MIRBasicBlockID>,
    cases: Vec<(HMIRExprID, MIRBasicBlockID)>,
}

impl Exit {
    pub(crate) fn new(
        break_block: MIRBasicBlockID,
        continue_block: Option<MIRBasicBlockID>,
        cases: Vec<(HMIRExprID, MIRBasicBlockID)>,
    ) -> Self {
        Self {
            break_block,
            continue_block,
            cases,
        }
    }

    pub(crate) fn break_block(&self) -> MIRBasicBlockID {
        self.break_block
    }

    pub(crate) fn continue_block(&self) -> Option<MIRBasicBlockID> {
        self.continue_block
    }

    pub(crate) fn case_block(&self, case: HMIRExprID) -> Option<MIRBasicBlockID> {
        self.cases
            .iter()
            .find_map(|(label, block)| (*label == case).then_some(*block))
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Defer {
    expr: HMIRExprID,
    expectations: Expectations,
    source: HMIRUnitID,
    visible_through: Option<HMIRDeclarationPoint>,
}

impl Defer {
    pub(crate) fn new(
        expr: HMIRExprID,
        expectations: Expectations,
        source: HMIRUnitID,
        visible_through: Option<HMIRDeclarationPoint>,
    ) -> Self {
        Self {
            expr,
            expectations,
            source,
            visible_through,
        }
    }

    pub(crate) fn expr(&self) -> HMIRExprID {
        self.expr
    }

    pub(crate) fn expectations(&self) -> Expectations {
        self.expectations
    }

    pub(crate) fn source(&self) -> HMIRUnitID {
        self.source
    }

    pub(crate) fn visible_through(&self) -> Option<HMIRDeclarationPoint> {
        self.visible_through
    }
}

#[derive(Debug, Default)]
pub(crate) struct RuntimeScope {
    mir: Option<MIRScopeID>,
    deferred: bool,
    defers: Vec<Defer>,
    exit: Option<Exit>,
    returning: Option<(HMIRUnitID, Option<HMIRDeclarationPoint>)>,
    yields: Option<Vec<(MIRBasicBlockID, HMIRValue)>>,
}

impl RuntimeScope {
    pub(crate) fn new(mir: Option<MIRScopeID>, deferred: bool) -> Self {
        Self {
            mir,
            deferred,
            ..Self::default()
        }
    }

    pub(crate) fn mir(&self) -> Option<MIRScopeID> {
        self.mir
    }

    pub(crate) fn deferred(&self) -> bool {
        self.deferred
    }

    pub(crate) fn defers(&self) -> &[Defer] {
        &self.defers
    }

    pub(crate) fn defer(&mut self, defer: Defer) {
        self.defers.push(defer);
    }

    pub(crate) fn exit(&self) -> Option<&Exit> {
        self.exit.as_ref()
    }

    pub(crate) fn set_exit(&mut self, exit: Exit) {
        self.exit = Some(exit);
    }

    pub(crate) fn set_return(&mut self, source: HMIRUnitID, point: Option<HMIRDeclarationPoint>) {
        self.returning = Some((source, point));
    }

    pub(crate) fn returning(&self) -> Option<(HMIRUnitID, Option<HMIRDeclarationPoint>)> {
        self.returning
    }

    pub(crate) fn yields(&self) -> bool {
        self.yields.is_some()
    }

    pub(crate) fn begin_yield(&mut self) {
        self.yields = Some(Vec::new());
    }

    pub(crate) fn push_yield(&mut self, block: MIRBasicBlockID, value: HMIRValue) {
        self.yields
            .as_mut()
            .expect("a yield targets a yielding scope")
            .push((block, value));
    }

    pub(crate) fn end_yield(&mut self) -> Vec<(MIRBasicBlockID, HMIRValue)> {
        self.yields.take().expect("a yielding scope is open")
    }
}
