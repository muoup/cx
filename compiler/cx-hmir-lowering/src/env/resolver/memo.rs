use cx_hmir::{
    HMIRDefID, HMIRUnitID,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::HMIRTypeID,
    unit::def::HMIRDeclarationPoint,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;
use cx_util::dense_id;

use crate::{
    env::function::{EvaluationID, LocalLoweringContext},
    value::HMIRValue,
};

dense_id!(MemoID, "memo");
dense_id!(MemoContextID, "context");

#[derive(Debug)]
pub(crate) struct MemoContext {
    owner: HMIRUnitID,
    visible_through: Option<HMIRDeclarationPoint>,
    bindings: Vec<(HMIRLocalID, MemoID)>,
    expected: Option<HMIRTypeID>,
    evaluation: Option<EvaluationID>,
}

impl MemoContext {
    pub fn new(
        owner: HMIRUnitID,
        visible_through: Option<HMIRDeclarationPoint>,
        bindings: Vec<(HMIRLocalID, MemoID)>,
        expected: Option<HMIRTypeID>,
        evaluation: Option<EvaluationID>,
    ) -> Self {
        Self {
            owner,
            visible_through,
            bindings,
            expected,
            evaluation,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum MemoKey {
    Definition(HMIRDefID, MemoContextID),
    Expression(HMIRExprID, MemoContextID),
}

#[derive(Debug)]
pub(crate) struct MemoEntry {
    key: MemoKey,
    state: MemoState,
}

#[derive(Debug)]
enum MemoState {
    Reserved,
    Evaluating,
    Ready(HMIRValue),
    Forward(MemoID),
}

impl LocalLoweringContext<'_, '_, '_> {
    pub fn evaluate(&mut self, id: MemoID, span: &TokenRange) -> CXResult<HMIRValue> {
        todo!("Follow forwarding links, check evaluation ownership, diagnose active cycles, evaluate the defining expression in its captured context, and cache only settled values; restore reservations on failure")
    }
}
