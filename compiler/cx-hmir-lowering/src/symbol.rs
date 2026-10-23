pub(crate) mod composite;
mod interface;
mod structure;

use cx_hmir::HMIRDefID;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::{
        HMIREnvironment,
        function::LocalLoweringContext,
        memo::{MemoContextID, MemoKey},
    },
    value::HMIRValue,
};

pub(crate) fn resolve(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    scope: MemoContextID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let id = context
        .env_mut()
        .resolver_mut()
        .reserve(MemoKey::Definition(def, scope));
    context.evaluate(id, span)
}

pub(crate) fn initialize(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    scope: MemoContextID,
) -> CXResult<HMIRValue> {
    todo!("Reserve the initializer expression in its owner context, then use context.evaluate to complete the global type and initializer together")
}

pub(crate) fn validate(env: &mut HMIREnvironment, def: HMIRDefID) -> CXResult<()> {
    todo!("Check owner-side declaration obligations independently of whether the symbol is used")
}
