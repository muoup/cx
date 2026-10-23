use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::MIRGlobalRef;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::types,
    symbol::{self, CompletedSymbol},
};

pub fn reference(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let CompletedSymbol::Global(global) = symbol::completed(context.env(), def) else {
        unreachable!("a global place names a global variable")
    };
    let global = context
        .env()
        .module()
        .declared_global(global.symbol_name.as_str())
        .expect("global referenced before its declaration");
    let ty = types::lower_type(context.env_mut(), ty, span)?;

    Ok(MIRGlobalRef {
        global,
        offset: 0,
        ty,
    })
}
