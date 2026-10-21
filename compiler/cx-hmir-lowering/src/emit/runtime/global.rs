use cx_hmir::{HMIRDefID, HMIRDefKind, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::MIRGlobalRef;
use cx_tokens::TokenRange;

use crate::{env::function::LocalLoweringContext, eval::types};

pub fn reference(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let HMIRDefKind::Global(global) = context.env().symbols().resolve_definition(def).kind() else {
        unreachable!("a global place names a global variable")
    };
    let global = context
        .env()
        .module()
        .declared_global(global.symbol_name().as_str())
        .expect("global referenced before its declaration");
    let ty = types::lower_type(context.env_mut(), ty, span)?;

    Ok(MIRGlobalRef {
        global,
        offset: 0,
        ty,
    })
}
