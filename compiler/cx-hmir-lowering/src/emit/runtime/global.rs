use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_log::CXResult;
use cx_mir::MIRGlobalRef;
use cx_tokens::TokenRange;

use crate::{env::HMIREnvironment, eval::types, symbol};

pub fn address(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    offset: i64,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let object = symbol::value_type(env, def, None)?;
    let global = crate::lower::global::declare(env, def, object, span)?;
    Ok(MIRGlobalRef {
        global,
        offset,
        ty: types::lower_type(env, ty, span)?,
    })
}
