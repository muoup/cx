use cx_mir::{EnvironmentNamespace, symbol::MIRSymbol};
use cx_util::CXResult;

use crate::symbol::registry::MIRSymbolRegistry;

pub(crate) fn resolve_enum_block(
    env: &mut MIRSymbolRegistry,
    namespace: &EnvironmentNamespace,
    block_idx: usize,
    return_variant_symbol: usize
) -> CXResult<MIRSymbol> {
    todo!()
}