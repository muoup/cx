use cx_ast::symbols::UntypedSymbol;
use cx_util::CXResult;

use crate::{
    mir::data::MIRTemplateInput,
    symbol::MIRSymbol,
    registry::MIRSymbolRegistry
};

pub fn resolve_symbol(
    _env: &mut MIRSymbolRegistry,
    _symbol: &UntypedSymbol,
) -> CXResult<MIRSymbol> {
    todo!()
}

pub fn apply_template(
    _env: &mut MIRSymbolRegistry,
    _symbol: &MIRSymbol,
    _input: MIRTemplateInput,
) -> CXResult<Option<MIRSymbol>> {
    todo!()
}
