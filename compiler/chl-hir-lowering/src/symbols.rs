use chl_hir::ast::HIR;
use chl_hmir::{HMIRDefID, HMIRUnitID, unit::symbols::HMIRSymbolTable};
use chl_namespace::module::NamespacePath;

use crate::plan::PlannedSymbol;

pub(crate) fn symbol_table(
    hir: &HIR,
    id: HMIRUnitID,
    planned: &[PlannedSymbol],
) -> HMIRSymbolTable {
    let mut imports = Vec::<NamespacePath>::new();
    for path in hir.imports.iter().flat_map(|import| &import.paths) {
        if !imports.contains(&path.resolved) {
            imports.push(path.resolved.clone());
        }
    }

    let symbols = planned
        .iter()
        .enumerate()
        .map(|(index, symbol)| {
            let key = (symbol.name().clone(), symbol.class());
            (key, HMIRDefID::new(id, index))
        })
        .collect();

    HMIRSymbolTable::new(imports, hir.namespace_aliases.clone(), symbols)
}
