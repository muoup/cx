use cx_hir::ast::HIR;
use cx_hmir::{HMIRDefID, HMIRUnitID, unit::symbols::HMIRSymbolTable};
use cx_namespace::module::NamespacePath;

use crate::plan::PlannedSymbol;

pub(crate) fn symbol_table(
    hir: &HIR,
    id: HMIRUnitID,
    planned: &[PlannedSymbol],
) -> HMIRSymbolTable {
    let mut imports = Vec::<NamespacePath>::new();
    for name in hir.imports.iter().flat_map(|import| &import.names) {
        let module = name.namespace.clone().child(name.name.clone());
        if !imports.contains(&module) {
            imports.push(module);
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
