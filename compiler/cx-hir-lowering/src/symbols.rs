use cx_hir::{ast::HIR, registry::GlobalSymbolRegistry};
use cx_hmir::{HMIRDefID, unit::symbols::HMIRSymbolTable};
use cx_namespace::module::NamespacePath;

use crate::plan::PlannedDef;

pub(crate) fn symbol_table(
    hir: &HIR,
    namespace: &NamespacePath,
    registry: &GlobalSymbolRegistry,
    plans: &[PlannedDef],
) -> HMIRSymbolTable {
    let mut imports = Vec::<NamespacePath>::new();
    for name in hir.imports.iter().flat_map(|import| &import.names) {
        let module = name.namespace.clone().child(name.name.clone());
        if !imports.contains(&module) {
            imports.push(module);
        }
    }

    let declarations = plans
        .iter()
        .enumerate()
        .flat_map(|(index, plan)| {
            plan.declarations()
                .iter()
                .map(move |declaration| (HMIRDefID::new(index), declaration.clone()))
        })
        .collect();

    HMIRSymbolTable::new(
        imports,
        hir.namespace_aliases.clone(),
        registry.namespace_friends(namespace),
        registry.export_name_mode(namespace),
        declarations,
    )
}
