mod body;
mod builder;
mod def;
mod expr;
mod log;
mod plan;
mod symbols;
mod ty;
mod unit;

use cx_hir::ast::HIR;
use cx_hmir::{HMIRDefID, HMIRUnit, HMIRUnitID};
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;

use crate::{
    builder::HMIRUnitBuilder, def::lower_symbol, plan::plan_symbols, symbols::symbol_table,
    unit::UnitContext,
};

pub fn generate_hmir(
    hir: &HIR,
    id: HMIRUnitID,
    namespace: NamespacePath,
    root_exports: bool,
) -> CXResult<HMIRUnit> {
    let planned = plan_symbols(hir, &namespace, id);
    let context = UnitContext::new(id, root_exports, &planned);

    let symbols = symbol_table(hir, id, &planned);
    let mut unit = HMIRUnitBuilder::new(id, namespace);
    for (index, symbol) in planned.iter().enumerate() {
        let def = HMIRDefID::new(id, index);
        let def = lower_symbol(&context, def, symbol)?;
        unit.push_def(def);
    }
    for def in context.take_statics() {
        unit.push_def(def);
    }

    for (id, declarations) in context.take_externs() {
        unit.extend_def(id, declarations);
    }

    Ok(unit.finish(symbols))
}
