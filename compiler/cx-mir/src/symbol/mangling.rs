use cx_ast::registry::{ExportNameMode, GlobalSymbolRegistry};
use cx_util::namespace::QualifiedName;

pub use crate::mir::name_mangling::{
    base_mangle_member, base_mangle_static_member, mangle_namespace_symbol,
};

use crate::registry::MIRSymbolRegistry;

pub fn base_mangle_standard(symbols: &MIRSymbolRegistry, name: &QualifiedName) -> String {
    mangle_qualified_symbol(symbols.global_registry, name)
}

pub fn mangle_qualified_symbol(
    global_registry: &GlobalSymbolRegistry,
    name: &QualifiedName,
) -> String {
    if global_registry.export_name_mode(&name.namespace) == ExportNameMode::Root {
        return name.name.as_string();
    }

    let flat_name = name.as_flat_name();
    if !flat_name.contains("::") {
        return flat_name;
    }

    format!("_N{}", flat_name.replace("::", "_"))
}
