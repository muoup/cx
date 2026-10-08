use crate::parse::ParserData;
use cx_hir::ast::{
    function::{HIRFunctionKind, HIRFunctionPrototype},
    modifiers::{HIRSymbolNameScheme, LinkageMode},
};
use cx_namespace::module::QualifiedName;

pub(crate) fn resolve(
    data: &mut ParserData,
    prototype: &mut HIRFunctionPrototype,
    file_scope: bool,
) {
    if !data.c_mode {
        return;
    }
    let HIRFunctionKind::Standard(name) = &prototype.kind else {
        return;
    };
    let name = QualifiedName::new(
        data.pp_contents.module_symbols.namespace.clone(),
        name.clone(),
    );
    if prototype.linkage != LinkageMode::Static
        && data.c_function_linkages.get(&name) == Some(&LinkageMode::Static)
    {
        prototype.linkage = LinkageMode::Static;
    }
    prototype.symbol_naming = if prototype.linkage == LinkageMode::Static {
        HIRSymbolNameScheme::Namespaced
    } else {
        HIRSymbolNameScheme::Unmangled
    };
    if file_scope {
        data.c_function_linkages
            .entry(name)
            .or_insert(prototype.linkage);
    }
}
