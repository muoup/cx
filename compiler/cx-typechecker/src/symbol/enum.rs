use cx_ast::ast::global_var::CXEnumDefinition;
use cx_mir::{EnvironmentNamespace, symbol::MIRSymbol};
use cx_util::{CXResult, namespace::QualifiedName};

use crate::symbol::registry::MIRSymbolRegistry;

pub struct EnumBlockResolution<'a> {
    env: &'a MIRSymbolRegistry<'a>,
    block: &'a CXEnumDefinition,
}

impl<'a> EnumBlockResolution<'a> {
    pub fn variant_expr(&self, idx: usize) -> Option<&MIRSymbol> {
        self.block.variants.get(idx).and_then(|variant| {
            self.env
                .get_preresolved_symbol(&QualifiedName::root(variant.name.clone()))
        })
    }
}

pub(crate) fn resolve_enum_block<'a, 'b>(
    env: &'a mut MIRSymbolRegistry<'b>,
    namespace: &EnvironmentNamespace,
    block_idx: usize
) -> CXResult<EnumBlockResolution<'a>> {
    let (_, data) = env.get_global_registry()
        .get_bucket(namespace)
        .expect("Expected enum block to be in the global registry");

    let block = data
        .get_enum_block(block_idx)
        .expect("Expected enum block to be in the global registry");

    // TODO: Insert symbol data

    Ok(EnumBlockResolution { env, block })
}
