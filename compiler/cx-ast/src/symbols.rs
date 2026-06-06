use std::collections::HashMap;

use cx_util::identifier::CXIdent;
use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, global_var::CXEnumDefinition,
    modifiers::VisibilityMode, template::CXTemplatePrototype, types::CXType,
};

#[derive(Debug, Clone)]
pub struct CXSymbol {
    pub visibility: VisibilityMode,
    pub kind: CXSymbolKind,
}

impl CXSymbol {
    pub fn new(visibility: VisibilityMode, kind: CXSymbolKind) -> Self {
        Self { visibility, kind }
    }
}

pub type EnumBlockIdx = usize;

#[derive(Debug, Clone)]
pub enum CXSymbolKind {
    Type(CXType),
    FunctionReference(CXFunctionPrototype),
    TypeConstructor {
        template: Option<CXTemplatePrototype>,
        union_type: CXType,
        variant_index: usize,
    },
    AddressableGlobal(CXIdent, CXType),
    EnumIdent {
        enum_block_idx: EnumBlockIdx,
        variant_index: usize,
    },
    TypeTemplate {
        template: CXTemplatePrototype,
        definition: CXType,
    },
    FunctionTemplate {
        template: CXTemplatePrototype,
        definition: CXFunctionPrototype,
        body: Box<CXExpression>,
    },
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    enum_blocks: Vec<CXEnumDefinition>,
    symbols: HashMap<String, CXSymbol>,
    namespace_aliases: HashMap<NamespacePath, NamespacePath>,
}

impl SymbolNamespaceData {
    pub fn new() -> Self {
        Self {
            enum_blocks: Vec::new(),
            symbols: HashMap::new(),
            namespace_aliases: HashMap::new(),
        }
    }

    pub fn new_with_namespace_aliases(
        namespace_aliases: HashMap<NamespacePath, NamespacePath>,
    ) -> Self {
        Self {
            enum_blocks: Vec::new(),
            symbols: HashMap::new(),
            namespace_aliases,
        }
    }

    pub fn insert_symbol(&mut self, name: impl Into<String>, symbol: CXSymbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn merge_from(&mut self, other: SymbolNamespaceData) {
        let enum_offset = self.enum_blocks.len();
        self.enum_blocks.extend(other.enum_blocks);
        self.namespace_aliases.extend(other.namespace_aliases);
        self.symbols
            .extend(other.symbols.into_iter().map(|(name, mut symbol)| {
                if let CXSymbolKind::EnumIdent { enum_block_idx, .. } = &mut symbol.kind {
                    *enum_block_idx += enum_offset;
                }

                (name, symbol)
            }));
    }

    pub fn insert_enum_block(&mut self, block: CXEnumDefinition) -> EnumBlockIdx {
        self.enum_blocks.push(block);
        self.enum_blocks.len() - 1
    }

    pub fn get_symbol(&self, name: &str) -> Option<&CXSymbol> {
        self.symbols.get(name)
    }

    pub fn resolve_qualified_alias(&self, name: &QualifiedName) -> Option<QualifiedName> {
        self.namespace_aliases
            .get(&name.namespace)
            .map(|alias| QualifiedName {
                namespace: alias.clone(),
                name: name.name.clone(),
            })
    }

    pub fn get_enum_block(&self, idx: usize) -> Option<&CXEnumDefinition> {
        self.enum_blocks.get(idx)
    }
}
