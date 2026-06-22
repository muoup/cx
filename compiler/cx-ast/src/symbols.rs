use std::collections::HashMap;

use cx_preparse_data::NamespaceAliases;
use cx_util::identifier::CXIdent;
use cx_util::namespace::NamespacePath;

use crate::ast::{
    expression::CXExpression,
    function::{CXComptimeFnPrototype, CXFunctionPrototype},
    global_var::CXEnumDefinition,
    modifiers::VisibilityMode,
    template::CXTemplatePrototype,
    types::CXType,
};

#[derive(Debug, Clone, PartialEq)]
pub struct CXSymbol {
    pub visibility: VisibilityMode,
    pub kind: CXSymbolKind,
}

impl CXSymbol {
    pub fn new(visibility: VisibilityMode, kind: CXSymbolKind) -> Self {
        Self { visibility, kind }
    }

    pub fn is_type(&self) -> bool {
        match &self.kind {
            CXSymbolKind::Type(_) => true,
            CXSymbolKind::DuplicateDefinition(definitions) => definitions.iter().any(|kind| {
                matches!(
                    kind,
                    CXSymbolKind::Type(_) | CXSymbolKind::TypeTemplate { .. }
                )
            }),
            _ => false,
        }
    }
}

pub type EnumBlockIdx = usize;

#[derive(Debug, Clone, PartialEq)]
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
    ComptimeFunction {
        definition: CXComptimeFnPrototype,
        body: Box<CXExpression>,
    },
    ComptimeFunctionTemplate {
        template: CXTemplatePrototype,
        definition: CXComptimeFnPrototype,
        body: Box<CXExpression>,
    },
    DuplicateDefinition(Vec<CXSymbolKind>),
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    enum_blocks: Vec<CXEnumDefinition>,
    symbols: HashMap<String, CXSymbol>,
    namespace_aliases: NamespaceAliases,
}

impl SymbolNamespaceData {
    pub fn new() -> Self {
        Self {
            enum_blocks: Vec::new(),
            symbols: HashMap::new(),
            namespace_aliases: HashMap::new(),
        }
    }

    pub fn new_with_namespace_aliases(namespace_aliases: NamespaceAliases) -> Self {
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
        for (alias, targets) in other.namespace_aliases {
            for target in targets {
                self.insert_namespace_alias(alias.clone(), target);
            }
        }
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

    pub fn insert_namespace_alias(&mut self, alias: NamespacePath, target: NamespacePath) {
        let targets = self.namespace_aliases.entry(alias).or_default();
        if !targets.contains(&target) {
            targets.push(target);
        }
    }

    pub fn resolve_aliases(
        &self,
        namespace: &NamespacePath,
    ) -> impl Iterator<Item = &NamespacePath> {
        self.namespace_aliases
            .get(namespace)
            .map(|t| t.as_slice().iter())
            .unwrap_or_else(|| [].iter())
    }

    pub fn get_enum_block(&self, idx: usize) -> Option<&CXEnumDefinition> {
        self.enum_blocks.get(idx)
    }
}
