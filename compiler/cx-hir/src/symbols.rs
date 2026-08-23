use std::collections::HashMap;

use cx_preparse_data::NamespaceAliases;
use cx_util::identifier::CXIdent;
use cx_util::namespace::NamespacePath;

use crate::ast::{
    expression::HIRExpression,
    function::{HIRComptimeFnPrototype, HIRFunctionPrototype},
    global_var::HIREnumDefinition,
    modifiers::{HIRSymbolNameScheme, VisibilityMode},
    template::HIRTemplatePrototype,
    types::{HIRType, PredeclarationType},
};

#[derive(Debug, Clone, PartialEq)]
pub struct HIRSymbol {
    pub visibility: VisibilityMode,
    pub kind: HIRSymbolKind,
}

impl HIRSymbol {
    pub fn new(visibility: VisibilityMode, kind: HIRSymbolKind) -> Self {
        Self { visibility, kind }
    }

    pub fn is_type(&self) -> bool {
        match &self.kind {
            HIRSymbolKind::Type(_) | HIRSymbolKind::TagType { .. } => true,
            HIRSymbolKind::DuplicateDefinition(definitions) => definitions.iter().any(|kind| {
                matches!(
                    kind,
                    HIRSymbolKind::Type(_)
                        | HIRSymbolKind::TagType { .. }
                        | HIRSymbolKind::TypeTemplate { .. }
                        | HIRSymbolKind::TagTypeTemplate { .. }
                )
            }),
            _ => false,
        }
    }
}

pub type EnumBlockIdx = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum HIRSymbolKind {
    Type(HIRType),
    TagType {
        definition: HIRType,
        tag: PredeclarationType,
    },
    FunctionReference(HIRFunctionPrototype),
    TypeConstructor {
        template: Option<HIRTemplatePrototype>,
        union_type: HIRType,
        variant_index: usize,
    },
    AddressableGlobal {
        name: CXIdent,
        _type: HIRType,
        symbol_naming: HIRSymbolNameScheme,
    },
    EnumIdent {
        enum_block_idx: EnumBlockIdx,
        variant_index: usize,
    },
    TypeTemplate {
        template: HIRTemplatePrototype,
        definition: HIRType,
    },
    TagTypeTemplate {
        template: HIRTemplatePrototype,
        definition: HIRType,
        tag: PredeclarationType,
    },
    FunctionTemplate {
        template: HIRTemplatePrototype,
        definition: HIRFunctionPrototype,
        body: Box<HIRExpression>,
    },
    ComptimeFunction {
        definition: HIRComptimeFnPrototype,
        body: Box<HIRExpression>,
    },
    ComptimeFunctionTemplate {
        template: HIRTemplatePrototype,
        definition: HIRComptimeFnPrototype,
        body: Box<HIRExpression>,
    },
    DuplicateDefinition(Vec<HIRSymbolKind>),
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    enum_blocks: Vec<HIREnumDefinition>,
    symbols: HashMap<String, HIRSymbol>,
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

    pub fn insert_symbol(&mut self, name: impl Into<String>, symbol: HIRSymbol) {
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
                if let HIRSymbolKind::EnumIdent { enum_block_idx, .. } = &mut symbol.kind {
                    *enum_block_idx += enum_offset;
                }

                (name, symbol)
            }));
    }

    pub fn insert_enum_block(&mut self, block: HIREnumDefinition) -> EnumBlockIdx {
        self.enum_blocks.push(block);
        self.enum_blocks.len() - 1
    }

    pub fn get_symbol(&self, name: &str) -> Option<&HIRSymbol> {
        self.symbols.get(name)
    }

    pub fn symbol_names(&self) -> impl Iterator<Item = &str> {
        self.symbols.keys().map(String::as_str)
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

    pub fn get_enum_block(&self, idx: usize) -> Option<&HIREnumDefinition> {
        self.enum_blocks.get(idx)
    }
}
