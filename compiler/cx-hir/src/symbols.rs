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
    types::{HIRTagKind, HIRType},
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
        matches!(self.kind, HIRSymbolKind::Type(_))
    }
}

pub type EnumBlockIdx = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct HIRTypeSymbol {
    pub definition: HIRType,
    pub template: Option<HIRTemplatePrototype>,
    pub tag: Option<HIRTagKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRSymbolKind {
    Type(HIRTypeSymbol),
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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SymbolIdentifier {
    Standard(String),
    Tag(String),
}

impl SymbolIdentifier {
    pub fn standard(name: impl Into<String>) -> Self {
        Self::Standard(name.into())
    }

    pub fn tag(name: impl Into<String>) -> Self {
        Self::Tag(name.into())
    }

    pub fn name(&self) -> &str {
        match self {
            Self::Standard(name) | Self::Tag(name) => name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolResolution {
    declarations: Vec<HIRSymbol>,
}

impl SymbolResolution {
    pub fn new(symbol: HIRSymbol) -> Self {
        Self {
            declarations: vec![symbol],
        }
    }

    pub fn declarations(&self) -> &[HIRSymbol] {
        &self.declarations
    }

    pub fn filter(
        &self,
        mut predicate: impl FnMut(&HIRSymbol) -> bool,
    ) -> Option<SymbolResolution> {
        self.declarations
            .iter()
            .any(&mut predicate)
            .then(|| self.clone())
    }

    pub fn push(&mut self, symbol: HIRSymbol) {
        self.declarations.push(symbol);
    }

    pub fn replace(&mut self, index: usize, symbol: HIRSymbol) {
        self.declarations[index] = symbol;
    }
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    enum_blocks: Vec<HIREnumDefinition>,
    symbols: HashMap<SymbolIdentifier, SymbolResolution>,
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

    pub fn insert_symbol(&mut self, identifier: SymbolIdentifier, symbol: HIRSymbol) {
        self.symbols
            .entry(identifier)
            .and_modify(|resolution| resolution.push(symbol.clone()))
            .or_insert_with(|| SymbolResolution::new(symbol));
    }

    pub fn replace_symbol(
        &mut self,
        identifier: SymbolIdentifier,
        index: usize,
        symbol: HIRSymbol,
    ) {
        self.symbols
            .get_mut(&identifier)
            .expect("symbol resolution must exist before replacement")
            .replace(index, symbol);
    }

    pub fn merge_from(&mut self, other: SymbolNamespaceData) {
        let enum_offset = self.enum_blocks.len();
        self.enum_blocks.extend(other.enum_blocks);
        for (alias, targets) in other.namespace_aliases {
            for target in targets {
                self.insert_namespace_alias(alias.clone(), target);
            }
        }
        self.symbols.extend(
            other
                .symbols
                .into_iter()
                .map(|(identifier, mut resolution)| {
                    for symbol in &mut resolution.declarations {
                        if let HIRSymbolKind::EnumIdent { enum_block_idx, .. } = &mut symbol.kind {
                            *enum_block_idx += enum_offset;
                        }
                    }

                    (identifier, resolution)
                }),
        );
    }

    pub fn insert_enum_block(&mut self, block: HIREnumDefinition) -> EnumBlockIdx {
        self.enum_blocks.push(block);
        self.enum_blocks.len() - 1
    }

    pub fn get_symbol(&self, identifier: &SymbolIdentifier) -> Option<&SymbolResolution> {
        self.symbols.get(identifier)
    }

    pub fn symbol_names(&self) -> impl Iterator<Item = &str> {
        self.symbols.keys().map(SymbolIdentifier::name)
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
