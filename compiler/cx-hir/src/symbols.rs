use std::collections::{hash_map::Entry, HashMap};

use cx_namespace::module::NamespacePath;
use cx_preparse_data::NamespaceAliases;
use cx_util::identifier::CXIdent;

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
pub enum HIRSymbolData<
    Base: std::fmt::Debug + Clone + PartialEq,
    Data: std::fmt::Debug + Clone + PartialEq,
> {
    Standard {
        base: Base
    },
    Template {
        base: Base,
        template_data: Data,
        template_prototype: HIRTemplatePrototype,
    },
}

impl<Base: std::fmt::Debug + Clone + PartialEq, Data: std::fmt::Debug + Clone + PartialEq>
    HIRSymbolData<Base, Data>
{
    pub fn new(base: Base, data: Data, template_proto: Option<HIRTemplatePrototype>) -> Self {
        match template_proto {
            Some(proto) => Self::Template {
                base,
                template_data: data,
                template_prototype: proto,
            },
            None => Self::Standard { base },
        }
    }

    pub fn base(&self) -> &Base {
        match self {
            Self::Standard { base, .. } => base,
            Self::Template { base, .. } => base,
        }
    }
}

pub type HIRTypeSymbol = HIRSymbolData<HIRType, ()>;
pub type HIRTypeConstructorSymbol = HIRSymbolData<TypeConstructorData, ()>;
pub type HIRFunctionSymbol = HIRSymbolData<HIRFunctionPrototype, Box<HIRExpression>>;
pub type HIRComptimeFunctionSymbol = HIRSymbolData<HIRComptimeFnPrototype, Box<HIRExpression>>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructorData {
    pub union_type: HIRType,
    pub variant_index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRSymbolKind {
    Type(HIRTypeSymbol),
    Function(HIRFunctionSymbol),
    TypeConstructor(HIRTypeConstructorSymbol),
    ComptimeFunction(HIRComptimeFunctionSymbol),
    AddressableGlobal {
        name: CXIdent,
        _type: HIRType,
        symbol_naming: HIRSymbolNameScheme,
    },
    EnumIdent {
        enum_block_idx: EnumBlockIdx,
        variant_index: usize,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SymbolIdentifier {
    Standard(String),
    Tag { kind: HIRTagKind, name: String },
}

#[derive(Debug, Clone)]
pub struct SymbolResolution {
    declarations: Vec<HIRSymbol>,
    tag_kinds: Option<Vec<HIRTagKind>>,
}

impl SymbolResolution {
    pub fn new(symbol: HIRSymbol) -> Self {
        Self {
            declarations: vec![symbol],
            tag_kinds: None,
        }
    }

    pub fn new_tagged(tag: HIRTagKind, symbol: HIRSymbol) -> Self {
        Self {
            declarations: vec![symbol],
            tag_kinds: Some(vec![tag]),
        }
    }

    pub fn standard(declarations: Vec<HIRSymbol>) -> Self {
        Self {
            declarations,
            tag_kinds: None,
        }
    }

    pub fn tagged(declarations: Vec<(HIRTagKind, HIRSymbol)>) -> Self {
        let (tag_kinds, declarations) = declarations.into_iter().unzip();
        Self {
            declarations,
            tag_kinds: Some(tag_kinds),
        }
    }

    pub fn declarations(&self) -> &[HIRSymbol] {
        &self.declarations
    }

    pub fn into_declarations(self) -> Vec<HIRSymbol> {
        self.declarations
    }

    pub fn tag_kinds(&self) -> Option<&[HIRTagKind]> {
        self.tag_kinds.as_deref()
    }

    pub fn filter(&self, mut predicate: impl FnMut(&HIRSymbol) -> bool) -> Option<Self> {
        self.declarations
            .iter()
            .any(&mut predicate)
            .then(|| self.clone())
    }
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    enum_blocks: Vec<HIREnumDefinition>,
    symbols: HashMap<String, Vec<HIRSymbol>>,
    tagged_symbols: HashMap<String, Vec<(HIRTagKind, HIRSymbol)>>,
    namespace_aliases: NamespaceAliases,
}

impl SymbolNamespaceData {
    pub fn new() -> Self {
        Self {
            enum_blocks: Vec::new(),
            symbols: HashMap::new(),
            tagged_symbols: HashMap::new(),
            namespace_aliases: HashMap::new(),
        }
    }

    pub fn new_with_namespace_aliases(namespace_aliases: NamespaceAliases) -> Self {
        Self {
            enum_blocks: Vec::new(),
            symbols: HashMap::new(),
            tagged_symbols: HashMap::new(),
            namespace_aliases,
        }
    }

    pub fn insert_symbol(&mut self, identifier: SymbolIdentifier, symbol: HIRSymbol) {
        match identifier {
            SymbolIdentifier::Standard(name) => match self.symbols.entry(name) {
                Entry::Occupied(ref mut entry) => entry.get_mut().push(symbol),
                Entry::Vacant(entry) => {
                    entry.insert(vec![symbol]);
                }
            },
            SymbolIdentifier::Tag { kind, name } => match self.tagged_symbols.entry(name) {
                Entry::Occupied(ref mut entry) => entry.get_mut().push((kind, symbol)),
                Entry::Vacant(entry) => {
                    entry.insert(vec![(kind, symbol)]);
                }
            },
        };
    }

    pub fn insert_enum_block(&mut self, block: HIREnumDefinition) -> EnumBlockIdx {
        self.enum_blocks.push(block);
        self.enum_blocks.len() - 1
    }

    pub fn get_standard_symbol(&self, name: &str) -> Option<SymbolResolution> {
        self.symbols
            .get(name)
            .cloned()
            .map(SymbolResolution::standard)
    }

    pub fn get_tag_symbol(&self, name: &str) -> Option<SymbolResolution> {
        self.tagged_symbols
            .get(name)
            .cloned()
            .map(SymbolResolution::tagged)
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
