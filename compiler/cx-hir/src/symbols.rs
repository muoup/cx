use std::collections::{hash_map::Entry, HashMap};

use cx_log::CXRawResult;
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
pub enum HIRSymbolData<
    Base: std::fmt::Debug + Clone + PartialEq,
    TemplateData: std::fmt::Debug + Clone + PartialEq,
> {
    Standard(Base),
    Template {
        base: Base,
        template: TemplateData,
        template_prototype: HIRTemplatePrototype,
    },
}

impl<
        Base: std::fmt::Debug + Clone + PartialEq,
        TemplateData: std::fmt::Debug + Clone + PartialEq,
    > HIRSymbolData<Base, TemplateData>
{
    pub fn new_default(base: Base, template_proto: Option<HIRTemplatePrototype>) -> Self
    where
        TemplateData: Default,
    {
        match template_proto {
            Some(proto) => Self::Template {
                base,
                template: TemplateData::default(),
                template_prototype: proto,
            },
            None => Self::Standard(base),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructorData {
    pub union_type: HIRType,
    pub variant_index: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HIRSymbolKind {
    Type(HIRSymbolData<HIRType, ()>),
    Function(HIRSymbolData<HIRFunctionPrototype, Box<HIRExpression>>),
    TypeConstructor(HIRSymbolData<TypeConstructorData, ()>),
    ComptimeFunction(HIRSymbolData<HIRComptimeFnPrototype, Box<HIRExpression>>),
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

        Ok(())
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
