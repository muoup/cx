use std::collections::HashMap;

use cx_preparse_data::NamespaceAliases;
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

    pub fn is_type(&self) -> bool {
        matches!(self.kind, CXSymbolKind::Type(_))
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

    pub fn resolve_qualified_aliases(&self, name: &QualifiedName) -> Vec<QualifiedName> {
        let mut aliases = self
            .namespace_aliases
            .iter()
            .filter_map(|(alias, targets)| {
                if alias.is_root() {
                    Some((alias, targets))
                } else {
                    name.namespace.strip(alias).map(|_| (alias, targets))
                }
            })
            .collect::<Vec<_>>();

        aliases.sort_by(|(left, _), (right, _)| {
            right
                .segments()
                .len()
                .cmp(&left.segments().len())
                .then_with(|| left.as_scope_string().cmp(&right.as_scope_string()))
        });

        let mut resolved = Vec::new();
        for (alias, targets) in aliases {
            let suffix = if alias.is_root() {
                name.namespace.clone()
            } else {
                name.namespace
                    .strip(alias)
                    .expect("Alias prefix was checked above")
            };

            for target in targets {
                push_unique(
                    &mut resolved,
                    QualifiedName {
                        namespace: target.join(&suffix),
                        name: name.name.clone(),
                    },
                );
            }
        }

        resolved
    }

    pub fn get_enum_block(&self, idx: usize) -> Option<&CXEnumDefinition> {
        self.enum_blocks.get(idx)
    }
}

fn push_unique(names: &mut Vec<QualifiedName>, name: QualifiedName) {
    if !names.contains(&name) {
        names.push(name);
    }
}
