use cx_hir::{
    ast::{modifiers::VisibilityMode, types::HIRTagKind},
    symbols::HIRSymbol,
};
use cx_log::{
    CXRawResult, CXResult,
    error::{CXError, context::CXInternalContext},
};
use cx_namespace::{
    lookup::{QualifiedLookup, QualifiedLookupResult},
    module::{NamespacePath, QualifiedName},
};
use cx_thir::{intrinsic_types::INTRINSIC_TYPES, symbol::MIRSymbol};
use cx_util::scoped_map::ScopedMap;

use crate::{environment::TypeEnvironment, symbol::resolution::resolve_symbol};

pub struct SymbolLookup {
    pub resolved_name: QualifiedName,
    pub kind: SymbolLookupKind,
}

pub enum SymbolLookupKind {
    Resolved(MIRSymbol),
    Untyped(Vec<HIRSymbol>),
}

struct Lookup<'a, 'b> {
    env: &'a TypeEnvironment<'b>,
    tag: Option<HIRTagKind>,
}

impl QualifiedLookup for Lookup<'_, '_> {
    type Output = Vec<HIRSymbol>;

    fn lookup_exact(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        let registry = self.env.symbols.get_global_registry();
        let visible = |symbols: &Vec<HIRSymbol>| {
            symbols.iter().any(|symbol| {
                symbol.visibility == VisibilityMode::Public
                    || name.namespace == *lexical_namespace
                    || registry.namespaces_are_friends(lexical_namespace, &name.namespace)
                    || (symbol.visibility == VisibilityMode::Package
                        && name
                            .namespace
                            .clone()
                            .strip_prefix(lexical_namespace)
                            .is_some())
            })
        };
        if self.tag.is_none() {
            if let Some(symbols) = registry.resolve(name, false).filter(visible) {
                return Some(symbols);
            }
        }
        registry
            .resolve(name, true)
            .or_else(|| {
                self.env
                    .symbols
                    .implicit_tags
                    .get(name)
                    .map(|symbol| vec![symbol.clone()])
            })
            .filter(visible)
    }

    fn resolve_aliases(
        &self,
        lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.env
            .symbols
            .get_global_registry()
            .resolve_aliases(lexical_namespace, namespace)
    }

    fn priority(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
        symbols: &Self::Output,
    ) -> (u8, bool) {
        (
            if name.namespace == *lexical_namespace {
                2
            } else {
                u8::from(
                    self.env
                        .symbols
                        .get_global_registry()
                        .namespaces_are_friends(lexical_namespace, &name.namespace),
                )
            },
            symbols.iter().any(|symbol| symbol.tag == self.tag),
        )
    }
}

impl TypeEnvironment<'_> {
    pub fn lookup_symbol(
        &self,
        namespace: &NamespacePath,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> CXRawResult<Option<SymbolLookup>> {
        if tag.is_none() && name.namespace.is_root() {
            if let Some(symbol) = self.symbols.local(name, &self.staged_expansions) {
                return Ok(Some(SymbolLookup {
                    resolved_name: name.clone(),
                    kind: SymbolLookupKind::Resolved(symbol.clone()),
                }));
            }
        }
        match (Lookup { env: self, tag }).qualified_lookup(namespace, name) {
            QualifiedLookupResult::Found {
                resolved_name,
                value,
            } => {
                if let Some(tag) = tag {
                    if value.iter().any(|symbol| symbol.tag != Some(tag)) {
                        return self.log_error_base(format!(
                            "Symbol '{resolved_name}' has incompatible tag declarations"
                        ));
                    }
                }
                Ok(Some(SymbolLookup {
                    resolved_name,
                    kind: SymbolLookupKind::Untyped(value),
                }))
            }
            QualifiedLookupResult::NotFound => Ok((tag.is_none()
                && name.namespace.is_root()
                && INTRINSIC_TYPES
                    .iter()
                    .any(|(intrinsic, _)| *intrinsic == name.name.as_str()))
            .then(|| self.symbols.cached(name, false))
            .flatten()
            .map(|symbol| SymbolLookup {
                resolved_name: name.clone(),
                kind: SymbolLookupKind::Resolved(symbol.clone()),
            })),
            QualifiedLookupResult::Ambiguous { candidates } => self.log_error_base(format!(
                "Ambiguous Symbol Reference, candidates: {}",
                candidates
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }

    pub fn get_symbol(
        &mut self,
        namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> CXResult<Option<MIRSymbol>> {
        let lookup = self.lookup_symbol(namespace, name, None).map_err(|err| {
            CXError::new(
                err,
                CXInternalContext::error(
                    "symbol lookup failed before a source range was available",
                ),
            )
        })?;
        lookup
            .map(|lookup| self.resolve_lookup(namespace, lookup))
            .transpose()
    }

    pub(crate) fn resolve_lookup(
        &mut self,
        namespace: &NamespacePath,
        lookup: SymbolLookup,
    ) -> CXResult<MIRSymbol> {
        let SymbolLookupKind::Untyped(declarations) = lookup.kind else {
            let SymbolLookupKind::Resolved(symbol) = lookup.kind else {
                unreachable!()
            };
            return Ok(symbol);
        };
        let name = lookup.resolved_name;
        if declarations.iter().all(|symbol| !symbol.is_type()) {
            if let Some(symbol) = self.symbols.cached(&name, false) {
                return Ok(symbol.clone());
            }
        }
        self.in_definition(|env| {
            let symbol =
                resolve_symbol(env, namespace, &name.namespace, &name.name, &declarations)?;
            env.symbols
                .insert_symbol(name, symbol.clone(), declarations[0].tag.is_some());
            Ok(symbol)
        })
    }

    pub(crate) fn in_definition<T, E>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        let locals = std::mem::replace(
            &mut self.symbols.local_symbols,
            ScopedMap::new_with_starting_scope(),
        );
        let result = f(self);
        self.symbols.local_symbols = locals;
        result
    }
}
