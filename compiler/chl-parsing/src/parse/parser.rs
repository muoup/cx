use rustc_hash::FxHashMap;

use chl_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    function::{HIRFunctionKind, HIRParameter},
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRTypeKind,
    HIRDefinition, HIRStmt, HIR,
};
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_namespace::lookup::candidates;
use chl_namespace::module::{ModulePath, NamespacePath, QualifiedName};
use chl_preparse_data::registry::GlobalPreparseRegistry;
use chl_preparse_data::symbol_data::PreparseSymbolKind;
use chl_preparse_data::{NamespaceAliases, PreparseContents, VisibilityMode};
use chl_tokens::{TokenIter, TokenRange};
use chl_util::identifier::CHLIdent;

use crate::log::parse_point_error;

#[derive(Debug)]
pub struct ParserData<'a> {
    pub registry: &'a GlobalPreparseRegistry,
    pub pp_contents: &'a PreparseContents,
    pub tokens: TokenIter<'a>,

    include_states: Vec<IncludeParserState>,
    
    pub visibility: VisibilityMode,
    pub symbol_naming: HIRSymbolNameScheme,
    pub expr_commas: Vec<bool>,
    name_scopes: Vec<FxHashMap<CHLIdent, LocalName>>,
    namespace_aliases: NamespaceAliases,
    pub(super) c_function_linkages: FxHashMap<QualifiedName, LinkageMode>,

    pub ast: HIR,
    pub c_mode: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum LocalName {
    Type,
    Value,
    Inferred,
}

#[derive(Debug, Clone, Copy)]
struct IncludeParserState {
    visibility: VisibilityMode,
    symbol_naming: HIRSymbolNameScheme,
}

impl<'a> ParserData<'a> {
    pub fn new(
        tokens: TokenIter<'a>,
        pp_contents: &'a PreparseContents,
        registry: &'a GlobalPreparseRegistry,
    ) -> Self {
        let extension = tokens.file.extension().and_then(|extension| extension.to_str());
        let c_mode = extension == Some("c");

        Self {
            tokens,
            visibility: VisibilityMode::Package,
            symbol_naming: HIRSymbolNameScheme::Namespaced,
            include_states: Vec::new(),
            expr_commas: vec![true],
            pp_contents,
            registry,
            name_scopes: vec![FxHashMap::default()],
            namespace_aliases: pp_contents.namespace_aliases.clone(),
            c_function_linkages: FxHashMap::default(),
            ast: HIR::new(
                ModulePath::from_source_path(pp_contents.module.as_str()),
                pp_contents.imports.clone(),
            ),
            c_mode,
        }
    }

    pub fn back(&mut self) -> &mut Self {
        self.tokens.back();
        self
    }

    pub fn change_comma_mode(&mut self, expr_comma: bool) {
        self.expr_commas.push(expr_comma);
    }

    pub fn pop_comma_mode(&mut self) {
        if self.expr_commas.is_empty() {
            panic!("CRITICAL: No comma mode to pop!");
        }

        self.expr_commas.pop();
    }

    pub fn token_range(&self, start_token: usize, end_token: usize) -> TokenRange {
        TokenRange::from_tokens(start_token, end_token, self.tokens.slice)
    }

    pub fn expr_from(&self, start_token: usize, kind: HIRExprKind) -> HIRExpression {
        kind.into_expr(
            start_token,
            self.tokens.index,
            self.token_range(start_token, self.tokens.index),
        )
    }

    pub fn get_comma_mode(&self) -> bool {
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }

    pub fn add_stmt(&mut self, stmt: HIRStmt) {
        self.add_declaration(stmt, false);
    }

    pub(super) fn add_declaration(&mut self, stmt: HIRStmt, overwrite: bool) {
        let namespace = self.current_module_namespace();
        self.register_stmt_namespace_aliases(&namespace, &stmt);
        self.ast.definition_stmts.push(HIRDefinition {
            namespace,
            stmt,
            ordered: self.c_mode || self.in_include(),
            overwrite,
        })
    }

    pub fn begin_include(&mut self) {
        self.include_states.push(IncludeParserState {
            visibility: self.visibility,
            symbol_naming: self.symbol_naming,
        });
        self.symbol_naming = HIRSymbolNameScheme::Unmangled;
    }

    pub fn end_include(&mut self) -> CHLResult<()> {
        let Some(state) = self.include_states.pop() else {
            return parse_point_error(&self.tokens, &UNEXPECTED_END, Some("included source".into()));
        };
        self.visibility = state.visibility;
        self.symbol_naming = state.symbol_naming;
        Ok(())
    }

    pub fn in_include(&self) -> bool {
        !self.include_states.is_empty()
    }

    pub fn naming(&self, linkage: LinkageMode) -> HIRSymbolNameScheme {
        if self.c_mode {
            if linkage == LinkageMode::Static {
                HIRSymbolNameScheme::Namespaced
            } else {
                HIRSymbolNameScheme::Unmangled
            }
        } else {
            self.symbol_naming
        }
    }

    pub fn take_ast(mut self) -> HIR {
        self.ast.namespace_aliases = self.namespace_aliases;
        self.ast
    }

    pub fn is_generator(&self, name: &QualifiedName) -> CHLResult<bool> {
        Ok(self.local_name(name).is_none()
            && self.symbol_kind(name)? == Some(PreparseSymbolKind::Generator))
    }

    pub(super) fn with_name_scope<T>(&mut self, parse: impl FnOnce(&mut Self) -> T) -> T {
        self.name_scopes.push(FxHashMap::default());
        let result = parse(self);
        self.name_scopes.pop();
        result
    }

    pub(super) fn note_parameter(&mut self, parameter: &HIRParameter) {
        if let Some(name) = &parameter.name {
            let kind = if matches!(parameter.ty.kind, HIRTypeKind::Universe) {
                LocalName::Type
            } else {
                LocalName::Value
            };
            self.note_local_name(name.clone(), kind);
        }
    }

    pub(super) fn note_local_name(&mut self, name: CHLIdent, kind: LocalName) {
        self.name_scopes.last_mut().expect("a lexical name scope").insert(name, kind);
    }
    
    pub(super) fn local_name(&self, name: &QualifiedName) -> Option<LocalName> {
        let root = name.root_name_ref()?;
        self.name_scopes.iter().rev().find_map(|scope| scope.get(root).copied())
    }

    pub(super) fn is_module_type(&self, name: &QualifiedName) -> CHLResult<bool> {
        Ok(self
            .symbol_kind(name)?
            .is_some_and(|kind| !(self.c_mode && kind == PreparseSymbolKind::Tag)))
    }

    pub fn symbol_kind(&self, name: &QualifiedName) -> CHLResult<Option<PreparseSymbolKind>> {
        let lexical_namespace = self.current_module_namespace();
        let mut priority = (0, false);
        let mut found = Vec::new();
        for candidate in candidates(&lexical_namespace, name, &self.namespace_aliases) {
            let Some(kind) = self.registry.get_symbol(&candidate.namespace, &candidate.name) else {
                continue;
            };
            let rank = (
                if candidate.namespace == lexical_namespace { 2 } else { 0 },
                kind == PreparseSymbolKind::Type,
            );
            if rank > priority {
                found.clear();
                priority = rank;
            }
            if rank == priority {
                found.push((candidate, kind));
            }
        }

        match found.as_slice() {
            [] => Ok(None),
            [(_, kind), rest @ ..] if rest.iter().all(|(_, other)| other == kind) => Ok(Some(*kind)),
            _ => {
                let mut candidates = found.iter().map(|(name, _)| name.to_string()).collect::<Vec<_>>();
                candidates.sort();
                parse_point_error(
                    &self.tokens,
                    &AMBIGUOUS_LOOKUP,
                    (name.to_string(), candidates.join(", ")),
                )
            }
        }
    }

    fn current_module_namespace(&self) -> NamespacePath {
        self.pp_contents.module_symbols.namespace.clone()
    }

    fn register_stmt_namespace_aliases(&mut self, namespace: &NamespacePath, stmt: &HIRStmt) {
        if namespace.is_root() {
            return;
        }

        if let HIRStmt::FunctionDefinition { prototype, .. } = stmt {
            let q_namespace = prototype.kind.into_key().namespace;

            if !q_namespace.is_root()
                && matches!(&prototype.kind, HIRFunctionKind::AssociatedFunction { .. })
            {
                let entry = self.namespace_aliases.entry(namespace.clone()).or_default();

                if !entry.contains(&q_namespace) {
                    entry.push(q_namespace);
                }
            }
        }
    }
}
