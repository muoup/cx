use std::collections::HashMap;

use cx_hir::ast::{
    function::HIRFunctionKind, modifiers::HIRSymbolNameScheme, HIRDefinition, HIRStmt, HIR,
};
use cx_log::CXResult;
use cx_namespace::result::QualifiedLookupResult;
use cx_namespace::QualifiedLookup;
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_preparse_data::symbol_data::PreparseSymbolKind;
use cx_preparse_data::{NamespaceAliases, PreparseContents, VisibilityMode};
use cx_tokens::{TokenIter, TokenRange};
use cx_util::identifier::CXIdent;
use cx_util::module_path::ModulePath;
use cx_util::module::{NamespacePath, QualifiedName};

use crate::log::parse_point_error;

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub symbol_naming: HIRSymbolNameScheme,
    include_states: Vec<IncludeParserState>,
    pub expr_commas: Vec<bool>,
    pub pp_contents: &'a PreparseContents,
    // uses u8 mapping instead of a set to prevent problems with shadowing
    pub temporary_type_names: HashMap<CXIdent, u8>,
    namespace_aliases: NamespaceAliases,

    pub registry: &'a GlobalPreparseRegistry,
    pub ast: HIR,
    pub c_mode: bool,
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
        let c_mode = tokens
            .file
            .extension()
            .is_some_and(|extension| extension == "c");

        Self {
            tokens,
            visibility: VisibilityMode::Package,
            symbol_naming: HIRSymbolNameScheme::Namespaced,
            include_states: Vec::new(),
            expr_commas: vec![true],
            pp_contents,
            registry,
            temporary_type_names: HashMap::new(),
            namespace_aliases: pp_contents.namespace_aliases.clone(),
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

    pub fn token_range(
        &self,
        start_token: usize,
        end_token: usize,
    ) -> TokenRange {
        TokenRange::from_tokens(start_token, end_token, self.tokens.slice)
    }

    pub fn get_comma_mode(&self) -> bool {
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }

    pub fn add_stmt(&mut self, stmt: HIRStmt) {
        let namespace = self.current_module_namespace();
        self.register_stmt_namespace_aliases(&namespace, &stmt);
        self.ast
            .definition_stmts
            .push(HIRDefinition { namespace, stmt })
    }

    pub fn begin_include(&mut self) {
        self.include_states.push(IncludeParserState {
            visibility: self.visibility,
            symbol_naming: self.symbol_naming,
        });
        self.symbol_naming = HIRSymbolNameScheme::Unmangled;
    }

    pub fn end_include(&mut self) -> CXResult<()> {
        let Some(state) = self.include_states.pop() else {
            return parse_point_error(&self.tokens, "Unexpected end of included source");
        };
        self.visibility = state.visibility;
        self.symbol_naming = state.symbol_naming;
        Ok(())
    }

    pub fn in_include(&self) -> bool {
        !self.include_states.is_empty()
    }

    pub fn take_ast(mut self) -> HIR {
        self.ast.namespace_aliases = self.namespace_aliases;
        self.ast
    }

    pub fn is_type_ident(&self, name: &QualifiedName) -> CXResult<bool> {
        Ok(self.query_identifier(name.clone())?
            || (name.namespace.is_root() && self.temporary_type_names.contains_key(&name.name)))
    }

    pub fn query_identifier(&self, name: QualifiedName) -> CXResult<bool> {
        match self.qualified_lookup(&self.current_module_namespace(), &name) {
            QualifiedLookupResult::Found { value, .. } => {
                Ok(!(self.c_mode && matches!(value, PreparseSymbolKind::Tag)))
            }
            QualifiedLookupResult::NotFound => Ok(false),
            QualifiedLookupResult::Ambiguous { candidates } => parse_point_error(
                &self.tokens,
                format!(
                    "Ambiguous identifier '{}', candidates: {}",
                    name,
                    candidates
                        .iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            ),
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

impl QualifiedLookup for ParserData<'_> {
    type Output = PreparseSymbolKind;

    fn lookup_local(
        &self,
        _lexical_namespace: &NamespacePath,
        _name: &QualifiedName,
    ) -> Option<PreparseSymbolKind> {
        None
    }

    fn lookup_exact(
        &self,
        _lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<PreparseSymbolKind> {
        self.registry.get_symbol(&name.namespace, &name.name)
    }

    fn resolve_aliases(
        &self,
        _lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.namespace_aliases
            .get(namespace)
            .cloned()
            .unwrap_or_default()
    }
}
