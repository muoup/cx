use std::collections::HashMap;

use cx_ast::ast::{function::CXFunctionKind, CXASTDefinition, CXASTStmt, CXAST};
use cx_log::CXResult;
use cx_namespace::result::QualifiedLookupResult;
use cx_namespace::MIRQualifiedLookup;
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_preparse_data::symbol_data::PreparseSymbolKind;
use cx_preparse_data::{NamespaceAliases, PreparseContents, VisibilityMode};
use cx_tokens::TokenIter;
use cx_util::identifier::CXIdent;
use cx_util::module_path::ModulePath;
use cx_util::namespace::{EnvironmentNamespace, NamespacePath, QualifiedName};

use crate::log::parse_point_error;

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub extern_c_mode: bool,
    pub expr_commas: Vec<bool>,
    pub pp_contents: &'a PreparseContents,
    pub file_origin: EnvironmentNamespace,
    // uses u8 mapping instead of a set to prevent problems with shadowing
    pub temporary_type_names: HashMap<CXIdent, u8>,
    namespace_aliases: NamespaceAliases,

    pub registry: &'a GlobalPreparseRegistry,
    pub ast: CXAST,
}

impl<'a> ParserData<'a> {
    pub fn new(
        tokens: TokenIter<'a>,
        pp_contents: &'a PreparseContents,
        registry: &'a GlobalPreparseRegistry,
    ) -> Self {
        let file_origin = EnvironmentNamespace::from(pp_contents.module_symbols.namespace.clone());

        Self {
            tokens,
            visibility: VisibilityMode::Package,
            extern_c_mode: false,
            expr_commas: vec![true],
            pp_contents,
            file_origin,
            registry,
            temporary_type_names: HashMap::new(),
            namespace_aliases: pp_contents.namespace_aliases.clone(),
            ast: CXAST::new(
                ModulePath::from_source_path(pp_contents.module.as_str()),
                pp_contents.imports.clone(),
            ),
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

    pub fn file_origin_for_range(
        &self,
        _start_token: usize,
        _end_token: usize,
    ) -> EnvironmentNamespace {
        self.file_origin.clone()
    }

    pub fn get_comma_mode(&self) -> bool {
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }

    pub fn add_stmt(&mut self, stmt: CXASTStmt) {
        let namespace = self.namespace_for_current_stmt();
        self.register_stmt_namespace_aliases(&namespace, &stmt);
        self.ast
            .definition_stmts
            .push(CXASTDefinition { namespace, stmt })
    }

    pub fn take_ast(mut self) -> CXAST {
        self.ast.namespace_aliases = self.namespace_aliases;
        self.ast
    }

    pub fn is_type_ident(&self, name: &QualifiedName) -> CXResult<bool> {
        Ok(self.query_identifier(name.clone())?
            || (name.namespace.is_root() && self.temporary_type_names.contains_key(&name.name)))
    }

    pub fn query_identifier(&self, name: QualifiedName) -> CXResult<bool> {
        match self.qualified_lookup(&self.namespace_for_current_stmt(), &name) {
            QualifiedLookupResult::Found { .. } => Ok(true),
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

    fn namespace_for_current_stmt(&self) -> NamespacePath {
        if self.extern_c_mode {
            NamespacePath::root()
        } else {
            self.current_module_namespace()
        }
    }

    fn current_module_namespace(&self) -> NamespacePath {
        self.pp_contents.module_symbols.namespace.clone()
    }

    fn register_stmt_namespace_aliases(&mut self, namespace: &NamespacePath, stmt: &CXASTStmt) {
        if namespace.is_root() {
            return;
        }

        if let CXASTStmt::FunctionDefinition { prototype, .. } = stmt {
            let q_namespace = prototype.kind.into_key().namespace;

            if !q_namespace.is_root()
                && matches!(&prototype.kind, CXFunctionKind::AssociatedFunction { .. })
            {
                let entry = self.namespace_aliases.entry(namespace.clone()).or_default();

                if !entry.contains(&q_namespace) {
                    entry.push(q_namespace);
                }
            }
        }
    }
}

impl MIRQualifiedLookup for ParserData<'_> {
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
