use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use cx_ast::ast::{CXAST, CXASTStmt};
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_preparse_data::{PreparseContents, VisibilityMode};
use cx_tokens::TokenIter;
use cx_util::identifier::CXIdent;
use cx_util::module_path::ModulePath;
use cx_util::namespace::{NamespacePath, QualifiedName};

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,
    pub pp_contents: &'a PreparseContents,
    pub file_origin: Arc<str>,
    // uses u8 mapping instead of a set to prevent problems with shadowing
    pub temporary_type_names: HashMap<CXIdent, u8>,
    namespace_aliases: HashMap<NamespacePath, NamespacePath>,

    pub registry: &'a GlobalPreparseRegistry,
    pub ast: CXAST,
}

impl<'a> ParserData<'a> {
    pub fn new(
        tokens: TokenIter<'a>,
        pp_contents: &'a PreparseContents,
        registry: &'a GlobalPreparseRegistry,
    ) -> Self {
        let file_origin: Arc<str> = Arc::from(tokens.file.to_string_lossy().as_ref());

        Self {
            tokens,
            visibility: VisibilityMode::Package,
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

    pub fn file_origin_for_range(&self, start_token: usize, end_token: usize) -> Arc<str> {
        self.tokens
            .slice
            .get(start_token)
            .map(|token| token.file_origin.clone())
            .or_else(|| {
                end_token
                    .checked_sub(1)
                    .and_then(|index| self.tokens.slice.get(index))
                    .map(|token| token.file_origin.clone())
            })
            .filter(|origin| !origin.as_os_str().is_empty())
            .map(|origin| Arc::from(origin.to_string_lossy().as_ref()))
            .unwrap_or_else(|| self.file_origin.clone())
    }

    pub fn get_comma_mode(&self) -> bool {
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }

    pub fn add_stmt(&mut self, stmt: CXASTStmt) {
        self.ast.definition_stmts.push(stmt)
    }

    pub fn take_ast(mut self) -> CXAST {
        self.ast.namespace_aliases = self.namespace_aliases;
        self.ast
    }

    pub fn resolve_qualified_alias<'b>(&self, name: &'b QualifiedName) -> Cow<'b, QualifiedName> {
        if let Some(alias) = self.namespace_aliases.get(&name.namespace) {
            Cow::Owned(QualifiedName {
                namespace: alias.clone(),
                name: name.name.clone(),
            })
        } else {
            Cow::Borrowed(name)
        }
    }

    pub fn is_type_ident(&self, name: &QualifiedName) -> bool {
        let resolved_name = self.resolve_qualified_alias(name);

        self.registry
            .get_symbol(&resolved_name.namespace, &resolved_name.name)
            .is_some()
            || (name.namespace.is_root() && self.temporary_type_names.contains_key(&name.name))
    }
}
