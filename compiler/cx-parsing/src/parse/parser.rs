use std::collections::HashMap;
use std::sync::Arc;

use cx_ast::ast::{function::CXFunctionKind, CXASTDefinition, CXASTStmt, CXAST};
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_preparse_data::{NamespaceAliases, PreparseContents, VisibilityMode};
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

    pub fn resolve_qualified_aliases(&self, name: &QualifiedName) -> Vec<QualifiedName> {
        let mut candidates = Vec::new();
        push_unique(&mut candidates, name.clone());

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
                    &mut candidates,
                    QualifiedName {
                        namespace: target.join(&suffix),
                        name: name.name.clone(),
                    },
                );
            }
        }

        candidates
    }

    pub fn is_type_ident(&self, name: &QualifiedName) -> bool {
        self.resolve_qualified_aliases(name)
            .iter()
            .any(|resolved_name| {
                self.registry
                    .get_symbol(&resolved_name.namespace, &resolved_name.name)
                    .is_some()
            })
            || (name.namespace.is_root() && self.temporary_type_names.contains_key(&name.name))
    }

    fn namespace_for_current_stmt(&self) -> NamespacePath {
        let Some(token) = self.tokens.prev() else {
            return self.current_module_namespace();
        };

        if token.file_origin.as_ref() == self.tokens.file.as_path() {
            self.current_module_namespace()
        } else {
            NamespacePath::root()
        }
    }

    fn current_module_namespace(&self) -> NamespacePath {
        self.pp_contents.module_symbols.namespace.clone()
    }

    fn register_stmt_namespace_aliases(&mut self, namespace: &NamespacePath, stmt: &CXASTStmt) {
        if namespace.is_root() {
            return;
        }

        match stmt {
            CXASTStmt::TypeDefinition {
                name: Some(name), ..
            } => {
                insert_namespace_alias(
                    &mut self.namespace_aliases,
                    NamespacePath::root().child(name.clone()),
                    namespace.child(name.clone()),
                );
            }

            CXASTStmt::FunctionDefinition { prototype, .. } => {
                let q_namespace = prototype.kind.into_key().namespace;

                if !q_namespace.is_root()
                    && matches!(
                        &prototype.kind,
                        CXFunctionKind::MemberFunction { .. }
                            | CXFunctionKind::StaticMemberFunction { .. }
                    )
                {
                    insert_namespace_alias(
                        &mut self.namespace_aliases,
                        q_namespace.clone(),
                        namespace.join(&q_namespace),
                    );
                }
            }

            _ => {}
        }
    }
}

fn insert_namespace_alias(
    aliases: &mut NamespaceAliases,
    alias: NamespacePath,
    target: NamespacePath,
) {
    let targets = aliases.entry(alias).or_default();
    if !targets.contains(&target) {
        targets.push(target);
    }
}

fn push_unique(candidates: &mut Vec<QualifiedName>, name: QualifiedName) {
    if !candidates.contains(&name) {
        candidates.push(name);
    }
}
