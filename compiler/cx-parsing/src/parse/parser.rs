use std::sync::Arc;

use cx_ast::ast::VisibilityMode;
use cx_ast::{
    ast::{CXFunctionStmt, CXGlobalVariable, CXAST},
    data::{
        CXFunctionPrototype, CXFunctionTemplate, CXLinkageMode, CXTemplatePrototype, CXType,
        CXTypeTemplate, ModuleResource,
    },
};
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_preparse_data::symbol_data::PreparseSymbolKind;
use cx_preparse_data::PreparseContents;
use cx_tokens::TokenIter;
use cx_util::namespace::QualifiedName;
use cx_util::CXResult;

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,
    pub pp_contents: &'a PreparseContents,
    pub file_origin: Arc<str>,

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
            ast: CXAST {
                imports: pp_contents.imports.clone(),
                ..Default::default()
            },
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

    pub fn add_type(
        &mut self,
        name: String,
        _type: CXType,
        prototype: Option<CXTemplatePrototype>,
    ) {
        match prototype {
            Some(proto) => {
                self.ast.type_data.insert_template(
                    name,
                    ModuleResource::new(
                        CXTypeTemplate {
                            prototype: proto.clone(),
                            shell: _type,
                        },
                        self.visibility,
                        CXLinkageMode::Standard,
                    ),
                );
            }
            None => {
                self.ast.type_data.insert_standard(
                    name,
                    ModuleResource::new(_type, self.visibility, CXLinkageMode::Standard),
                );
            }
        }
    }

    pub fn add_function(
        &mut self,
        function: CXFunctionPrototype,
        prototype: Option<CXTemplatePrototype>,
    ) {
        match prototype {
            Some(proto) => {
                let linkage = function.linkage;
                self.ast.function_data.insert_template(
                    function.kind.into_key(),
                    ModuleResource::new(
                        CXFunctionTemplate {
                            prototype: proto.clone(),
                            shell: function,
                        },
                        self.visibility,
                        linkage,
                    ),
                );
            }
            None => {
                let linkage = function.linkage;
                self.ast.function_data.insert_standard(
                    function.kind.into_key(),
                    ModuleResource::new(function, self.visibility, linkage),
                );
            }
        }
    }

    pub fn add_global_variable(
        &mut self,
        name: String,
        var: CXGlobalVariable,
        linkage: CXLinkageMode,
    ) {
        self.ast
            .global_variables
            .insert(name, ModuleResource::new(var, self.visibility, linkage));
    }

    pub fn add_function_stmt(&mut self, stmt: CXFunctionStmt) {
        self.ast.function_stmts.push(stmt)
    }

    pub fn take_ast(self) -> CXAST {
        self.ast
    }

    pub fn is_type_ident(&self, name: &QualifiedName) -> CXResult<bool> {
        if name.namespace.is_root() && self.ast.type_data.is_key_any(&name.name.as_string()) {
            return Ok(true);
        }

        let namespace = if name.namespace.is_root() {
            &self.pp_contents.module_symbols.namespace
        } else {
            &name.namespace
        };

        Ok(matches!(
            self.registry.get_symbol(namespace, &name.name),
            Some(PreparseSymbolKind::Type)
        ))
    }
}
