use crate::{
    parse::ast::CXAST,
    preparse::{
        naive_types::{CXNaivePrototype, CXNaiveType, ModuleResource},
        templates::{CXFunctionTemplate, CXTemplatePrototype, CXTypeTemplate},
    },
    PreparseContents,
};
use cx_lexer_data::TokenIter;
use speedy::{Readable, Writable};

#[derive(Debug, Default, Hash, Clone, PartialOrd, PartialEq, Eq, Copy, Readable, Writable)]
pub enum VisibilityMode {
    #[default]
    Private,
    Package,
    Public,
}

#[derive(Debug)]
pub struct ParserData<'a> {
    pub tokens: TokenIter<'a>,
    pub visibility: VisibilityMode,
    pub expr_commas: Vec<bool>,
    pub pp_contents: &'a PreparseContents,

    pub ast: CXAST,
}

impl<'a> ParserData<'a> {
    pub fn back(&mut self) -> &mut Self {
        self.tokens.back();
        self
    }

    pub fn skip(&mut self) -> &mut Self {
        self.tokens.next();
        self
    }

    pub fn reset(&mut self) {
        self.tokens.index = 0;
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

    pub fn get_comma_mode(&self) -> bool {
        *self
            .expr_commas
            .last()
            .expect("CRITICAL: No comma mode to get!")
    }

    pub fn add_type(
        &mut self,
        name: String,
        type_: CXNaiveType,
        prototype: Option<CXTemplatePrototype>,
    ) {
        match prototype {
            Some(proto) => {
                self.ast.type_data.insert_template(
                    name,
                    ModuleResource::with_visibility(
                        CXTypeTemplate {
                            prototype: proto.clone(),
                            shell: type_,
                        },
                        self.visibility,
                    ),
                );
            }
            None => {
                self.ast.type_data.insert_standard(
                    name,
                    ModuleResource::with_visibility(type_, self.visibility),
                );
            }
        }
    }

    pub fn add_function(
        &mut self,
        function: CXNaivePrototype,
        prototype: Option<CXTemplatePrototype>,
    ) {
        match prototype {
            Some(proto) => {
                self.ast.function_data.insert_template(
                    function.name.clone(),
                    ModuleResource::with_visibility(
                        CXFunctionTemplate {
                            prototype: proto.clone(),
                            shell: function,
                        },
                        self.visibility,
                    ),
                );
            }
            None => {
                self.ast.function_data.insert_standard(
                    function.name.clone(),
                    ModuleResource::with_visibility(function, self.visibility),
                );
            }
        }
    }
}
