use cx_data_ast::preparse::{naive_types::{CXNaiveType, ModuleResource}, templates::{CXTemplatePrototype, CXTypeTemplate}};
use cx_data_lexer::TokenIter;
use cx_util::identifier::CXIdent;

use crate::preparse::PreparseData;

pub mod type_parsing;
pub mod decl_parsing;
pub mod data_parsing;
pub mod function_parsing;

pub enum DeclarationStatement {
    Import(String),
    
    TypeDeclaration(TypeDeclaration),
    FunctionDeclaration,
}

#[non_exhaustive]
pub(crate) enum TypeDeclaration {
    Standard {
        name: Option<CXIdent>,
        type_: CXNaiveType,
    },

    Template {
        name: Option<CXIdent>,
        template_prototype: CXTemplatePrototype,
        type_: CXNaiveType,
    },
}

impl TypeDeclaration {
    pub(crate) fn new(
        name: Option<CXIdent>,
        type_: CXNaiveType,
        template_prototype: Option<CXTemplatePrototype>,
    ) -> Self {
        if let Some(template_prototype) = template_prototype {
            TypeDeclaration::Template {
                name,
                template_prototype,
                type_,
            }
        } else {
            TypeDeclaration::Standard { name, type_ }
        }
    }

    pub(crate) fn assert_standard_type(self, iter: &TokenIter) -> CXNaiveType {
        match self {
            TypeDeclaration::Standard { type_, .. } => type_,

            _ => log_preparse_error!(iter, "Expected standard type declaration."),
        }
    }

    pub(crate) fn add_to(self, data: &mut PreparseData) {
        match self {
            TypeDeclaration::Standard { name: None, .. } => {}
            TypeDeclaration::Standard {
                name: Some(name),
                type_,
            } => data.contents.type_definitions.insert_standard(
                name.as_string(),
                ModuleResource::with_visibility(type_, data.visibility_mode),
            ),

            TypeDeclaration::Template { name: None, .. } => {}
            TypeDeclaration::Template {
                name: Some(name),
                template_prototype,
                type_,
            } => data.contents.type_definitions.insert_template(
                name.as_string(),
                ModuleResource::with_visibility(
                    CXTypeTemplate {
                        name: name.clone(),
                        prototype: template_prototype,
                        shell: type_,
                    },
                    data.visibility_mode,
                ),
            ),
        }
    }
}