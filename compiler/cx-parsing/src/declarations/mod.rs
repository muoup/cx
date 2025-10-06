use cx_parsing_data::{parse::parser::VisibilityMode, preparse::{
    naive_types::{CXNaivePrototype, CXNaiveType, ModuleResource},
    templates::{CXFunctionTemplate, CXTemplatePrototype, CXTypeTemplate},
}};
use cx_util::identifier::CXIdent;

use crate::preparse::PreparseData;

pub mod data_parsing;
pub mod decl_parsing;
pub mod function_parsing;
pub mod type_parsing;

#[allow(unused)]
pub enum DeclarationStatement {
    Import(String),
    TypeDeclaration(TypeDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ChangeVisibility(VisibilityMode),

    /**
     *  Not indicative of an error, there are statements the Preparser can recognize, but has no
     *  use for. Most commonly, this is either extraneous semicolons that can be safely ignored, or
     *  global variables which are handled by the parser as parsing expressions requires full context
     *  of types and functions, which is what the Preparser is trying to build up.
     */
    None,
}

#[non_exhaustive]
pub struct TypeDeclaration {
    pub name: Option<CXIdent>,
    pub type_: CXNaiveType,
    pub template_prototype: Option<CXTemplatePrototype>,
}

#[non_exhaustive]
pub struct FunctionDeclaration {
    pub prototype: CXNaivePrototype,
    pub template_prototype: Option<CXTemplatePrototype>,
}

impl DeclarationStatement {
    pub(crate) fn add_to(self, data: &mut PreparseData) {
        match self {
            DeclarationStatement::FunctionDeclaration(decl) =>
                decl.add_to(data),
            DeclarationStatement::TypeDeclaration(decl) =>
                decl.add_to(data),

            DeclarationStatement::Import(import) =>
                data.contents.imports.push(import),
            DeclarationStatement::ChangeVisibility(mode) =>
                data.visibility_mode = mode,

            DeclarationStatement::None => {}
        }
    }
}

impl TypeDeclaration {
    pub(crate) fn add_to(self, data: &mut PreparseData) {
        let Some(name) = self.name else {
            // We cannot add unnamed types to the global scope.
            return;
        };

        match self.template_prototype {
            Some(prototype) => {
                data.contents.type_definitions.insert_template(
                    name.as_string(),
                    ModuleResource::with_visibility(
                        CXTypeTemplate {
                            prototype,
                            shell: self.type_,
                        },
                        data.visibility_mode,
                    ),
                );
            }

            None => {
                data.contents.type_definitions.insert_standard(
                    name.as_string(),
                    ModuleResource::with_visibility(self.type_, data.visibility_mode),
                );
            }
        }
    }
}

impl FunctionDeclaration {
    pub(crate) fn add_to(self, data: &mut PreparseData) {
        match self.template_prototype {
            Some(prototype) => {
                data.contents.function_definitions.insert_template(
                    self.prototype.name.clone(),
                    ModuleResource::with_visibility(
                        CXFunctionTemplate {
                            prototype,
                            shell: self.prototype,
                        },
                        data.visibility_mode,
                    ),
                );
            }

            None => {
                data.contents.function_definitions.insert_standard(
                    self.prototype.name.clone(),
                    ModuleResource::with_visibility(self.prototype, data.visibility_mode),
                );
            }
        }
    }
}
