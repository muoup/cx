use cx_parsing_data::{
    parse::parser::VisibilityMode,
    preparse::{
        naive_types::{CXNaivePrototype, CXNaiveType, ModuleResource},
        templates::{CXFunctionTemplate, CXTemplatePrototype, CXTypeTemplate},
        CXNaiveFnMap, CXNaiveTypeMap,
    },
};
use cx_util::identifier::CXIdent;

use crate::preparse::PreparseData;

pub mod data_parsing;
pub mod decl_parsing;

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
            DeclarationStatement::FunctionDeclaration(decl) => decl.add_to(data),
            DeclarationStatement::TypeDeclaration(decl) => decl.add_pp(data),

            DeclarationStatement::Import(import) => data.contents.imports.push(import),
            DeclarationStatement::ChangeVisibility(mode) => data.visibility_mode = mode,

            DeclarationStatement::None => {}
        }
    }
}

impl FunctionDeclaration {
    pub(crate) fn add_to(self, data: &mut PreparseData) {
        data.contents
            .func_idents
            .push(ModuleResource::with_visibility(
                match self.template_prototype {
                    Some(prototype) => {
                        PreparseIdentifier::Templated(self.prototype.name.clone(), prototype)
                    }
                    None => PreparseIdentifier::Standard(self.prototype.name.clone()),
                },
                data.visibility_mode,
            ));
    }

    pub(crate) fn add_map(self, map: &mut CXNaiveFnMap, visibility: VisibilityMode) {
        match self.template_prototype {
            Some(prototype) => {
                let template = CXFunctionTemplate {
                    prototype,
                    shell: self.prototype,
                };

                map.insert_template(
                    template.shell.name.clone(),
                    ModuleResource::with_visibility(template, visibility),
                );
            }
            None => {
                map.insert_standard(
                    self.prototype.name.clone(),
                    ModuleResource::with_visibility(self.prototype, visibility),
                );
            }
        }
    }
}
