use cx_data_ast::preparse::{naive_types::{CXNaivePrototype, CXNaiveType, ModuleResource}, templates::{CXTemplatePrototype, CXTypeTemplate}};
use cx_data_lexer::TokenIter;
use cx_data_typechecker::cx_types::CXFunctionIdentifier;
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
    name: Option<CXIdent>,
    type_: CXNaiveType,
    template_prototype: Option<CXTemplatePrototype>
}

#[non_exhaustive]
pub struct FunctionDeclaration {
    name: CXFunctionIdentifier,
    prototype: CXNaivePrototype,
    template_prototype: Option<CXTemplatePrototype>,
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
                            name: name,
                            prototype,
                            shell: self.type_
                        },
                        data.visibility_mode,
                    ),
                );
            },
            
            None => {
                data.contents.type_definitions.insert_standard(
                    name.as_string(),
                    ModuleResource::with_visibility(
                        self.type_,
                        data.visibility_mode,
                    ),
                );
            }
        }
    }
}