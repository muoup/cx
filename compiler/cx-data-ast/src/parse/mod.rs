use speedy::{Readable, Writable};
use cx_util::mangling::{mangle_destructor, mangle_member_function, mangle_templated_fn};
use crate::parse::identifier::CXIdent;
use crate::parse::value_type::CXType;
use crate::preparse::CXNaiveFnIdent;
use crate::preparse::pp_type::{CXNaiveTemplateInput, CXNaiveType};

pub mod ast;
pub mod maps;
pub mod type_mapping;

pub mod intrinsic_types;
pub mod value_type;
pub mod identifier;
pub mod parser;
pub mod macros;
pub mod template;

mod format;

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXObjectIdentifier {
    Standard(CXIdent),
    Templated {
        name: CXIdent,
        template_input: CXNaiveTemplateInput
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXFunctionIdentifier {
    Standard(CXIdent),
    MemberFunction {
        object: CXObjectIdentifier,
        function_name: CXIdent
    },
    Destructor(CXIdent)
}

impl CXObjectIdentifier {
    pub fn name(&self) -> &str {
        match self {
            CXObjectIdentifier::Standard(name) |
            CXObjectIdentifier::Templated { name, .. } => name.as_str(),
        }
    }
    
    pub fn as_type(&self) -> CXNaiveType {
        match self {
            CXObjectIdentifier::Standard(name) => {
                CXNaiveType {
                    uuid: 0,
                    kind: crate::preparse::pp_type::CXNaiveTypeKind::Identifier {
                        name: name.clone(),
                        predeclaration: crate::preparse::pp_type::PredeclarationType::None
                    },
                    specifiers: 0
                }
            },
            CXObjectIdentifier::Templated { name, template_input: template_args } => {
                CXNaiveType {
                    uuid: 0,
                    kind: crate::preparse::pp_type::CXNaiveTypeKind::TemplatedIdentifier {
                        name: name.clone(),
                        input: template_args.clone()
                    },
                    specifiers: 0
                }
            }
        }
    }
} 

impl CXFunctionIdentifier {
    pub fn as_ident(&self) -> CXIdent {
        CXIdent::from(self.as_string())
    }
    
    pub fn as_string(&self) -> String {
        match self {
            CXFunctionIdentifier::Standard(name) => name.to_string(),
            CXFunctionIdentifier::MemberFunction { object, function_name, .. } => {
                mangle_member_function(object.name(), function_name.as_str())
            },
            CXFunctionIdentifier::Destructor(_type) => {
                mangle_destructor(_type.as_str())
            }
        }
    }
    
    pub fn map_name_ident<F>(&mut self, mapping: F)
        where F: FnOnce(&str) -> String {
        match self {
            CXFunctionIdentifier::Standard(name) => {
                let new_name = mapping(name.as_str());
                name.set_data(new_name);
            },
            CXFunctionIdentifier::MemberFunction { function_name, .. } => {
                let new_name = mapping(function_name.as_str());
                function_name.set_data(new_name);
            },
            CXFunctionIdentifier::Destructor(_type) => {
                let new_name = mapping(_type.as_str());
                _type.set_data(new_name);
            }
        }
    }
}