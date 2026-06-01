use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};
use speedy::{Readable, Writable};

use crate::ast::{
    expression::CXExpression,
    modifiers::{CXLinkageMode, CXTypeQualifiers},
    template::CXTemplateInput,
    types::{CXType, CXTypeKind, PredeclarationType},
};

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct CXFunctionContract {
    pub safe: bool,

    pub precondition: Option<CXExpression>,
    pub postcondition: Option<(Option<CXIdent>, CXExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXFunctionPrototype {
    pub kind: CXFunctionKind,
    pub params: Vec<CXParameter>,
    pub return_type: CXType,
    pub var_args: bool,
    pub contract: CXFunctionContract,
    pub linkage: CXLinkageMode,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CXFunctionTypeIdent {
    Standard(QualifiedName),
    Templated(QualifiedName, CXTemplateInput),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CXFunctionKind {
    Standard(CXIdent),
    MemberFunction {
        member_type: CXFunctionTypeIdent,
        name: CXIdent,
        receiver: CXReceiverData,
    },
    StaticMemberFunction {
        member_type: CXFunctionTypeIdent,
        name: CXIdent,
    },
}

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXReceiverMode {
    #[default]
    None,
    ByRef,
    ByMove,
}

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXReceiverData {
    pub mode: CXReceiverMode,
    pub specifiers: CXTypeQualifiers,
}

impl CXFunctionKind {
    pub fn implicit_member(&self) -> Option<&CXFunctionTypeIdent> {
        match self {
            CXFunctionKind::MemberFunction { member_type, .. } => Some(member_type),
            CXFunctionKind::Standard(_) | CXFunctionKind::StaticMemberFunction { .. } => None,
        }
    }

    pub fn receiver(&self) -> Option<&CXReceiverData> {
        match self {
            CXFunctionKind::MemberFunction { receiver, .. } => Some(receiver),
            CXFunctionKind::Standard(_) | CXFunctionKind::StaticMemberFunction { .. } => None,
        }
    }

    pub fn into_key(&self) -> QualifiedName {
        match self {
            CXFunctionKind::Standard(name) => QualifiedName::new_raw(name.clone()),
            CXFunctionKind::MemberFunction {
                member_type, name, ..
            } => member_function_key(member_type.base_name(), name),
            CXFunctionKind::StaticMemberFunction { member_type, name } => {
                member_function_key(member_type.base_name(), name)
            }
        }
    }
}

impl CXFunctionTypeIdent {
    pub fn from_type(ty: &CXType) -> Option<CXFunctionTypeIdent> {
        match &ty.kind {
            CXTypeKind::Identifier {
                name,
                template_input: Some(input),
                ..
            } => Some(CXFunctionTypeIdent::Templated(name.clone(), input.clone())),
            CXTypeKind::Identifier { name, .. } => {
                Some(CXFunctionTypeIdent::Standard(name.clone()))
            }
            _ => None,
        }
    }

    pub fn base_name(&self) -> &QualifiedName {
        match self {
            CXFunctionTypeIdent::Standard(name) => name,
            CXFunctionTypeIdent::Templated(name, _) => name,
        }
    }

    pub fn as_type(&self) -> CXType {
        match self {
            CXFunctionTypeIdent::Standard(name) => CXTypeKind::Identifier {
                name: name.clone(),
                predeclaration: PredeclarationType::None,
                template_input: None,
            }
            .to_type(),
            CXFunctionTypeIdent::Templated(name, input) => CXTypeKind::Identifier {
                name: name.clone(),
                predeclaration: PredeclarationType::None,
                template_input: Some(input.clone()),
            }
            .to_type(),
        }
    }
}

fn member_function_key(type_base_name: &QualifiedName, name: &CXIdent) -> QualifiedName {
    QualifiedName::new(
        type_base_name.namespace.child(type_base_name.name.clone()),
        name.clone(),
    )
}
