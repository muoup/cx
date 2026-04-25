use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::ast::{CXExpression, VisibilityMode};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXFunctionTypeIdent {
    Standard(CXIdent),
    Templated(CXIdent, CXTemplateInput),
}

pub type CXTypeKey = String;

/**
 *  Represents the skeleton of the function, useful for looking up functions in a map
 */
#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXFunctionKey {
    Standard(CXIdent),
    MemberFunction {
        type_base_name: CXIdent,
        name: CXIdent,
    },
    StaticMemberFunction {
        type_base_name: CXIdent,
        name: CXIdent,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Readable, Writable)]
pub enum CXLinkageMode {
    #[default]
    Standard,
    Static,
    Extern,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Readable, Writable)]
pub struct ModuleResource<Resource> {
    pub visibility: VisibilityMode,
    pub linkage: CXLinkageMode,

    pub external_module: Option<String>,
    pub resource: Resource,
}

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq, Readable, Writable)]
pub enum PredeclarationType {
    #[default]
    None,
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXStructAttributes {
    pub nocopy: bool,
    pub nodrop: bool,
    pub copy_traits: Option<String>,
}

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXFunctionContract {
    pub safe: bool,

    pub precondition: Option<CXExpression>,
    pub postcondition: Option<(Option<CXIdent>, CXExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXFunctionPrototype {
    pub kind: CXFunctionKind,
    pub params: Vec<CXParameter>,
    pub return_type: CXType,
    pub var_args: bool,
    pub contract: CXFunctionContract,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXTemplatePrototype {
    pub types: Vec<String>,
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXTemplate<Shell> {
    pub prototype: CXTemplatePrototype,
    pub shell: Shell,
}

pub type CXTypeTemplate = CXTemplate<CXType>;
pub type CXFunctionTemplate = CXTemplate<CXFunctionPrototype>;
pub type CXTypeQualifiers = u8;

pub const CX_CONST: CXTypeQualifiers = 1 << 0;
pub const CX_VOLATILE: CXTypeQualifiers = 1 << 1;
pub const CX_RESTRICT: CXTypeQualifiers = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeQualifiers = 1 << 3;
pub const CX_UNION: CXTypeQualifiers = 1 << 4;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXTemplateInput {
    pub params: Vec<CXType>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXType {
    pub kind: CXTypeKind,
    pub specifiers: CXTypeQualifiers,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXField {
    Standard {
        name: String,
        _type: CXType,
    },
    Bitfield {
        name: Option<String>,
        integer_type: CXType,
        width: usize,
    },
}

impl CXField {
    pub fn standard(name: String, _type: CXType) -> Self {
        Self::Standard { name, _type }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            CXField::Standard { name, .. } => Some(name.as_str()),
            CXField::Bitfield { name, .. } => name.as_deref(),
        }
    }

    pub fn standard_parts(&self) -> Option<(&String, &CXType)> {
        match self {
            CXField::Standard { name, _type } => Some((name, _type)),
            CXField::Bitfield { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXTypeKind {
    Identifier {
        name: CXIdent,
        predeclaration: PredeclarationType,
    },
    TemplatedIdentifier {
        name: CXIdent,
        input: CXTemplateInput,
    },

    ExplicitSizedArray(Box<CXType>, Box<CXExpression>),
    ImplicitSizedArray(Box<CXType>),

    MemoryReference {
        inner_type: Box<CXType>,
    },
    PointerTo {
        inner_type: Box<CXType>,
    },

    Structured {
        name: Option<CXIdent>,
        attributes: CXStructAttributes,
        fields: Vec<CXField>,
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<CXField>,
    },
    TaggedUnion {
        name: CXIdent,
        attributes: CXStructAttributes,
        variants: Vec<CXField>,
    },

    FunctionPointer {
        prototype: Box<CXFunctionPrototype>,
    },
}

impl CXType {
    pub fn new(specifiers: CXTypeQualifiers, kind: CXTypeKind) -> Self {
        Self { kind, specifiers }
    }

    pub fn pointer_to(self, specifier: CXTypeQualifiers) -> Self {
        Self::new(
            specifier,
            CXTypeKind::PointerTo {
                inner_type: Box::new(self),

            },
        )
    }

    pub fn add_specifier(mut self, specifier: CXTypeQualifiers) -> Self {
        self.specifiers |= specifier;
        self
    }

    pub fn get_name(&self) -> Option<&CXIdent> {
        match &self.kind {
            CXTypeKind::Identifier { name, .. } => Some(name),
            CXTypeKind::TemplatedIdentifier { name, .. } => Some(name),
            CXTypeKind::TaggedUnion { name, .. } => Some(name),

            CXTypeKind::Structured { name, .. } => name.as_ref(),
            CXTypeKind::Union { name, .. } => name.as_ref(),

            _ => None,
        }
    }

    pub fn set_name(&mut self, to: CXIdent) {
        match &mut self.kind {
            CXTypeKind::Union { name, .. } | CXTypeKind::Structured { name, .. } => {
                *name = Some(to);
            }

            CXTypeKind::TaggedUnion { name, .. } => {
                *name = to;
            }

            _ => {}
        }
    }
}

impl CXTypeKind {
    pub fn to_type(self) -> CXType {
        CXType::new(0, self)
    }
}

impl<T: Clone> ModuleResource<T> {
    pub fn transfer(&self, from_module: impl AsRef<str>) -> ModuleResource<T> {
        if self.linkage == CXLinkageMode::Static {
            panic!("Static linkage resources must be declared private");
        }

        ModuleResource {
            visibility: VisibilityMode::Private,
            linkage: CXLinkageMode::Extern,
            external_module: Some(from_module.as_ref().to_string()),
            resource: self.resource.clone(),
        }
    }

    pub fn standard(resource: T) -> ModuleResource<T> {
        ModuleResource {
            visibility: VisibilityMode::Private,
            linkage: CXLinkageMode::Standard,
            external_module: None,
            resource,
        }
    }

    pub fn new(
        resource: T,
        visibility: VisibilityMode,
        linkage: CXLinkageMode,
    ) -> ModuleResource<T> {
        ModuleResource {
            visibility,
            linkage,
            external_module: None,
            resource,
        }
    }
}

impl CXFunctionTypeIdent {
    pub fn as_type(&self) -> CXType {
        match self {
            CXFunctionTypeIdent::Standard(name) => CXTypeKind::Identifier {
                name: name.clone(),
                predeclaration: PredeclarationType::None,
            }
            .to_type(),
            CXFunctionTypeIdent::Templated(name, input) => CXTypeKind::TemplatedIdentifier {
                name: name.clone(),
                input: input.clone(),
            }
            .to_type(),
        }
    }
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

    pub fn into_key(&self) -> CXFunctionKey {
        match self {
            CXFunctionKind::Standard(name) => CXFunctionKey::Standard(name.clone()),
            CXFunctionKind::MemberFunction {
                member_type, name, ..
            } => {
                let type_base_name = member_type.base_name().clone();

                CXFunctionKey::MemberFunction {
                    type_base_name,
                    name: name.clone(),
                }
            }
            CXFunctionKind::StaticMemberFunction { member_type, name } => {
                let type_base_name = member_type.base_name().clone();

                CXFunctionKey::StaticMemberFunction {
                    type_base_name,
                    name: name.clone(),
                }
            }
        }
    }
}

impl CXFunctionTypeIdent {
    pub fn from_type(ty: &CXType) -> Option<CXFunctionTypeIdent> {
        match &ty.kind {
            CXTypeKind::Identifier { name, .. } => {
                Some(CXFunctionTypeIdent::Standard(name.clone()))
            }
            CXTypeKind::TemplatedIdentifier { name, input } => {
                Some(CXFunctionTypeIdent::Templated(name.clone(), input.clone()))
            }
            _ => None,
        }
    }

    pub fn base_name(&self) -> &CXIdent {
        match self {
            CXFunctionTypeIdent::Standard(name) => name,
            CXFunctionTypeIdent::Templated(name, _) => name,
        }
    }
}
