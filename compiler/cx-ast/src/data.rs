use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::ast::{CXExpr, VisibilityMode};

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
    Destructor {
        type_base_name: CXIdent,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXFunctionKind {
    Standard(CXIdent),
    MemberFunction {
        member_type: CXFunctionTypeIdent,
        name: CXIdent,
    },
    StaticMemberFunction {
        member_type: CXFunctionTypeIdent,
        name: CXIdent,
    },
    Destructor(CXFunctionTypeIdent),
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

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXFunctionContract {
    pub safe: bool,
    
    pub precondition: Option<CXExpr>,
    pub postcondition: Option<(Option<CXIdent>, CXExpr)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXPrototype {
    pub kind: CXFunctionKind,
    pub params: Vec<CXParameter>,
    pub return_type: CXType,
    pub var_args: bool,
    pub contract: CXFunctionContract,
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
pub type CXFunctionTemplate = CXTemplate<CXPrototype>;
pub type CXDestructorTemplate = CXTemplate<String>;

pub type CXTypeSpecifier = u8;

pub const CX_CONST: CXTypeSpecifier = 1 << 0;
pub const CX_VOLATILE: CXTypeSpecifier = 1 << 1;
pub const CX_RESTRICT: CXTypeSpecifier = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeSpecifier = 1 << 3;
pub const CX_UNION: CXTypeSpecifier = 1 << 4;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXTemplateInput {
    pub params: Vec<CXType>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXType {
    pub kind: CXTypeKind,
    pub specifiers: CXTypeSpecifier,
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

    ExplicitSizedArray(Box<CXType>, usize),
    ImplicitSizedArray(Box<CXType>),

    MemoryReference {
        inner_type: Box<CXType>,
    },
    PointerTo {
        inner_type: Box<CXType>,
        weak: bool,
    },

    Structured {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>,
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>,
    },
    TaggedUnion {
        name: CXIdent,
        variants: Vec<(String, CXType)>,
    },

    FunctionPointer {
        prototype: Box<CXPrototype>,
    },
}

impl CXType {
    pub fn new(specifiers: CXTypeSpecifier, kind: CXTypeKind) -> Self {
        Self { kind, specifiers }
    }

    pub fn pointer_to(self, weak: bool, specifier: CXTypeSpecifier) -> Self {
        Self::new(
            specifier,
            CXTypeKind::PointerTo {
                inner_type: Box::new(self),
                weak,
            },
        )
    }

    pub fn add_specifier(mut self, specifier: CXTypeSpecifier) -> Self {
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
            CXTypeKind::Union { name, .. } |
            CXTypeKind::Structured { name, .. } => {
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
    pub fn transfer(&self, from_module: &str) -> ModuleResource<T> {
        if self.linkage == CXLinkageMode::Static {
            panic!("Static linkage resources must be declared private");
        }
        
        ModuleResource {
            visibility: VisibilityMode::Private,
            linkage: CXLinkageMode::Extern,
            external_module: Some(from_module.to_string()),
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
            CXFunctionKind::Destructor(name) => Some(name),
            CXFunctionKind::Standard(_) | CXFunctionKind::StaticMemberFunction { .. } => None,
        }
    }

    pub fn into_key(&self) -> CXFunctionKey {
        match self {
            CXFunctionKind::Standard(name) => CXFunctionKey::Standard(name.clone()),
            CXFunctionKind::MemberFunction { member_type, name } => {
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
            CXFunctionKind::Destructor(base_type) => {
                let type_base_name = base_type.base_name().clone();

                CXFunctionKey::Destructor {
                    type_base_name,
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