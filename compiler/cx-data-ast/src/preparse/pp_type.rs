use std::hash::Hash;
use speedy::{Readable, Writable};
use uuid::Uuid;
use crate::parse::identifier::CXIdent;
use crate::parse::parser::VisibilityMode;

pub type CXTypeSpecifier = u8;

pub const CX_CONST: CXTypeSpecifier = 1 << 0;
pub const CX_VOLATILE: CXTypeSpecifier = 1 << 1;
pub const CX_RESTRICT: CXTypeSpecifier = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeSpecifier = 1 << 3;
pub const CX_UNION: CXTypeSpecifier = 1 << 4;

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct ModuleResource<Resource> {
    pub visibility: VisibilityMode,
    pub external_module: Option<String>,
    
    pub resource: Resource
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveType {
    pub uuid: u64,
    pub kind: CXNaiveTypeKind,
    pub specifiers: CXTypeSpecifier,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Readable, Writable)]
pub enum PredeclarationType {
    None,
    Struct,
    Union,
    Enum
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXNaivePrototype {
    pub name: CXIdent,
    pub params: Vec<CXNaiveParameter>,
    pub return_type: CXNaiveType,
    pub var_args: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXFunctionTemplate {
    pub name: CXIdent,
    pub inputs: Vec<String>,
    pub shell: CXNaivePrototype,
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXTypeTemplate {
    pub name: CXIdent,
    pub inputs: Vec<String>,
    pub shell: CXNaiveType
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveParameter {
    pub name: Option<CXIdent>,
    pub _type: CXNaiveType,
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveTemplateInput {
    pub params: Vec<CXNaiveType>
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXNaiveTypeKind {
    Identifier {
        name: CXIdent,
        predeclaration: PredeclarationType
    },
    TemplatedIdentifier {
        name: CXIdent,
        input: CXNaiveTemplateInput
    },

    ExplicitSizedArray(Box<CXNaiveType>, usize),
    ImplicitSizedArray(Box<CXNaiveType>),

    PointerTo {
        inner_type: Box<CXNaiveType>,
        weak: bool,
    },
    StrongPointer {
        inner: Box<CXNaiveType>,
        is_array: bool,
    },

    Structured {
        name: Option<CXIdent>,
        fields: Vec<(String, CXNaiveType)>,
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<(String, CXNaiveType)>,
    },

    FunctionPointer {
        prototype: Box<CXNaivePrototype>
    },
}

impl CXNaiveType {
    pub fn new(specifiers: CXTypeSpecifier, kind: CXNaiveTypeKind) -> Self {
        Self {
            uuid: Uuid::new_v4().as_u64_pair().0,
            kind,
            specifiers,
        }
    }

    pub fn pointer_to(self, weak: bool, specifier: CXTypeSpecifier) -> Self {
        Self::new(
            specifier,
          CXNaiveTypeKind::PointerTo {
                inner_type: Box::new(self),
                weak,
            }
        )
    }

    pub fn add_specifier(mut self, specifier: CXTypeSpecifier) -> Self {
        self.specifiers |= specifier;
        self
    }
}

impl CXNaiveTypeKind {
    pub fn to_type(self) -> CXNaiveType {
        CXNaiveType::new(0, self)
    }
}

impl<T: Clone> ModuleResource<T> {
    pub fn transfer(&self, from_module: &str) -> ModuleResource<T> {
        ModuleResource {
            visibility: VisibilityMode::Private,
            external_module: Some(from_module.to_string()),
            resource: self.resource.clone(),
        }
    }
    
    pub fn standard(resource: T) -> ModuleResource<T> {
        ModuleResource {
            visibility: VisibilityMode::Private,
            external_module: None,
            resource,
        }
    }
    
    pub fn with_visibility(resource: T, visibility: VisibilityMode) -> ModuleResource<T> {
        ModuleResource {
            visibility,
            external_module: None,
            resource,
        }
    }
}

impl Hash for CXNaiveType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.uuid.hash(state);
    }
}