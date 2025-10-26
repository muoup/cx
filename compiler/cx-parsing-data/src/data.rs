use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::parser::VisibilityMode;

pub type NaiveTypeIdent = String;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum FunctionTypeIdent {
    Standard(CXIdent),
    Templated(CXIdent, CXNaiveTemplateInput),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum NaiveFnKind {
    Standard(CXIdent),
    MemberFunction {
        _type: FunctionTypeIdent,
        function_name: CXIdent,
    },
    Destructor(FunctionTypeIdent),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum NaiveFnIdent {
    Standard(CXIdent),
    MemberFunction {
        type_identifier: CXIdent,
        function_name: CXIdent,
    },
    Destructor(CXIdent),
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

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveType {
    pub kind: CXNaiveTypeKind,
    pub specifiers: CXTypeSpecifier,
}

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq, Readable, Writable)]
pub enum PredeclarationType {
    #[default]
    None,
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXNaivePrototype {
    pub name: NaiveFnKind,
    pub params: Vec<CXNaiveParameter>,
    pub return_type: CXNaiveType,
    pub var_args: bool,
    pub this_param: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveParameter {
    pub name: Option<CXIdent>,
    pub _type: CXNaiveType,
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

pub type CXTypeTemplate = CXTemplate<CXNaiveType>;
pub type CXFunctionTemplate = CXTemplate<CXNaivePrototype>;
pub type CXDestructorTemplate = CXTemplate<String>;

pub type CXTypeSpecifier = u8;

pub const CX_CONST: CXTypeSpecifier = 1 << 0;
pub const CX_VOLATILE: CXTypeSpecifier = 1 << 1;
pub const CX_RESTRICT: CXTypeSpecifier = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeSpecifier = 1 << 3;
pub const CX_UNION: CXTypeSpecifier = 1 << 4;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXNaiveTemplateInput {
    pub params: Vec<CXNaiveType>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum CXNaiveTypeKind {
    Identifier {
        name: CXIdent,
        predeclaration: PredeclarationType,
    },
    TemplatedIdentifier {
        name: CXIdent,
        input: CXNaiveTemplateInput,
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
    TaggedUnion {
        name: CXIdent,
        variants: Vec<(String, CXNaiveType)>,
    },

    FunctionPointer {
        prototype: Box<CXNaivePrototype>,
    },
}

impl CXNaiveType {
    pub fn new(specifiers: CXTypeSpecifier, kind: CXNaiveTypeKind) -> Self {
        Self { kind, specifiers }
    }

    pub fn pointer_to(self, weak: bool, specifier: CXTypeSpecifier) -> Self {
        Self::new(
            specifier,
            CXNaiveTypeKind::PointerTo {
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
            CXNaiveTypeKind::Identifier { name, .. } => Some(name),
            CXNaiveTypeKind::TemplatedIdentifier { name, .. } => Some(name),
            CXNaiveTypeKind::TaggedUnion { name, .. } => Some(name),

            CXNaiveTypeKind::Structured { name, .. } => name.as_ref(),
            CXNaiveTypeKind::Union { name, .. } => name.as_ref(),

            _ => None,
        }
    }
}

impl CXNaiveTypeKind {
    pub fn to_type(self) -> CXNaiveType {
        CXNaiveType::new(0, self)
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

impl FunctionTypeIdent {
    pub fn as_type(&self) -> CXNaiveType {
        match self {
            FunctionTypeIdent::Standard(name) => CXNaiveTypeKind::Identifier {
                name: name.clone(),
                predeclaration: PredeclarationType::None,
            }
            .to_type(),
            FunctionTypeIdent::Templated(name, input) => CXNaiveTypeKind::TemplatedIdentifier {
                name: name.clone(),
                input: input.clone(),
            }
            .to_type(),
        }
    }

    pub fn from_type(ty: &CXNaiveType) -> Option<FunctionTypeIdent> {
        match &ty.kind {
            CXNaiveTypeKind::Identifier { name, .. } => {
                Some(FunctionTypeIdent::Standard(name.clone()))
            }
            CXNaiveTypeKind::TemplatedIdentifier { name, input } => {
                Some(FunctionTypeIdent::Templated(name.clone(), input.clone()))
            }
            _ => None,
        }
    }
}

impl NaiveFnKind {
    pub fn implicit_member(&self) -> Option<&FunctionTypeIdent> {
        match self {
            NaiveFnKind::MemberFunction { _type, .. } => Some(_type),
            NaiveFnKind::Destructor(name) => Some(name),
            NaiveFnKind::Standard(_) => None,
        }
    }
}

impl From<&NaiveFnKind> for NaiveFnIdent {
    fn from(kind: &NaiveFnKind) -> Self {
        match kind {
            NaiveFnKind::Standard(name) => NaiveFnIdent::Standard(name.clone()),
            NaiveFnKind::MemberFunction {
                _type,
                function_name,
            } => NaiveFnIdent::MemberFunction {
                type_identifier: match _type {
                    FunctionTypeIdent::Standard(n) => n,
                    FunctionTypeIdent::Templated(n, _) => n,
                }
                .clone(),
                function_name: function_name.clone(),
            },
            NaiveFnKind::Destructor(name) => NaiveFnIdent::Destructor(
                match name {
                    FunctionTypeIdent::Standard(n) => n,
                    FunctionTypeIdent::Templated(n, _) => n,
                }
                .clone(),
            ),
        }
    }
}
