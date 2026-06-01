use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, modifiers::CXTypeQualifiers,
    template::CXTemplateInput,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXType {
    pub kind: CXTypeKind,
    pub specifiers: CXTypeQualifiers,
}

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq)]
pub enum PredeclarationType {
    #[default]
    None,
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct CXStructAttributes {
    pub nocopy: bool,
    pub nodrop: bool,
    pub copy_traits: Option<String>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CXTypeKind {
    Identifier {
        name: QualifiedName,
        predeclaration: PredeclarationType,
        template_input: Option<CXTemplateInput>,
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
            CXTypeKind::Identifier { name, .. } => Some(&name.name),
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
