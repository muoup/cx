use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent};

use crate::ast::{
    expression::HIRExpression, function::HIRFunctionPrototype, modifiers::HIRTypeQualifiers,
    template::HIRTemplateInput,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRType {
    pub kind: HIRTypeKind,
    pub specifiers: HIRTypeQualifiers,
    pub range: TokenRange,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum HIRTagKind {
    Struct,
    Union,
    Enum,
}

impl HIRTagKind {
    pub fn prefix(self) -> &'static str {
        match self {
            Self::Struct => "struct",
            Self::Union => "union",
            Self::Enum => "enum",
        }
    }
}

#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq)]
pub enum HIRTypeLookup {
    #[default]
    Standard,
    Tag(HIRTagKind),
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct HIRAggregateAttributes {
    pub semantics: HIRMoveSemantics,
    pub copy_traits: Option<String>,
    pub unsafe_move: bool,
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub enum HIRMoveSemantics {
    #[default]
    POD,
    Nocopy,
    Nodrop,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HIRField {
    Standard {
        name: String,
        _type: HIRType,
    },
    Bitfield {
        name: Option<String>,
        integer_type: HIRType,
        width: usize,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HIRTypeKind {
    Identifier {
        name: QualifiedName,
        lookup: HIRTypeLookup,
        template_input: Option<HIRTemplateInput>,
    },

    ExplicitSizedArray(Box<HIRType>, Box<HIRExpression>),
    ImplicitSizedArray(Box<HIRType>),

    MemoryReference {
        inner_type: Box<HIRType>,
    },
    PointerTo {
        inner_type: Box<HIRType>,
    },

    Structured {
        name: Option<CXIdent>,
        attributes: HIRAggregateAttributes,
        fields: Vec<HIRField>,
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<HIRField>,
    },
    TaggedUnion {
        name: CXIdent,
        attributes: HIRAggregateAttributes,
        variants: Vec<HIRField>,
    },

    FunctionPointer {
        prototype: Box<HIRFunctionPrototype>,
    },
}

impl From<&str> for HIRType {
    fn from(value: &str) -> Self {
        HIRType::new(
            0,
            HIRTypeKind::Identifier {
                name: QualifiedName::new_raw(CXIdent::from(value)),
                lookup: HIRTypeLookup::Standard,
                template_input: None,
            },
        )
    }
}

impl HIRType {
    pub fn new(specifiers: HIRTypeQualifiers, kind: HIRTypeKind) -> Self {
        Self {
            kind,
            specifiers,
            range: TokenRange::Internal,
        }
    }

    pub fn tag_kind(&self) -> Option<HIRTagKind> {
        match &self.kind {
            HIRTypeKind::Identifier {
                lookup: HIRTypeLookup::Tag(tag),
                ..
            } => Some(*tag),
            _ => None,
        }
    }

    pub fn pointer_to(self, specifier: HIRTypeQualifiers) -> Self {
        let range = self.range.clone();
        let mut ty = Self::new(
            specifier,
            HIRTypeKind::PointerTo {
                inner_type: Box::new(self),
            },
        );
        ty.range = range;
        ty
    }

    pub fn add_specifier(mut self, specifier: HIRTypeQualifiers) -> Self {
        self.specifiers |= specifier;
        self
    }

    pub fn with_range(mut self, range: TokenRange) -> Self {
        self.range = range;
        self
    }

    pub fn range(&self) -> &TokenRange {
        &self.range
    }

    pub fn get_name(&self) -> Option<&CXIdent> {
        match &self.kind {
            HIRTypeKind::Identifier { name, .. } => Some(&name.name),
            HIRTypeKind::TaggedUnion { name, .. } => Some(name),

            HIRTypeKind::Structured { name, .. } => name.as_ref(),
            HIRTypeKind::Union { name, .. } => name.as_ref(),

            _ => None,
        }
    }

    pub fn set_name(&mut self, to: CXIdent) {
        match &mut self.kind {
            HIRTypeKind::Union { name, .. } | HIRTypeKind::Structured { name, .. } => {
                *name = Some(to);
            }

            HIRTypeKind::TaggedUnion { name, .. } => {
                *name = to;
            }

            _ => {}
        }
    }
}

impl HIRTypeKind {
    pub fn to_type(self) -> HIRType {
        HIRType::new(0, self)
    }
}

impl HIRField {
    pub fn standard(name: String, _type: HIRType) -> Self {
        Self::Standard { name, _type }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            HIRField::Standard { name, .. } => Some(name.as_str()),
            HIRField::Bitfield { name, .. } => name.as_deref(),
        }
    }

    pub fn standard_parts(&self) -> Option<(&String, &HIRType)> {
        match self {
            HIRField::Standard { name, _type } => Some((name, _type)),
            HIRField::Bitfield { .. } => None,
        }
    }
}
