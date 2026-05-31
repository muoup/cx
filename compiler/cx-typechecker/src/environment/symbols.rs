use std::collections::{HashMap, HashSet};

use cx_ast::{
    ast::modifiers::{CX_CONST, CXTypeQualifiers},
    registry::GlobalSymbolRegistry,
};
use cx_mir::intrinsic_types::INTRINSIC_TYPES;
use cx_mir::mir::data::{
    MIRFunctionPrototype, MIRSymbolRegistry, MIRType, MIRTypeId, MIRTypeKind, TemplateInfo,
};
use cx_mir::mir::expression::{MIRExpression, MIRPureExpression};
use cx_util::scoped_map::ScopedMap;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub(crate) mod completion;
pub(crate) mod templates;
