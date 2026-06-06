use cx_ast::ast::{function::CXFunctionContract, modifiers::CXLinkageMode};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::mir::expression::MIRExpression;
use crate::mir::r#type::TypeComparisonState;
pub use crate::mir::r#type::{
    MIRFloatType, MIRIntegerType, MIRMoveAttributes, MIRType, MIRTypeId, MIRTypeKind,
};
use crate::type_context::MIRTypeContext;

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: MIRExpression,
}

#[derive(Debug, Clone)]
pub struct MIRParameter {
    pub name: Option<CXIdent>,
    pub _type: MIRType,
}

impl MIRParameter {
    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.name == other.name
            && self
                ._type
                .contextual_eq_with_state(&other._type, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct MIRFunctionSignature {
    pub return_type: MIRType,
    pub params: Vec<MIRParameter>,
    pub var_args: bool,
    pub contract: CXFunctionContract,
}

impl Default for MIRFunctionSignature {
    fn default() -> Self {
        Self {
            return_type: MIRTypeKind::Unit.into(),
            params: Vec::new(),
            var_args: false,
            contract: CXFunctionContract::default(),
        }
    }
}

impl MIRFunctionSignature {
    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.var_args == other.var_args
            && self
                .return_type
                .contextual_eq_with_state(&other.return_type, definitions, state)
            && self.params.len() == other.params.len()
            && self
                .params
                .iter()
                .zip(other.params.iter())
                .all(|(left, right)| left._type.contextual_eq(&right._type, definitions))
    }
}

#[derive(Debug, Clone)]
pub struct MIRFunctionPrototype {
    name: CXIdent,
    mangled_name: Option<String>,
    linkage: CXLinkageMode,
    signature: MIRFunctionSignature,
}

impl MIRFunctionPrototype {
    pub fn new(name: CXIdent, linkage: CXLinkageMode, signature: MIRFunctionSignature) -> Self {
        Self {
            name,
            mangled_name: None,
            linkage,
            signature,
        }
    }

    pub fn name(&self) -> &str {
        self.mangled_name.as_ref()
            .map(String::as_str)
            .unwrap_or(self.name.as_str())
    }

    pub fn base_name(&self) -> &CXIdent {
        &self.name
    }

    pub fn signature(&self) -> &MIRFunctionSignature {
        &self.signature
    }

    pub fn linkage(&self) -> CXLinkageMode {
        self.linkage
    }

    pub fn with_mangled_name<F>(mut self, f: F) -> Self
        where F: FnOnce(&str) -> String {
        self.mangle_name(f);
        self
    }

    pub fn mangle_name<F>(&mut self, f: F)
        where F: FnOnce(&str) -> String {

        if let Some(mangled) = &self.mangled_name {
            self.mangled_name = Some(f(mangled));
        } else {
            self.mangled_name = Some(f(self.name.as_str()));
        }
    }

    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.name == other.name
            && self
                .signature
                .contextual_eq_with_state(&other.signature, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct MIRTemplateInput {
    pub args: Vec<MIRType>,
}

impl MIRTemplateInput {
    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.args.len() == other.args.len()
            && self
                .args
                .iter()
                .zip(other.args.iter())
                .all(|(left, right)| left.contextual_eq_with_state(right, definitions, state))
    }
}

#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub base_name: Option<QualifiedName>,
    pub template_input: MIRTemplateInput,
}

impl TemplateInfo {
    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.base_name == other.base_name
            && self.template_input.contextual_eq_with_state(
                &other.template_input,
                definitions,
                state,
            )
    }
}
