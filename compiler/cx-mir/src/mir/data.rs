use cx_ast::ast::{function::CXFunctionContract, modifiers::CXLinkageMode};
use cx_util::identifier::CXIdent;

use crate::mir::r#type::TypeComparisonState;
pub use crate::mir::r#type::{
    MIRFloatType, MIRIntegerType, MIRMoveAttributes, MIRType, MIRTypeContext, MIRTypeId,
    MIRTypeKind,
};

#[derive(Debug, Clone)]
pub struct MIRParameter {
    pub name: Option<CXIdent>,
    pub _type: MIRType,
}

impl MIRParameter {
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &MIRTypeContext,
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
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &MIRTypeContext,
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
    pub name: CXIdent,
    pub linkage: CXLinkageMode,
    pub signature: MIRFunctionSignature,
}

impl MIRFunctionPrototype {
    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn signature(&self) -> &MIRFunctionSignature {
        &self.signature
    }

    pub fn contextual_eq(&self, other: &Self, definitions: &MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &MIRTypeContext,
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
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &MIRTypeContext,
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
    pub base_name: CXIdent,
    pub template_input: MIRTemplateInput,
}

impl TemplateInfo {
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &MIRTypeContext,
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
