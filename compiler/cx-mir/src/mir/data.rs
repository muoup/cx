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
    symbol_name: String,
    lookup_identifier: Option<QualifiedName>,
    debug_name: Option<CXIdent>,
    linkage: CXLinkageMode,
    signature: MIRFunctionSignature,
}

impl MIRFunctionPrototype {
    pub fn new(
        symbol_name: impl Into<String>,
        linkage: CXLinkageMode,
        signature: MIRFunctionSignature,
    ) -> Self {
        Self {
            symbol_name: symbol_name.into(),
            lookup_identifier: None,
            debug_name: None,
            linkage,
            signature,
        }
    }

    pub fn name(&self) -> &str {
        self.symbol_name()
    }

    pub fn symbol_name(&self) -> &str {
        self.symbol_name.as_str()
    }

    pub fn lookup_identifier(&self) -> Option<&QualifiedName> {
        self.lookup_identifier.as_ref()
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn signature(&self) -> &MIRFunctionSignature {
        &self.signature
    }

    pub fn linkage(&self) -> CXLinkageMode {
        self.linkage
    }

    pub fn with_lookup_identifier(mut self, lookup_identifier: QualifiedName) -> Self {
        self.lookup_identifier = Some(lookup_identifier);
        self
    }

    pub fn with_debug_name(mut self, debug_name: CXIdent) -> Self {
        self.debug_name = Some(debug_name);
        self
    }

    pub fn map_symbol_name<F>(&mut self, f: F)
    where
        F: FnOnce(&str) -> String,
    {
        self.symbol_name = f(self.symbol_name.as_str());
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
        self.symbol_name == other.symbol_name
            && self
                .signature
                .contextual_eq_with_state(&other.signature, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct MIRTemplateInput {
    pub args: Vec<MIRTypeId>,
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
