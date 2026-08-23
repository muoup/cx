use cx_hir::ast::function::HIRFunctionContract;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, namespace::QualifiedName};

use crate::thir::contextual_eq::{TypeComparisonState, TypeContextEqual, compare_ordered};
use crate::thir::expression::{THIRExpression, THIRLocalID};
pub use crate::thir::r#type::{
    THIRFloatType, THIRIntType, THIRType, THIRTypeAttributes, THIRTypeID, THIRTypeKind,
};
use crate::type_context::THIRTypeContext;

#[derive(Debug, Clone)]
pub struct THIRFunction {
    pub prototype: THIRFnPrototype,
    pub body: Option<THIRExpression>,
}

#[derive(Debug, Clone)]
pub struct THIRParameter {
    pub name: Option<CXIdent>,
    pub local_id: THIRLocalID,
    pub _type: THIRType,
}

#[derive(Debug, Clone)]
pub struct THIRComptimeFunction {
    pub name: Option<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct THIRComptimeFnPrototype {
    symbol_name: String,
    lookup_identifier: Option<QualifiedName>,
    debug_name: Option<CXIdent>,
    return_type: THIRComptimeValueType,
    params: Vec<THIRComptimeParameter>,
    runtime_return_type: Option<THIRType>,
}

#[derive(Debug, Clone)]
pub struct THIRComptimeParameter {
    pub name: Option<CXIdent>,
    pub local_id: THIRLocalID,
    pub value_type: THIRComptimeValueType,
}

#[derive(Debug, Clone)]
pub struct THIRComptimeValueType {
    pub expr: bool,
    pub params: Vec<THIRType>,
    pub _type: THIRType,
}

impl THIRComptimeFnPrototype {
    pub fn new(
        symbol_name: impl Into<String>,
        return_type: THIRComptimeValueType,
        params: Vec<THIRComptimeParameter>,
    ) -> Self {
        Self {
            symbol_name: symbol_name.into(),
            lookup_identifier: None,
            debug_name: None,
            return_type,
            params,
            runtime_return_type: None,
        }
    }

    pub fn symbol_name(&self) -> &str {
        self.symbol_name.as_str()
    }

    pub fn pretty_name(&self) -> &str {
        if let Some(debug_name) = &self.debug_name {
            debug_name.as_str()
        } else {
            self.symbol_name.as_str()
        }
    }

    pub fn lookup_identifier(&self) -> Option<&QualifiedName> {
        self.lookup_identifier.as_ref()
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn return_type(&self) -> &THIRComptimeValueType {
        &self.return_type
    }

    pub fn params(&self) -> &[THIRComptimeParameter] {
        &self.params
    }

    pub fn runtime_return_type(&self) -> Option<&THIRType> {
        self.runtime_return_type.as_ref()
    }

    pub fn with_runtime_return_type(mut self, ty: Option<THIRType>) -> Self {
        self.runtime_return_type = ty;
        self
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
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRParameter {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.name == other.name && self._type.compare(&other._type, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct THIRFnSignature {
    pub return_type: THIRType,
    pub params: Vec<THIRParameter>,
    pub var_args: bool,
    pub contract: HIRFunctionContract,
}

impl Default for THIRFnSignature {
    fn default() -> Self {
        Self {
            return_type: THIRTypeKind::Void.into(),
            params: Vec::new(),
            var_args: false,
            contract: HIRFunctionContract::default(),
        }
    }
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRFnSignature {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.var_args == other.var_args
            && self
                .return_type
                .compare(&other.return_type, definitions, state)
            && self.params.len() == other.params.len()
            && self
                .params
                .iter()
                .zip(other.params.iter())
                .all(|(left, right)| left._type.compare(&right._type, definitions, state))
    }
}

#[derive(Debug, Clone)]
pub struct THIRFnPrototype {
    symbol_name: String,
    lookup_identifier: Option<QualifiedName>,
    debug_name: Option<CXIdent>,
    linkage: LinkageMode,
    signature: THIRFnSignature,
}

impl THIRFnPrototype {
    pub fn new(
        symbol_name: impl Into<String>,
        linkage: LinkageMode,
        signature: THIRFnSignature,
    ) -> Self {
        Self {
            symbol_name: symbol_name.into(),
            lookup_identifier: None,
            debug_name: None,
            linkage,
            signature,
        }
    }

    pub fn symbol_name(&self) -> &str {
        self.symbol_name.as_str()
    }

    pub fn lookup_identifier(&self) -> Option<&QualifiedName> {
        self.lookup_identifier.as_ref()
    }

    pub fn pretty_name(&self) -> &str {
        if let Some(debug_name) = &self.debug_name {
            debug_name.as_str()
        } else {
            self.symbol_name.as_str()
        }
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn signature(&self) -> &THIRFnSignature {
        &self.signature
    }

    pub fn linkage(&self) -> LinkageMode {
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
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRFnPrototype {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.symbol_name == other.symbol_name
            && self.signature.compare(&other.signature, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct THIRTemplateInput {
    pub args: Vec<THIRTypeID>,
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRTemplateInput {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        compare_ordered(&self.args, &other.args, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub base_name: Option<QualifiedName>,
    pub template_input: THIRTemplateInput,
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for TemplateInfo {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        self.base_name == other.base_name
            && self
                .template_input
                .compare(&other.template_input, definitions, state)
    }
}
