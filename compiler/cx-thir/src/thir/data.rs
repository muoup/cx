use cx_hir::ast::function::HIRFunctionContract;
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::thir::contextual_eq::{TypeComparisonState, TypeContextEqual, compare_ordered};
use crate::thir::expression::{THIRExpression, THIRLocalID};
pub use crate::thir::r#type::{
    THIRFloatType, THIRIntType, THIRType, THIRTypeAttributes, THIRTypeID, THIRTypeKind,
};
use crate::type_context::THIRTypeContext;

#[derive(Debug, Clone)]
pub struct THIRFunction {
    prototype: THIRFnPrototype,
    body: Option<THIRFunctionBody>,

    reject_nonvoid_fallthrough: bool,
}

impl THIRFunction {
    pub fn new(
        prototype: THIRFnPrototype,
        body: Option<THIRFunctionBody>,
        reject_nonvoid_fallthrough: bool,
    ) -> Self {
        Self {
            prototype,
            body,
            reject_nonvoid_fallthrough,
        }
    }

    pub fn prototype(&self) -> &THIRFnPrototype {
        &self.prototype
    }

    pub fn body(&self) -> Option<&THIRFunctionBody> {
        self.body.as_ref()
    }

    pub fn reject_nonvoid_fallthrough(&self) -> bool {
        self.reject_nonvoid_fallthrough
    }

    /// Replaces this function's prototype and body with those of `definition`, keeping this
    /// function's fallthrough policy.
    pub fn take_definition(&mut self, definition: THIRFunction) {
        self.prototype = definition.prototype;
        self.body = definition.body;
    }
}

#[derive(Debug, Clone)]
pub enum THIRFunctionBody {
    Expression(THIRExpression),
    Block {
        exprs: Vec<THIRExpression>,
        token_range: TokenRange,
    }
}

impl THIRFunctionBody {
    pub fn exprs(&self) -> &[THIRExpression] {
        match self {
            THIRFunctionBody::Expression(expr) => std::slice::from_ref(expr),
            THIRFunctionBody::Block { exprs, .. } => exprs,
        }
    }
}

#[derive(Debug, Clone)]
pub struct THIRParameter {
    name: Option<CXIdent>,
    local_id: THIRLocalID,
    ty: THIRType,
}

impl THIRParameter {
    pub fn new(name: Option<CXIdent>, local_id: THIRLocalID, ty: THIRType) -> Self {
        Self { name, local_id, ty }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn local_id(&self) -> THIRLocalID {
        self.local_id
    }

    pub fn ty(&self) -> &THIRType {
        &self.ty
    }
}

#[derive(Debug, Clone)]
pub struct THIRComptimeFnPrototype {
    symbol_name: String,
    debug_name: Option<CXIdent>,
    lookup_identifier: QualifiedName,

    return_type: THIRComptimeValueType,
    params: Vec<THIRComptimeParameter>,
}

#[derive(Debug, Clone)]
pub struct THIRComptimeParameter {
    name: Option<CXIdent>,
    local_id: THIRLocalID,
    value_type: THIRComptimeValueType,
}

impl THIRComptimeParameter {
    pub fn new(
        name: Option<CXIdent>,
        local_id: THIRLocalID,
        value_type: THIRComptimeValueType,
    ) -> Self {
        Self {
            name,
            local_id,
            value_type,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn local_id(&self) -> THIRLocalID {
        self.local_id
    }

    pub fn value_type(&self) -> &THIRComptimeValueType {
        &self.value_type
    }
}

#[derive(Debug, Clone)]
pub struct THIRComptimeValueType {
    expr: bool,
    params: Vec<THIRType>,
    ty: THIRType,
}

impl THIRComptimeValueType {
    pub fn new(expr: bool, params: Vec<THIRType>, ty: THIRType) -> Self {
        Self { expr, params, ty }
    }

    pub fn is_expr(&self) -> bool {
        self.expr
    }

    pub fn params(&self) -> &[THIRType] {
        &self.params
    }

    pub fn ty(&self) -> &THIRType {
        &self.ty
    }
}

impl THIRComptimeFnPrototype {
    pub fn new(
        symbol_name: impl Into<String>,
        lookup_identifier: QualifiedName,
        return_type: THIRComptimeValueType,
        params: Vec<THIRComptimeParameter>,
    ) -> Self {
        Self {
            symbol_name: symbol_name.into(),
            lookup_identifier,
            return_type,
            params,
            debug_name: None,
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

    pub fn lookup_identifier(&self) -> &QualifiedName {
        &self.lookup_identifier
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
        self.name == other.name && self.ty.compare(&other.ty, definitions, state)
    }
}

#[derive(Debug, Clone)]
pub struct THIRFnSignature {
    return_type: THIRType,
    params: Vec<THIRParameter>,
    var_args: bool,
    contract: HIRFunctionContract,
}

impl THIRFnSignature {
    pub fn new(
        return_type: THIRType,
        params: Vec<THIRParameter>,
        var_args: bool,
        contract: HIRFunctionContract,
    ) -> Self {
        Self {
            return_type,
            params,
            var_args,
            contract,
        }
    }

    pub fn return_type(&self) -> &THIRType {
        &self.return_type
    }

    pub fn params(&self) -> &[THIRParameter] {
        &self.params
    }

    pub fn var_args(&self) -> bool {
        self.var_args
    }

    pub fn contract(&self) -> &HIRFunctionContract {
        &self.contract
    }
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
                .all(|(left, right)| left.ty.compare(&right.ty, definitions, state))
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
    args: Vec<THIRTypeID>,
}

impl THIRTemplateInput {
    pub fn new(args: Vec<THIRTypeID>) -> Self {
        Self { args }
    }

    pub fn args(&self) -> &[THIRTypeID] {
        &self.args
    }
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
    base_name: Option<QualifiedName>,
    template_input: THIRTemplateInput,
}

impl TemplateInfo {
    pub fn new(base_name: Option<QualifiedName>, template_input: THIRTemplateInput) -> Self {
        Self {
            base_name,
            template_input,
        }
    }

    pub fn base_name(&self) -> Option<&QualifiedName> {
        self.base_name.as_ref()
    }

    pub fn template_input(&self) -> &THIRTemplateInput {
        &self.template_input
    }
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
