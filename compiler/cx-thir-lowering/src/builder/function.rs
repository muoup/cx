use std::collections::HashMap;

use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRComptimeFnPrototype, MIRComptimeOperand, MIRComptimeRegisterID,
    MIRComptimeType, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRInstruction,
    MIRInstructionKind, MIRIntrinsic, MIRPlaceID, MIRRegister, MIRScopeID, MIRTypeID, MIRValue,
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::builder::body::{MIRBodyBuilder, MIRBodyKind};

#[derive(Debug)]
pub(crate) struct MIRFunctionBuilder<'thir> {
    id: MIRFunctionID,

    body: MIRBodyBuilder<'thir>,
    current_block: MIRBasicBlockID,

    local_values: HashMap<THIRLocalID, MIRValue>,
    comptime_values: HashMap<THIRLocalID, MIRComptimeOperand>,
    labels: HashMap<String, MIRBasicBlockID>,

    scope_stack: Vec<ScopeContext<'thir>>,
    control_stack: Vec<ControlContext>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext<'thir> {
    id: MIRScopeID,

    named_values: HashMap<String, MIRValue>,

    deferred_expressions: Vec<DeferredExpression<'thir>>,
}

#[derive(Debug, Clone)]
pub(crate) struct DeferredExpression<'thir> {
    pub expression: &'thir THIRExpression,
    pub locals: HashMap<THIRLocalID, MIRValue>,
    pub comptime: HashMap<THIRLocalID, MIRComptimeOperand>,
}

#[derive(Debug)]
pub(crate) struct ControlContext {
    cleanup_boundary: MIRScopeID,
    yield_target: Option<MIRBasicBlockID>,
    break_target: Option<MIRBasicBlockID>,
    continue_target: Option<MIRBasicBlockID>,
}

impl<'thir> ScopeContext<'thir> {
    pub fn new(id: MIRScopeID) -> Self {
        Self {
            id,
            named_values: HashMap::new(),
            deferred_expressions: Vec::new(),
        }
    }

    pub fn deferred_expressions(&self) -> &[DeferredExpression<'thir>] {
        &self.deferred_expressions
    }

    pub(crate) fn id(&self) -> MIRScopeID {
        self.id
    }
}

impl ControlContext {
    fn new(cleanup_boundary: MIRScopeID) -> Self {
        Self {
            cleanup_boundary,
            yield_target: None,
            break_target: None,
            continue_target: None,
        }
    }

    pub fn cleanup_boundary(&self) -> MIRScopeID {
        self.cleanup_boundary
    }

    pub fn set_yield_target(&mut self, target: MIRBasicBlockID) {
        self.yield_target = Some(target);
    }

    pub fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.yield_target
    }

    pub fn set_break_target(&mut self, target: MIRBasicBlockID) -> &mut Self {
        self.break_target = Some(target);
        self
    }

    pub fn set_continue_target(&mut self, target: MIRBasicBlockID) -> &mut Self {
        self.continue_target = Some(target);
        self
    }

    pub fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.break_target
    }

    pub fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.continue_target
    }
}

impl<'thir> MIRFunctionBuilder<'thir> {
    pub(crate) fn new_runtime(id: MIRFunctionID, func: MIRFunction) -> Self {
        Self::new(
            id,
            MIRBodyBuilder::new_runtime(func.prototype().clone(), MIRBody::new()),
        )
    }

    pub(crate) fn new_comptime(id: MIRFunctionID, prototype: MIRComptimeFnPrototype) -> Self {
        Self::new(
            id,
            MIRBodyBuilder::new_comptime(prototype, cx_mir::MIRComptimeBody::new()),
        )
    }

    pub(crate) fn new_comptime_scratch(id: MIRFunctionID) -> Self {
        Self::new(
            id,
            MIRBodyBuilder::new_comptime_scratch(cx_mir::MIRComptimeBody::new()),
        )
    }

    fn new(id: MIRFunctionID, mut body: MIRBodyBuilder<'thir>) -> Self {
        let entry = body.add_block(None);
        let root_scope = body.add_scope(TokenRange::internal());
        Self {
            id,
            body,
            current_block: entry,
            local_values: HashMap::new(),
            comptime_values: HashMap::new(),
            labels: HashMap::new(),
            scope_stack: vec![ScopeContext::new(root_scope)],
            control_stack: Vec::new(),
        }
    }

    pub(crate) fn thin_finish(self) -> (MIRFunctionID, MIRBodyKind<'thir>) {
        (self.id, self.body.finish())
    }

    #[allow(dead_code)]
    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        self.body
            .runtime_prototype()
            .expect("runtime prototype required")
    }

    pub fn body(&self) -> &MIRBodyBuilder<'thir> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut MIRBodyBuilder<'thir> {
        &mut self.body
    }

    #[allow(dead_code)]
    pub fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        assert!(
            self.body.has_block(block),
            "selected block does not belong to the active function"
        );
        self.current_block = block;
        self.body.set_current_block(block);
    }

    pub fn current_block_terminated(&self) -> bool {
        self.body.current_block_terminated()
    }

    pub fn set_yield_recipient(&mut self, target: MIRBasicBlockID, ty: MIRTypeID) -> MIRRegister {
        self.block_param(target, ty, Some(CXIdent::new("yield_result")))
    }

    pub fn label(&mut self, name: &CXIdent) -> Option<MIRBasicBlockID> {
        self.labels.get(name.as_str()).copied()
    }

    pub fn declare_label(&mut self, name: &CXIdent, id: MIRBasicBlockID) {
        self.labels.insert(name.to_string(), id);
    }

    pub fn new_register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.body.add_register(ty, debug_name)
    }

    pub fn new_comptime_register(
        &mut self,
        ty: MIRComptimeType,
        debug_name: Option<CXIdent>,
    ) -> MIRComptimeRegisterID {
        self.body.add_comptime_register(ty, debug_name)
    }

    pub fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.body.register(register).map(|decl| decl.ty)
    }

    pub fn emit(&mut self, instr: MIRInstruction) {
        if !self.current_block_terminated() {
            self.body.emit(instr);
        }
    }

    pub fn emit_intrinsic(&mut self, intrinsic: impl Into<MIRIntrinsic>, range: TokenRange) {
        self.emit(MIRInstruction {
            kind: MIRInstructionKind::IntrinsicOp(intrinsic.into()),
            token_range: range,
        })
    }

    pub fn emit_comptime(&mut self, op: cx_mir::MIRComptimeOp<'thir>, range: TokenRange) {
        if !self.current_block_terminated() {
            self.body.emit_comptime(op, range);
        }
    }

    pub fn new_block(&mut self, name: impl Into<CXIdent>) -> MIRBasicBlockID {
        self.body.add_block(Some(name.into()))
    }

    pub fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.body.add_block_param(block, ty, debug_name)
    }

    pub fn new_place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlaceID {
        let scope = self
            .scope_stack
            .last()
            .expect("active function has no lexical scope")
            .id;

        self.body.add_place(ty, debug_name, nodrop, scope)
    }

    pub fn places_in_scope(&self, scope: MIRScopeID) -> Vec<MIRPlaceID> {
        self.body.places_in_scope(scope)
    }

    pub fn local(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.local_values.get(&local).cloned()
    }

    pub fn comptime_local(&self, local: THIRLocalID) -> Option<MIRComptimeOperand> {
        self.comptime_values.get(&local).cloned()
    }

    pub fn locals(&self) -> HashMap<THIRLocalID, MIRValue> {
        self.local_values.clone()
    }

    pub fn bind_local(&mut self, local: THIRLocalID, value: MIRValue) {
        self.local_values.insert(local, value);
    }

    pub fn bind_comptime_local(&mut self, local: THIRLocalID, value: MIRComptimeOperand) {
        self.comptime_values.insert(local, value);
    }

    pub fn add_deferred_expression(&mut self, expression: &'thir THIRExpression) {
        let defer = DeferredExpression {
            expression,
            locals: self.local_values.clone(),
            comptime: self.comptime_values.clone(),
        };
        self.current_scope_mut().deferred_expressions.push(defer);
    }

    pub fn replace_local_bindings(
        &mut self,
        locals: HashMap<THIRLocalID, MIRValue>,
        comptime: HashMap<THIRLocalID, MIRComptimeOperand>,
    ) -> (
        HashMap<THIRLocalID, MIRValue>,
        HashMap<THIRLocalID, MIRComptimeOperand>,
    ) {
        (
            std::mem::replace(&mut self.local_values, locals),
            std::mem::replace(&mut self.comptime_values, comptime),
        )
    }

    pub fn comptime_locals(&self) -> HashMap<THIRLocalID, MIRComptimeOperand> {
        self.comptime_values.clone()
    }

    pub fn bind_named_value(&mut self, name: &CXIdent, value: MIRValue) {
        self.scope_stack
            .last_mut()
            .expect("active function has no lexical scope")
            .named_values
            .insert(name.as_string(), value);
    }

    pub fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.scope_stack
            .iter()
            .rev()
            .find_map(|context| context.named_values.get(name.as_str()).cloned())
    }

    pub fn current_scope_range(&self) -> TokenRange {
        self.body()
            .scope(self.current_scope_id())
            .expect("active function has no lexical scope")
            .token_range
            .clone()
    }

    pub fn current_scope_id(&self) -> MIRScopeID {
        self.scope_stack
            .last()
            .expect("active function has no lexical scope")
            .id
    }

    pub fn push_control_scope(&mut self) {
        self.control_stack
            .push(ControlContext::new(self.current_scope_id()));
    }

    pub fn pop_control_scope(&mut self) {
        self.control_stack
            .pop()
            .expect("control scope stack is empty");
    }

    pub fn push_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let scope = self.body.add_scope(token_range.clone());
        self.scope_stack.push(ScopeContext::new(scope));
        scope
    }

    #[must_use]
    pub fn pop_scope(&mut self) -> MIRScopeID {
        let scope = self.scope_stack.pop().expect("scope stack is empty");

        scope.id
    }

    pub fn current_scope(&self) -> &ScopeContext<'thir> {
        self.scope_stack
            .last()
            .expect("active function has no lexical scope")
    }

    pub fn current_scope_mut(&mut self) -> &mut ScopeContext<'thir> {
        self.scope_stack
            .last_mut()
            .expect("active function has no lexical scope")
    }

    pub fn scope_stack(&self) -> &[ScopeContext<'thir>] {
        &self.scope_stack
    }

    #[allow(dead_code)]
    pub fn scope_stack_mut(&mut self) -> &mut [ScopeContext<'thir>] {
        &mut self.scope_stack
    }

    pub fn control_stack(&self) -> &[ControlContext] {
        &self.control_stack
    }

    pub fn current_control_mut(&mut self) -> &mut ControlContext {
        self.control_stack
            .last_mut()
            .expect("active control scope is missing")
    }
}
