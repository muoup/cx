use std::{collections::HashMap, rc::Rc};

use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::visit::MIRWalk;
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRComptimeBody, MIRFnPrototype, MIRFunction, MIRFunctionBody, MIRFunctionID, MIRFunctionMode, MIRInstruction, MIRPlaceID, MIRRegister, MIRScopeID, MIRStagedBody, MIRStagedCapture, MIRStagedExitKind, MIRStagedInstrKind, MIRTypeID, MIRValue,
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::log::mir_error;

#[derive(Debug)]
pub(crate) struct CaptureContext {
    pub(crate) source_locals: HashMap<THIRLocalID, MIRValue>,
    pub(crate) captures: Vec<(MIRStagedCapture, MIRValue)>,
    pub(crate) params: Vec<MIRRegister>,
    pub(crate) runtime_places: bool,
}

#[derive(Debug)]
pub(crate) struct MIRFunctionBuilder {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    source_range: TokenRange,

    body: MIRFunctionBody,
    current_block: MIRBasicBlockID,

    local_values: HashMap<THIRLocalID, MIRValue>,
    labels: HashMap<String, MIRBasicBlockID>,

    scope_stack: Vec<ScopeContext>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext {
    id: MIRScopeID,

    yield_target: Option<MIRBasicBlockID>,
    break_target: Option<MIRBasicBlockID>,
    continue_target: Option<MIRBasicBlockID>,

    named_values: HashMap<String, MIRValue>,

    // Rc, not Arc because we guarantee that a module is compiled single-threaded, parallelism is only applied at the scheduling
    // level, not at the compilation level
    defered_expressions: Vec<Rc<THIRExpression>>,
}

impl ScopeContext {
    pub fn new(id: MIRScopeID) -> Self {
        Self {
            id,

            yield_target: None,
            break_target: None,
            continue_target: None,
            named_values: HashMap::new(),
            defered_expressions: Vec::new(),
        }
    }

    pub fn set_yield_target(&mut self, target: MIRBasicBlockID) {
        self.yield_target = Some(target);
    }

    pub fn set_break_target(&mut self, target: MIRBasicBlockID) -> &mut Self {
        self.break_target = Some(target);
        self
    }

    pub fn set_continue_target(&mut self, target: MIRBasicBlockID) -> &mut Self {
        self.continue_target = Some(target);
        self
    }

    pub fn deferred_expressions(&self) -> &[Rc<THIRExpression>] {
        &self.defered_expressions
    }

    pub(crate) fn id(&self) -> MIRScopeID {
        self.id
    }
}

impl MIRFunctionBuilder {
    pub(crate) fn new_runtime(func: MIRFunction, parent: Option<&Self>) -> Self {
        let mut body = MIRBody::new();
        let entry = body.add_block();
        let root_scope = body.add_scope(TokenRange::internal());

        Self {
            id: func.id(),
            prototype: func.prototype().clone(),
            source_range: parent
                .map(|parent| parent.source_range.clone())
                .unwrap_or_else(TokenRange::internal),

            current_block: entry,

            local_values: HashMap::new(),
            labels: HashMap::new(),

            scope_stack: vec![ScopeContext::new(root_scope)],

            body,
        }
    }

    pub(crate) fn new_comptime(func: MIRFunction, parent: Option<&Self>) -> Self {
        let mut body = MIRComptimeBody::new();
        let entry = body.add_block();
        let root_scope = body.add_scope(TokenRange::internal());

        Self {
            id: func.id(),
            prototype: func.prototype().clone(),
            source_range: parent
                .map(|parent| parent.source_range.clone())
                .unwrap_or_else(TokenRange::internal),

            current_block: entry,

            local_values: HashMap::new(),
            labels: HashMap::new(),

            scope_stack: vec![ScopeContext::new(root_scope)],

            body,
        }
    }

    pub(crate) fn thin_finish(self) -> (MIRFunctionID, Body) {
        (self.id, self.body)
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub(crate) fn set_source_range(&mut self, range: TokenRange) -> TokenRange {
        std::mem::replace(&mut self.source_range, range)
    }

    pub(crate) fn restore_source_range(&mut self, range: TokenRange) {
        self.source_range = range;
    }

    pub(crate) fn source_range(&self) -> &TokenRange {
        &self.source_range
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn body(&self) -> &Body {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Body {
        &mut self.body
    }

    #[allow(dead_code)]
    pub fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        assert!(
            self.body.block(block).is_some(),
            "selected block does not belong to the active function"
        );
        self.current_block = block;
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

    pub fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.body.register(register).map(|decl| decl.ty)
    }

    pub fn emit(&self, instr: MIRInstruction) {
        
    }

    pub fn new_block(&mut self, name: impl Into<CXIdent>) -> MIRBasicBlockID {
        self.body.add_block_named(name)
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

    pub fn local(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.local_values.get(&local).cloned()
    }

    pub fn locals(&self) -> HashMap<THIRLocalID, MIRValue> {
        self.local_values.clone()
    }

    pub(crate) fn take_capture(&mut self) -> Option<CaptureContext> {
        self.capture.take()
    }

    pub(crate) fn set_capture(&mut self, capture: Option<CaptureContext>) {
        self.capture = capture;
    }

    pub fn bind_local(&mut self, local: THIRLocalID, value: MIRValue) {
        self.local_values.insert(local, value);
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

    pub fn push_invisible_scope(&mut self) -> MIRScopeID {
        let scope = self.body.add_scope(self.current_scope_range());
        self.scope_stack.push(ScopeContext::new(scope));
        scope
    }

    pub fn push_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let scope = self.body.add_scope(token_range);
        self.scope_stack.push(ScopeContext::new(scope));
        scope
    }

    #[must_use]
    pub fn pop_scope(&mut self) -> (MIRScopeID, Vec<Rc<THIRExpression>>) {
        let scope = self.scope_stack.pop().expect("scope stack is empty");

        (scope.id, scope.defered_expressions)
    }

    pub fn current_scope(&self) -> &ScopeContext {
        self.scope_stack
            .last()
            .expect("active function has no lexical scope")
    }

    pub fn current_scope_mut(&mut self) -> &mut ScopeContext {
        self.scope_stack
            .last_mut()
            .expect("active function has no lexical scope")
    }

    pub fn scope_stack(&self) -> &[ScopeContext] {
        &self.scope_stack
    }

    pub fn exit_target(&self, kind: MIRStagedExitKind) -> Option<(MIRScopeID, MIRBasicBlockID)> {
        self.scope_stack.iter().rev().find_map(|scope| {
            let block = match kind {
                MIRStagedExitKind::Break => scope.break_target,
                MIRStagedExitKind::Continue => scope.continue_target,
                MIRStagedExitKind::Expr => None,
            }?;

            Some((scope.id(), block))
        })
    }

    pub fn scope_stack_mut(&mut self) -> &mut [ScopeContext] {
        &mut self.scope_stack
    }
}
