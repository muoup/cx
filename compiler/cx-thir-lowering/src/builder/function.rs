use std::collections::HashMap;

use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID,
    MIRFunctionMode, MIRInstr, MIRInstrKind, MIRPlace, MIRRegister, MIRScopeID, MIRTypeID,
    MIRValue,
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug)]
pub(crate) struct FunctionBuilder {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    mode: MIRFunctionMode,

    body: MIRBody,
    current_block: MIRBasicBlockID,

    local_values: HashMap<THIRLocalID, MIRValue>,
    labels: HashMap<String, MIRBasicBlockID>,

    scope_stack: Vec<ScopeContext>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext {
    id: MIRScopeID,

    pub(crate) yield_target: Option<MIRBasicBlockID>,

    pub(crate) break_target: Option<MIRBasicBlockID>,
    pub(crate) continue_target: Option<MIRBasicBlockID>,

    named_values: HashMap<String, MIRValue>,
    pub(crate) defered_expressions: Vec<THIRExpression>,
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

    pub fn deferred_expressions(&self) -> &[THIRExpression] {
        &self.defered_expressions
    }
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result: Option<MIRRegister>,
}

impl FunctionBuilder {
    pub(crate) fn new(func: MIRFunction) -> Self {
        let mut body = MIRBody::new();
        let entry = body.add_block();
        let root_scope = body.add_scope(TokenRange::internal());

        Self {
            id: func.id(),
            mode: func.mode(),
            prototype: func.prototype().clone(),

            current_block: entry,

            local_values: HashMap::new(),
            labels: HashMap::new(),

            scope_stack: vec![ScopeContext::new(root_scope)],

            body,
        }
    }

    pub(crate) fn finish(self) -> MIRFunction {
        assert!(
            self.scope_stack.len() == 1,
            "scope stack is unbalanced at function end"
        );

        MIRFunction::new(self.id, self.prototype, Some(self.body))
    }

    pub(crate) fn concise_finish(self) -> (MIRFunctionID, MIRBody) {
        (self.id, self.body)
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn mode(&self) -> MIRFunctionMode {
        self.mode
    }

    pub fn body(&self) -> &MIRBody {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut MIRBody {
        &mut self.body
    }

    fn active_block(&self) -> &MIRBasicBlock {
        self.body
            .block(self.current_block)
            .expect("current block must exist")
    }

    fn active_block_mut(&mut self) -> &mut MIRBasicBlock {
        let block = self.current_block;
        self.body
            .block_mut(block)
            .expect("current block must exist")
    }

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

    pub fn block_terminated(&self, block: MIRBasicBlockID) -> bool {
        self.body
            .block(block)
            .expect("selected block does not exist")
            .instrs
            .last()
            .is_some_and(|instr| instr.is_terminator())
    }

    pub fn current_block_terminated(&self) -> bool {
        self.active_block()
            .instrs
            .last()
            .is_some_and(|instr| instr.is_terminator())
    }

    pub fn set_yield_recipient(&mut self, target: MIRBasicBlockID, ty: MIRTypeID) -> MIRRegister {
        self.block_param(target, ty, Some(CXIdent::new("yield_result")))
    }

    pub fn label(&mut self, name: &CXIdent) -> Option<MIRBasicBlockID> {
        self.labels.get(name.as_str())
            .copied()
    }

    pub fn declare_label(&mut self, name: &CXIdent, id: MIRBasicBlockID) {
        self.labels.insert(name.to_string(), id);
    }

    pub fn emit(&mut self, instruction: MIRInstrKind, range: TokenRange) {
        if self.current_block_terminated() {
            return;
        }
        
        self.active_block_mut()
            .instrs
            .push(MIRInstr::new(instruction, range));
    }

    pub fn new_register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.body.add_register(ty, debug_name)
    }

    pub fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.body.register(register).map(|decl| decl.ty)
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

    pub fn new_place(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>, nodrop: bool) -> MIRPlace {
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
    pub fn pop_scope(&mut self) -> (MIRScopeID, Vec<THIRExpression>) {
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

    pub fn lexical_scope_depth(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn scope_stack(&self) -> &[ScopeContext] {
        &self.scope_stack
    }
}
