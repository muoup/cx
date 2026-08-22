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

    local_places: HashMap<THIRLocalID, MIRPlace>,
    local_values: HashMap<THIRLocalID, MIRValue>,
    named_values: Vec<HashMap<String, MIRValue>>,
    labels: HashMap<String, LabelTarget>,

    scope_stack: Vec<ScopeContext>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext {
    id: Option<MIRScopeID>,

    yield_target: Option<MIRBasicBlockID>,
    yield_result: Option<MIRRegister>,

    break_target: Option<MIRBasicBlockID>,
    continue_target: Option<MIRBasicBlockID>,

    cleanups: Vec<THIRExpression>,
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result: Option<MIRRegister>,
}

#[derive(Debug)]
struct LabelTarget {
    block: MIRBasicBlockID,
    declared: bool,
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

            local_places: HashMap::new(),
            local_values: HashMap::new(),
            named_values: vec![HashMap::new()],
            labels: HashMap::new(),

            scope_stack: vec![ScopeContext {
                id: Some(root_scope),
                yield_target: None,
                yield_result: None,
                break_target: None,
                continue_target: None,
                cleanups: Vec::new(),
            }],

            body,
        }
    }

    pub(crate) fn finish(self) -> MIRFunction {
        assert!(
            self.scope_stack.len() == 1,
            "scope stack is unbalanced at function end"
        );
        assert!(
            self.labels.values().all(|label| label.declared),
            "MIR function contains an unresolved label"
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

    pub fn label_block(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        if let Some(label) = self.labels.get(name.as_str()) {
            return label.block;
        }
        let block = self.body.add_block_named(format!("label.{}", name.as_str()));
        self.labels.insert(
            name.as_string(),
            LabelTarget {
                block,
                declared: false,
            },
        );
        block
    }

    pub fn declare_label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        let block = self.label_block(name);
        let label = self
            .labels
            .get_mut(name.as_str())
            .expect("label block was just allocated");
        assert!(!label.declared, "duplicate MIR label declaration");
        label.declared = true;
        block
    }

    pub fn emit(&mut self, instruction: MIRInstrKind, range: TokenRange) {
        if self.current_block_terminated() {
            return;
        }
        self.active_block_mut()
            .instrs
            .push(MIRInstr::new(instruction, range));
    }

    pub fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.body.add_register(ty, debug_name)
    }

    pub fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.body.register(register).map(|decl| decl.ty)
    }

    pub fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.body.add_block_param(block, ty, debug_name)
    }

    pub fn place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let scope = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|context| context.id)
            .expect("active function has no lexical scope");
        self.body.add_place(ty, debug_name, nodrop, scope)
    }

    pub fn bind_local(&mut self, local: THIRLocalID, place: MIRPlace) {
        self.local_places.insert(local, place);
    }

    pub fn bind_local_value(&mut self, local: THIRLocalID, value: MIRValue) {
        self.local_values.insert(local, value);
    }

    pub fn local(&self, local: THIRLocalID) -> Option<MIRPlace> {
        self.local_places.get(&local).copied()
    }

    pub fn local_value(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.local_values.get(&local).cloned()
    }

    pub fn push_named_scope(&mut self) {
        self.named_values.push(HashMap::new());
    }

    pub fn pop_named_scope(&mut self) {
        assert!(
            self.named_values.len() > 1,
            "attempted to pop the function's base symbol scope"
        );
        self.named_values.pop();
    }

    pub fn bind_named(&mut self, name: &CXIdent, value: MIRValue) {
        self.named_values
            .last_mut()
            .expect("active function has no symbol scope")
            .insert(name.as_string(), value);
    }

    pub fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.named_values
            .iter()
            .rev()
            .find_map(|scope| scope.get(name.as_str()).cloned())
    }

    pub fn push_lexical_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let scope = self.body.add_scope(token_range);
        self.scope_stack.push(ScopeContext {
            id: Some(scope),
            yield_target: None,
            yield_result: None,
            break_target: None,
            continue_target: None,
            cleanups: Vec::new(),
        });
        scope
    }

    pub fn pop_lexical_scope(&mut self) -> (MIRScopeID, Vec<THIRExpression>) {
        let index = self.top_lexical_index();
        let context = self.scope_stack.remove(index);
        (
            context.id.expect("lexical scope entry owns a MIR scope"),
            context.cleanups,
        )
    }

    pub fn lexical_scope_depth(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn register_defer(&mut self, expression: THIRExpression) {
        let index = self.top_lexical_index();
        self.scope_stack[index].cleanups.push(expression);
    }

    pub fn lexical_scope_exits_to(
        &self,
        depth: usize,
    ) -> Vec<(MIRScopeID, Vec<THIRExpression>)> {
        assert!(depth <= self.scope_stack.len(), "invalid lexical scope depth");
        self.scope_stack[depth..]
            .iter()
            .filter_map(|context| context.id.map(|id| (id, context.cleanups.clone())))
            .rev()
            .collect()
    }

    pub fn root_defers(&self) -> Vec<THIRExpression> {
        self.scope_stack
            .first()
            .map(|context| context.cleanups.clone())
            .unwrap_or_default()
    }

    pub fn push_contextual_scope(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        self.scope_stack.push(ScopeContext {
            id: None,
            yield_target: None,
            yield_result: None,
            break_target: Some(break_target),
            continue_target,
            cleanups: Vec::new(),
        });
    }

    pub fn pop_loop(&mut self) {
        let context = self
            .scope_stack
            .pop()
            .expect("loop context stack is unbalanced");
        assert!(
            context.break_target.is_some(),
            "attempted to pop a non-loop scope context"
        );
    }

    pub fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.scope_stack
            .iter()
            .rev()
            .find_map(|context| context.break_target)
    }

    pub fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.scope_stack
            .iter()
            .rev()
            .find_map(|context| context.continue_target)
    }

    pub fn break_scope_depth(&self) -> Option<usize> {
        self.scope_stack
            .iter()
            .rposition(|context| context.break_target.is_some())
    }

    pub fn continue_scope_depth(&self) -> Option<usize> {
        self.scope_stack
            .iter()
            .rposition(|context| context.continue_target.is_some())
    }

    pub fn push_yield(&mut self, target: MIRBasicBlockID, result_type: Option<MIRTypeID>) {
        let result = result_type.map(|ty| self.block_param(target, ty, None));
        self.scope_stack.push(ScopeContext {
            id: None,
            yield_target: Some(target),
            yield_result: result,
            break_target: None,
            continue_target: None,
            cleanups: Vec::new(),
        });
    }

    pub fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.scope_stack
            .iter()
            .rev()
            .find_map(|context| context.yield_target)
    }

    pub fn yield_scope_depth(&self) -> Option<usize> {
        self.scope_stack
            .iter()
            .rposition(|context| context.yield_target.is_some())
    }

    pub fn yield_result(&self) -> Option<MIRRegister> {
        self.scope_stack
            .iter()
            .rev()
            .find_map(|context| context.yield_result)
    }

    pub fn pop_yield(&mut self) -> YieldContext {
        let index = self
            .scope_stack
            .iter()
            .rposition(|context| context.yield_target.is_some())
            .expect("yield context stack is unbalanced");
        let context = self.scope_stack.remove(index);
        let target = context.yield_target.expect("yield entry carries a target");
        YieldContext {
            target,
            result: context.yield_result,
        }
    }

    fn top_lexical_index(&self) -> usize {
        self.scope_stack
            .iter()
            .rposition(|context| context.id.is_some())
            .expect("active function has no lexical scope")
    }
}
