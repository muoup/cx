use std::collections::HashMap;

use cx_mir::{
    MIRBasicBlockID, MIRFnPrototype, MIRFunction, MIRFunctionDefinition, MIRFunctionID,
    MIRInstrKind, MIRPlace, MIRRegister, MIRScopeID, MIRTypeID, MIRValue,
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoopContext {
    pub break_target: MIRBasicBlockID,
    pub continue_target: Option<MIRBasicBlockID>,
    pub lexical_scope_depth: usize,
}

#[derive(Debug)]
pub(crate) struct YieldContext {
    pub target: MIRBasicBlockID,
    pub result: Option<MIRRegister>,
    pub lexical_scope_depth: usize,
}

#[derive(Debug)]
struct LabelTarget {
    block: MIRBasicBlockID,
    declared: bool,
}

#[derive(Debug)]
pub(crate) struct FunctionContext {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    mir: MIRFunctionDefinition,
    current_block: MIRBasicBlockID,

    local_places: HashMap<THIRLocalID, MIRPlace>,
    local_values: HashMap<THIRLocalID, MIRValue>,

    named_values: Vec<HashMap<String, MIRValue>>,

    loops: Vec<LoopContext>,
    yields: Vec<YieldContext>,
    labels: HashMap<String, LabelTarget>,

    lexical_scopes: Vec<MIRScopeID>,
    defers: Vec<Vec<THIRExpression>>,
}

impl FunctionContext {
    pub(crate) fn new(
        id: MIRFunctionID,
        prototype: MIRFnPrototype,
        mir: MIRFunctionDefinition,
        current_block: MIRBasicBlockID,
        root_scope: MIRScopeID,
    ) -> Self {
        Self {
            id,
            prototype,
            mir,
            current_block,
            local_places: HashMap::new(),
            local_values: HashMap::new(),
            named_values: vec![HashMap::new()],
            loops: Vec::new(),
            yields: Vec::new(),
            labels: HashMap::new(),
            lexical_scopes: vec![root_scope],
            defers: vec![Vec::new()],
        }
    }

    pub(crate) fn finish(mut self, unit_type: MIRTypeID) -> MIRFunction {
        assert!(self.loops.is_empty(), "loop context stack is unbalanced");
        assert!(self.yields.is_empty(), "yield context stack is unbalanced");
        assert!(
            self.labels.values().all(|label| label.declared),
            "MIR function contains an unresolved label"
        );
        assert_eq!(
            self.lexical_scopes.len(),
            1,
            "lexical scope stack is unbalanced"
        );
        assert_eq!(self.defers.len(), 1, "defer stack is unbalanced");

        let returns_value = self.prototype.signature.return_type != unit_type;
        for block in self.mir.blocks_mut() {
            if block.terminator().is_some() {
                continue;
            }
            let terminator = if block.id == self.current_block && !returns_value {
                MIRInstrKind::Return { value: None }
            } else {
                MIRInstrKind::Unreachable
            };
            block.push(terminator);
        }
        MIRFunction::defined(self.id, self.prototype, self.mir)
    }

    pub(crate) fn _id(&self) -> MIRFunctionID {
        self.id
    }

    pub(crate) fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub(crate) fn set_current_block(&mut self, block: MIRBasicBlockID) {
        assert!(
            self.mir.block(block).is_some(),
            "selected block does not belong to the active function"
        );
        self.current_block = block;
    }

    pub(crate) fn new_block(&mut self, debug_name: &str) -> MIRBasicBlockID {
        let id = self.mir.add_block();
        self.set_block_name(id, debug_name);
        id
    }

    pub(crate) fn set_block_name(&mut self, block: MIRBasicBlockID, debug_name: &str) {
        self.mir
            .block_mut(block)
            .expect("selected block does not exist")
            .debug_name = Some(CXIdent::new(debug_name));
    }

    pub(crate) fn block_terminated(&self, block: MIRBasicBlockID) -> bool {
        self.mir
            .block(block)
            .expect("selected block does not exist")
            .terminator()
            .is_some()
    }

    pub(crate) fn current_block_terminated(&self) -> bool {
        self.block_terminated(self.current_block)
    }

    pub(crate) fn label_block(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        if let Some(label) = self.labels.get(name.as_str()) {
            return label.block;
        }
        let block = self.new_block(&format!("label.{}", name.as_str()));
        self.labels.insert(
            name.as_string(),
            LabelTarget {
                block,
                declared: false,
            },
        );
        block
    }

    pub(crate) fn declare_label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        let block = self.label_block(name);
        let label = self
            .labels
            .get_mut(name.as_str())
            .expect("label block was just allocated");
        assert!(!label.declared, "duplicate MIR label declaration");
        label.declared = true;
        block
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstrKind, source_range: TokenRange) -> bool {
        if self.current_block_terminated() {
            return false;
        }
        self.mir
            .push_instr_at(self.current_block, instruction, source_range)
            .expect("active MIR block is missing");
        true
    }

    pub(crate) fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.mir.add_register(ty, debug_name)
    }

    pub(crate) fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.mir.register(register).map(|register| register.ty)
    }

    pub(crate) fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.mir
            .add_block_param(block, ty, debug_name)
            .expect("selected block does not exist")
    }

    pub(crate) fn place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
    ) -> MIRPlace {
        let scope = self
            .lexical_scopes
            .last()
            .copied()
            .expect("active function has no lexical scope");
        self.mir.add_place(ty, debug_name, nodrop, scope)
    }

    pub(crate) fn bind_local(&mut self, local: THIRLocalID, place: MIRPlace) {
        self.local_places.insert(local, place);
    }

    pub(crate) fn bind_local_value(&mut self, local: THIRLocalID, value: MIRValue) {
        self.local_values.insert(local, value);
    }

    pub(crate) fn local(&self, local: THIRLocalID) -> Option<MIRPlace> {
        self.local_places.get(&local).copied()
    }

    pub(crate) fn local_value(&self, local: THIRLocalID) -> Option<MIRValue> {
        self.local_values.get(&local).cloned()
    }

    pub(crate) fn push_named_scope(&mut self) {
        self.named_values.push(HashMap::new());
    }

    pub(crate) fn pop_named_scope(&mut self) {
        assert!(
            self.named_values.len() > 1,
            "attempted to pop the function's base symbol scope"
        );
        self.named_values.pop();
    }

    pub(crate) fn bind_named(&mut self, name: &CXIdent, value: MIRValue) {
        self.named_values
            .last_mut()
            .expect("active function has no symbol scope")
            .insert(name.as_string(), value);
    }

    pub(crate) fn named(&self, name: &CXIdent) -> Option<MIRValue> {
        self.named_values
            .iter()
            .rev()
            .find_map(|scope| scope.get(name.as_str()).cloned())
    }

    pub(crate) fn push_lexical_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let scope = self.mir.add_scope(token_range);
        self.lexical_scopes.push(scope);
        self.defers.push(Vec::new());
        scope
    }

    pub(crate) fn pop_lexical_scope(&mut self) -> (MIRScopeID, Vec<THIRExpression>) {
        assert!(
            self.lexical_scopes.len() > 1,
            "attempted to pop the function's lexical scope"
        );
        assert_eq!(
            self.lexical_scopes.len(),
            self.defers.len(),
            "lexical scope and defer stacks are unbalanced"
        );
        let defers = self
            .defers
            .pop()
            .expect("active lexical scope has a defer list");
        let scope = self
            .lexical_scopes
            .pop()
            .expect("lexical scope stack is non-empty");
        (scope, defers)
    }

    pub(crate) fn lexical_scope_depth(&self) -> usize {
        self.lexical_scopes.len()
    }

    pub(crate) fn register_defer(&mut self, expression: THIRExpression) {
        self.defers
            .last_mut()
            .expect("active function has no defer scope")
            .push(expression);
    }

    pub(crate) fn lexical_scope_exits_to(
        &self,
        depth: usize,
    ) -> Vec<(MIRScopeID, Vec<THIRExpression>)> {
        assert!(
            depth <= self.lexical_scope_depth(),
            "invalid lexical scope depth"
        );
        self.lexical_scopes[depth..]
            .iter()
            .zip(&self.defers[depth..])
            .rev()
            .map(|(scope, defers)| (*scope, defers.clone()))
            .collect()
    }

    pub(crate) fn push_contextual_scope(
        &mut self,
        break_target: MIRBasicBlockID,
        continue_target: Option<MIRBasicBlockID>,
    ) {
        let lexical_scope_depth = self.lexical_scope_depth();
        self.loops.push(LoopContext {
            break_target,
            continue_target,
            lexical_scope_depth,
        });
    }

    pub(crate) fn pop_loop(&mut self) -> LoopContext {
        self.loops.pop().expect("loop context stack is unbalanced")
    }

    pub(crate) fn break_target(&self) -> Option<MIRBasicBlockID> {
        self.loops.last().map(|context| context.break_target)
    }

    pub(crate) fn continue_target(&self) -> Option<MIRBasicBlockID> {
        self.loops
            .iter()
            .rev()
            .find_map(|context| context.continue_target)
    }

    pub(crate) fn break_scope_depth(&self) -> Option<usize> {
        self.loops.last().map(|context| context.lexical_scope_depth)
    }

    pub(crate) fn continue_scope_depth(&self) -> Option<usize> {
        self.loops
            .iter()
            .rev()
            .find_map(|context| context.continue_target.map(|_| context.lexical_scope_depth))
    }

    pub(crate) fn push_yield(&mut self, target: MIRBasicBlockID, result_type: Option<MIRTypeID>) {
        let result = result_type.map(|ty| self.block_param(target, ty, None));
        let lexical_scope_depth = self.lexical_scope_depth();
        self.yields.push(YieldContext {
            target,
            result,
            lexical_scope_depth,
        });
    }

    pub(crate) fn yield_target(&self) -> Option<MIRBasicBlockID> {
        self.yields.last().map(|context| context.target)
    }

    pub(crate) fn yield_scope_depth(&self) -> Option<usize> {
        self.yields
            .last()
            .map(|context| context.lexical_scope_depth)
    }

    pub(crate) fn yield_result(&self) -> Option<MIRRegister> {
        self.yields.last().and_then(|context| context.result)
    }

    pub(crate) fn pop_yield(&mut self) -> YieldContext {
        self.yields
            .pop()
            .expect("yield context stack is unbalanced")
    }

    pub(crate) fn root_defers(&self) -> Vec<THIRExpression> {
        self.defers.first().cloned().unwrap_or_default()
    }
}
