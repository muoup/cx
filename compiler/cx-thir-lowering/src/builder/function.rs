use std::collections::HashMap;

use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRFunctionMode,
    MIRInstrKind, MIRPlace, MIRRegister, MIRScopeID, MIRType, MIRTypeID, MIRValue,
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug)]
pub(crate) struct FunctionBuilder<'thir> {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    mode: MIRFunctionMode,

    body: MIRBodyBuilder,

    scope_stack: Vec<ScopeContext<'thir>>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext<'a> {
    id: MIRScopeID,

    yield_target: Option<(MIRBasicBlockID, MIRType)>,
    continue_target: Option<MIRBasicBlockID>,
    break_target: Option<MIRBasicBlockID>,

    cleanups: Vec<&'a THIRExpression>,
}

impl FunctionBuilder {
    pub(crate) fn new(func: MIRFunction) -> Self {
        Self {
            id: func.id(),
            mode: func.mode(),
            prototype: *func.prototype(),
            body: MIRBody::new(),

            scope_stack: Vec::new(),
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
        for block in self.body.blocks_mut() {
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

        MIRFunction::new(self.id, self.prototype, Some(self.body.finish()))
    }

    pub(crate) fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub(crate) fn definition(self) -> MIRBody {
        self.body
    }

    pub(crate) fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub(crate) fn emit(&mut self, instruction: MIRInstrKind, source_range: TokenRange) -> bool {
        if self.current_block_terminated() {
            return false;
        }
        self.body
            .push_instr_at(self.current_block, instruction, source_range)
            .expect("active MIR block is missing");
        true
    }

    pub(crate) fn register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        self.body.add_register(ty, debug_name)
    }

    pub(crate) fn register_type(&self, register: MIRRegister) -> Option<MIRTypeID> {
        self.body.register(register).map(|register| register.ty)
    }

    pub(crate) fn block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        self.body
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
        self.body.add_place(ty, debug_name, nodrop, scope)
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
        let scope = self.body.add_scope(token_range);
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
