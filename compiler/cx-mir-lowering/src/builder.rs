use std::collections::HashMap;

use crate::mir_lowering::types::convert_cx_prototype;
use crate::{LMIRResult, LMIRUnit};
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRTypeID, LMIRTypeKind};
use cx_lmir::*;
use cx_log::CXResult;
use cx_target::ArchitectureConfig;
use cx_thir::layout::THIRTypeLayout;
use cx_thir::registry::THIRDecomposedRegistry;
use cx_thir::thir::{data::THIRFnPrototype, expression::THIRLocalID};
use cx_thir::type_context::THIRTypeContext;
use cx_thir::THIRUnit;
use cx_util::format::dump_all;
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::unsafe_float::FloatWrapper;

#[derive(Debug)]
pub struct LMIRBuilder {
    pub registry: THIRDecomposedRegistry,
    pub fn_map: LMIRFunctionMap,

    functions: Vec<LMIRFunction>,
    global_variables: Vec<LMIRGlobalValue>,

    symbol_table: ScopedMap<String, LMIRValue>,
    goto_stack: Vec<LMIRGotoContext>,
    yield_stack: Vec<LMIRYieldContext>,
    function_context: Option<LMIRFunctionContext>,
}

#[derive(Debug)]
pub struct LMIRGotoContext {
    pub break_block: Option<CXIdent>,
    pub continue_block: Option<CXIdent>,
}

#[derive(Debug)]
pub struct LMIRYieldContext {
    pub target_block: CXIdent,
    pub result_type: LMIRTypeID,
    pub yields: Vec<(LMIRValue, CXIdent)>,
}

#[derive(Debug)]
pub struct LMIRFunctionContext {
    prototype: LMIRFunctionPrototype,
    mir_prototype: THIRFnPrototype,

    current_block: usize,
    register_counter: u32,

    blocks: Vec<LMIRBasicBlock>,
    local_symbols: HashMap<THIRLocalID, LMIRValue>,
}

impl LMIRBuilder {
    pub fn new(mir: &THIRUnit) -> Self {
        LMIRBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),
            registry: mir.registry.clone(),

            fn_map: HashMap::new(),
            symbol_table: ScopedMap::new_with_starting_scope(),
            goto_stack: Vec::new(),
            yield_stack: Vec::new(),
            function_context: None,
        }
    }

    pub(crate) fn type_layout(&self, ty: &cx_thir::thir::data::THIRType) -> THIRTypeLayout {
        self.registry
            .type_layout(ty)
            .unwrap_or_else(|err| panic!("Failed to calculate MIR layout: {}", err.message()))
    }

    pub fn architecture(&self) -> &ArchitectureConfig {
        self.registry.architecture()
    }

    pub fn new_register(&mut self) -> LMIRRegister {
        let context = self.fun_mut();

        let reg_id = context.register_counter;
        context.register_counter += 1;

        LMIRRegister::new(format!("{}", reg_id))
    }

    pub fn new_function(&mut self, fn_prototype: THIRFnPrototype) {
        assert!(
            self.function_context.is_none(),
            "Attempted to start a new function while another function context is active"
        );

        let bc_prototype = convert_cx_prototype(&fn_prototype, &self.registry);

        self.insert_fn_prototype(bc_prototype.clone());
        self.function_context = Some(LMIRFunctionContext {
            prototype: bc_prototype,
            mir_prototype: fn_prototype,
            current_block: 0,
            register_counter: 0,

            blocks: Vec::new(),
            local_symbols: HashMap::new(),
        });
        self.push_scope(None, None);
    }

    /// Take the current function context, leaving None in its place.
    /// Used when generating nested helper functions.
    pub fn take_function_context(&mut self) -> Option<LMIRFunctionContext> {
        self.function_context.take()
    }

    /// Set the function context.
    /// Used to restore a previously saved context.
    pub fn set_function_context(&mut self, context: LMIRFunctionContext) {
        self.function_context = Some(context);
    }

    pub fn finish_function(&mut self) -> CXResult<()> {
        self.pop_scope()?;

        let context = self.function_context.take().unwrap();

        self.functions.push(LMIRFunction {
            prototype: context.prototype,
            blocks: context.blocks,
        });

        Ok(())
    }

    pub fn push_scope(&mut self, continue_block: Option<CXIdent>, break_block: Option<CXIdent>) {
        self.symbol_table.push_scope();
        self.goto_stack.push(LMIRGotoContext {
            continue_block,
            break_block,
        });
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.symbol_table.pop_scope();
        self.goto_stack.pop();

        Ok(())
    }

    pub fn dump_current_fn(&self) {
        dump_all(self.fun().blocks.iter());
    }

    fn fun_mut(&mut self) -> &mut LMIRFunctionContext {
        self.function_context
            .as_mut()
            .expect("Attempted to access function context with no current function selected")
    }

    fn fun(&self) -> &LMIRFunctionContext {
        self.function_context
            .as_ref()
            .expect("Attempted to access function context with no current function selected")
    }

    pub fn insert_symbol(&mut self, mir_value: CXIdent, bc_value: LMIRValue) {
        self.symbol_table.insert(mir_value.to_string(), bc_value);
    }

    pub fn insert_local(&mut self, local_id: THIRLocalID, value: LMIRValue) {
        self.fun_mut().local_symbols.insert(local_id, value);
    }

    pub fn insert_fn_prototype(&mut self, prototype: LMIRFunctionPrototype) {
        self.fn_map.insert(prototype.name.to_string(), prototype);
    }

    #[allow(dead_code)]
    pub fn dump_symbols(&self) {
        dump_all(
            self.symbol_table
                .iter()
                .map(|(name, value)| format!("{name}: {value}")),
        );
    }

    pub fn get_continue_block(&self) -> Option<&CXIdent> {
        self.goto_stack
            .iter()
            .rev()
            .find_map(|ctx| ctx.continue_block.as_ref())
    }

    pub fn get_break_target(&self) -> Option<&CXIdent> {
        self.goto_stack
            .iter()
            .rev()
            .find_map(|ctx| ctx.break_block.as_ref())
    }

    pub fn push_yield_target(&mut self, target_block: CXIdent, result_type: LMIRTypeID) {
        self.yield_stack.push(LMIRYieldContext {
            target_block,
            result_type,
            yields: Vec::new(),
        });
    }

    pub fn pop_yield_target(&mut self) -> LMIRYieldContext {
        self.yield_stack
            .pop()
            .expect("Yield target stack has uneven push/pop")
    }

    pub fn current_yield_target(&self) -> Option<&LMIRYieldContext> {
        self.yield_stack.last()
    }

    pub fn record_yield(&mut self, value: LMIRValue) {
        let block = self.current_block();
        self.yield_stack
            .last_mut()
            .expect("Yield lowered outside of an active yield target")
            .yields
            .push((value, block));
    }

    pub fn move_block_to_end(&mut self, block_id: &CXIdent) {
        let context = self.fun_mut();

        if let Some(pos) = context.blocks.iter().position(|b| &b.id == block_id) {
            let block = context.blocks.remove(pos);
            let new_end_index = context.blocks.len();
            context.blocks.push(block);

            // Adjust current_block removed block was current, set to new end index
            // - If pos < current, decrement by 1 to account for left-shift
            // - Otherwise, keep current_block unchanged
            if pos == context.current_block {
                context.current_block = new_end_index;
            } else if pos < context.current_block {
                context.current_block -= 1;
            }
        }
    }

    pub fn scope_depth(&self) -> usize {
        self.symbol_table.scope_depth()
    }

    pub fn current_mir_prototype(&self) -> &THIRFnPrototype {
        &self.fun().mir_prototype
    }

    pub fn current_prototype(&self) -> &LMIRFunctionPrototype {
        &self.fun().prototype
    }

    pub fn get_prototype(&self, name: &str) -> Option<&LMIRFunctionPrototype> {
        self.fn_map.get(name)
    }

    pub fn get_symbol(&self, name: &CXIdent) -> Option<LMIRValue> {
        self.symbol_table.get(name.as_str()).cloned()
    }

    pub fn get_local(&self, local_id: THIRLocalID) -> Option<LMIRValue> {
        self.fun().local_symbols.get(&local_id).cloned()
    }

    pub fn get_global_symbol(&self, name: &str) -> Option<LMIRValue> {
        self.global_variables
            .iter()
            .position(|global| global.name.as_str() == name)
            .map(|index| LMIRValue::Global(index as u32))
    }

    pub fn add_global_variable(&mut self, value: LMIRGlobalValue) -> u32 {
        self.global_variables.push(value);

        (self.global_variables.len() - 1) as u32
    }

    pub fn create_static_string(&mut self, value: String) -> LMIRValue {
        let global_index = self.global_variables.len() as u32;

        self.global_variables.push(LMIRGlobalValue {
            name: CXIdent::from(format!("str_{}", global_index)),
            _type: LMIRGlobalType::StringLiteral(value),
            linkage: LinkageType::Static,
        });

        LMIRValue::Global(global_index)
    }

    pub fn current_block_closed(&self) -> bool {
        let Some(last_inst) = self.current_block_last_inst() else {
            return false;
        };

        last_inst.kind.is_block_terminating()
    }

    // Creates an instruction without a direct mapping to a MIR instruction
    // In effect, this just means that the generator will need to create a new register
    // if a result is expected
    pub fn add_new_instruction(
        &mut self,
        instruction: LMIRInstructionKind,
        value_type: LMIRTypeID,
        result_expected: bool,
    ) -> CXResult<LMIRValue> {
        if self.current_block_closed() {
            return Ok(LMIRValue::NULL);
        }

        let result = if result_expected {
            Some(self.new_register())
        } else {
            None
        };

        let context = self.fun_mut();
        let current_block = context.current_block;

        context.blocks[current_block].body.push(LMIRInstruction {
            kind: instruction,
            value_type: value_type.clone(),
            result: result.clone(),
        });

        match result {
            Some(reg) => Ok(LMIRValue::Register {
                register: reg,
                _type: value_type,
            }),
            None => Ok(LMIRValue::NULL),
        }
    }

    pub fn add_instruction_translated(
        &mut self,
        instruction: LMIRInstructionKind,
        value_type: LMIRTypeID,
        result: Option<CXIdent>,
    ) -> CXResult<LMIRValue> {
        if self.current_block_closed() {
            return Ok(LMIRValue::NULL);
        }

        let (result, result_val) = if let Some(result) = result.clone() {
            let bc_result = self.new_register();
            let bc_result_val = LMIRValue::Register {
                register: bc_result.clone(),
                _type: value_type.clone(),
            };

            self.insert_symbol(result, bc_result_val.clone());
            (Some(bc_result), bc_result_val)
        } else {
            (None, LMIRValue::NULL)
        };

        let context = self.fun_mut();
        let current_block = context.current_block;

        context.blocks[current_block].body.push(LMIRInstruction {
            kind: instruction,
            value_type,
            result: result.clone(),
        });

        Ok(result_val)
    }

    pub fn fn_ref(&mut self, name: &str) -> LMIRResult<String> {
        if self.fn_map.contains_key(name) {
            Some(name.to_string())
        } else {
            None
        }
    }

    pub fn get_value_type(&self, value: &LMIRValue) -> LMIRTypeID {
        match value {
            LMIRValue::NULL => LMIRTypeID::unit(),

            LMIRValue::Register { _type, .. } => _type.clone(),

            LMIRValue::FloatImmediate { _type, .. } | LMIRValue::IntImmediate { _type, .. } => {
                _type.clone()
            }

            LMIRValue::ParameterRef(param_index) => {
                let context = self.fun();
                let signature = context.prototype.signature();
                signature
                    .expanded_param_type(self.architecture(), *param_index as usize)
                    .expect("Parameter index out of bounds in function prototype")
            }
            LMIRValue::Global(global_index) => {
                let global = self
                    .global_variables
                    .get(*global_index as usize)
                    .expect("Global variable index out of bounds");

                match &global._type {
                    LMIRGlobalType::StringLiteral(..) => {
                        LMIRTypeID::default_pointer(self.architecture())
                    }
                    LMIRGlobalType::Variable { _type, .. } => _type.clone(),
                }
            }

            LMIRValue::FunctionRef(_) => LMIRTypeID::default_pointer(self.architecture()),
        }
    }

    pub fn match_int_const(&self, value: i32, _type: &LMIRTypeID) -> LMIRValue {
        match &_type.kind {
            LMIRTypeKind::Integer(_type) => self.int_const(value, *_type),

            _ => {
                panic!("PANIC: Attempted to match integer constant with non-integer type: {_type}")
            }
        }
    }

    pub fn int_const(&self, value: i32, _type: LMIRIntegerType) -> LMIRValue {
        LMIRValue::IntImmediate {
            val: value as i64,
            _type: LMIRTypeID::with_implicit_abi(self.architecture(), LMIRTypeKind::Integer(_type)),
        }
    }

    pub fn match_float_const(&self, value: f64, _type: &LMIRTypeID) -> LMIRValue {
        match &_type.kind {
            LMIRTypeKind::Float(_type) => self.float_const(value, *_type),

            _ => panic!("PANIC: Attempted to match float constant with non-float type: {_type}"),
        }
    }

    pub fn float_const(&self, value: f64, _type: LMIRFloatType) -> LMIRValue {
        LMIRValue::FloatImmediate {
            val: FloatWrapper::from(value),
            _type: LMIRTypeID::with_implicit_abi(self.architecture(), LMIRTypeKind::Float(_type)),
        }
    }

    pub fn create_block(&mut self, debug_name: Option<&str>) -> CXIdent {
        let context = self.fun_mut();
        let name: CXIdent = format!("block_{}", context.blocks.len()).into();

        context.blocks.push(LMIRBasicBlock {
            id: name.clone(),
            debug_name: debug_name.map(|s| s.to_string()),
            body: Vec::new(),
        });

        name
    }

    pub fn set_current_block(&mut self, block: LMIRBlockID) {
        let fun = self.fun();

        let block_id = fun.blocks.iter().position(|b| b.id == block);

        self.fun_mut().current_block = block_id.expect("Block ID not found in function blocks");
    }

    pub fn block_count(&self) -> usize {
        let fun = self.fun();

        fun.blocks.len()
    }

    pub fn current_block(&self) -> LMIRBlockID {
        let fun = self.fun();

        fun.blocks[fun.current_block].id.clone()
    }

    pub fn last_instruction(&self) -> Option<&LMIRInstruction> {
        let context = self.fun();

        context.blocks.last()?.body.last()
    }

    pub fn current_block_last_inst(&self) -> Option<&LMIRInstruction> {
        let context = self.fun();

        context.blocks.get(context.current_block)?.body.last()
    }

    pub fn current_function_name(&self) -> Option<&str> {
        self.function_context
            .as_ref()
            .map(|ctx| ctx.prototype.name.as_str())
    }

    pub fn finish(self) -> LMIRUnit {
        LMIRUnit {
            architecture: *self.registry.architecture(),
            fn_map: self.fn_map,
            fn_defs: self.functions,

            global_vars: self.global_variables,
        }
    }
}
