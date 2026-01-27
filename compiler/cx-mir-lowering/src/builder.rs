use std::collections::HashSet;

use crate::mir_lowering::deconstructors::{
    create_deconstructor_prototype, invoke_conditional_deconstruction,
};
use crate::mir_lowering::types::convert_cx_prototype;
use crate::{LMIRUnit, LMIRResult};
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::*;
use cx_mir::mir::name_mangling::{base_mangle_deconstructor, base_mangle_destructor};
use cx_mir::mir::program::MIRUnit;
use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};
use cx_util::format::dump_all;
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::unsafe_float::FloatWrapper;
use cx_util::CXResult;

#[derive(Debug, Clone)]
pub struct LivenessEntry {
    pub region_ptr: LMIRValue,
    pub liveness_ptr: LMIRValue,
    pub mir_type: MIRType,
    pub scope_depth: usize,
}

#[derive(Debug)]
pub struct LMIRBuilder {
    functions: Vec<LMIRFunction>,
    global_variables: Vec<LMIRGlobalValue>,

    pub fn_map: LMIRFunctionMap,

    symbol_table: ScopedMap<LMIRValue>,
    liveness_table: ScopedMap<LivenessEntry>,
    goto_stack: Vec<LMIRGotoContext>,

    deconstructors_needed: HashSet<MIRType>,
    function_context: Option<LMIRFunctionContext>,
}

#[derive(Debug)]
pub struct LMIRGotoContext {
    pub break_block: Option<CXIdent>,
    pub continue_block: Option<CXIdent>,
}

#[derive(Debug)]
pub struct LMIRFunctionContext {
    prototype: LMIRFunctionPrototype,
    mir_prototype: MIRFunctionPrototype,

    current_block: usize,
    register_counter: u32,
    return_buffer_size: Option<usize>,

    blocks: Vec<LMIRBasicBlock>,
}

impl LMIRBuilder {
    pub fn new(mir: &MIRUnit) -> Self {
        LMIRBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),

            fn_map: mir
                .prototypes
                .iter()
                .map(|proto| (proto.name.to_string(), convert_cx_prototype(proto)))
                .collect(),
            symbol_table: ScopedMap::new_with_starting_scope(),
            liveness_table: ScopedMap::new_with_starting_scope(),
            goto_stack: Vec::new(),

            deconstructors_needed: HashSet::new(),

            function_context: None,
        }
    }

    pub fn new_register(&mut self) -> LMIRRegister {
        let context = self.fun_mut();

        let reg_id = context.register_counter;
        context.register_counter += 1;

        LMIRRegister::new(format!("{}", reg_id))
    }

    pub fn new_function(
        &mut self,
        fn_prototype: MIRFunctionPrototype,
        return_buffer_size: Option<usize>,
    ) {
        assert!(
            self.function_context.is_none(),
            "Attempted to start a new function while another function context is active"
        );

        let bc_prototype = convert_cx_prototype(&fn_prototype);

        if !self.fn_map.contains_key(bc_prototype.name.as_str()) {
            self.insert_fn_prototype(bc_prototype.clone());
        }

        self.function_context = Some(LMIRFunctionContext {
            prototype: bc_prototype,
            mir_prototype: fn_prototype,
            current_block: 0,
            register_counter: 0,
            return_buffer_size,

            blocks: Vec::new(),
        });
        self.push_scope(None, None);
    }

    /// Take the current function context, leaving None in its place.
    /// Used when generating nested functions (like deconstructors).
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
        self.liveness_table.push_scope();
        self.goto_stack.push(LMIRGotoContext {
            continue_block,
            break_block,
        });
    }

    pub fn pop_scope(&mut self) -> CXResult<()> {
        self.deconstruct_at_current_depth()?;
        self.symbol_table.pop_scope();
        self.liveness_table.pop_scope();
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

    pub fn add_deconstructor_request(&mut self, _type: MIRType) {
        let deconstructor_prototype =
            self.convert_cx_prototype(&create_deconstructor_prototype(&_type));

        self.insert_fn_prototype(deconstructor_prototype);
        self.deconstructors_needed.insert(_type);
    }

    pub fn is_deconstructor_pending(&self, _type: &MIRType) -> bool {
        self.deconstructors_needed.contains(_type)
    }

    pub fn pop_deconstructor_request(&mut self) -> Option<MIRType> {
        if let Some(_type) = self.deconstructors_needed.iter().next().cloned() {
            self.deconstructors_needed.remove(&_type);
            Some(_type)
        } else {
            None
        }
    }
    
    pub fn get_deconstructor(&self, _type: &MIRType) -> Option<&LMIRFunctionPrototype> {
        let deconstructor_name = base_mangle_deconstructor(_type);
        self.get_prototype(deconstructor_name.as_str())
    }

    pub fn get_destructor(&self, _type: &MIRType) -> Option<&LMIRFunctionPrototype> {
        let destructor_name = base_mangle_destructor(_type);
        self.get_prototype(destructor_name.as_str())
    }

    pub fn insert_symbol(&mut self, mir_value: CXIdent, bc_value: LMIRValue) {
        self.symbol_table.insert(mir_value.to_string(), bc_value);
    }

    pub fn insert_fn_prototype(&mut self, prototype: LMIRFunctionPrototype) {
        self.fn_map.insert(prototype.name.clone(), prototype);
    }

    pub fn deconstruct_at_depth(&mut self, depth: usize) -> CXResult<()> {
        if self.current_block_closed() { return Ok(()); }
        
        let deconstructed = self
            .liveness_table
            .get_all_at_level(depth)
            .map(|(_, e)| e.clone())
            .collect::<Vec<_>>();

        for entry in deconstructed {
            invoke_conditional_deconstruction(
                self,
                &entry.region_ptr,
                &entry.liveness_ptr,
                &entry.mir_type,
            )?;
        }

        Ok(())
    }

    pub fn deconstruct_at_current_depth(&mut self) -> CXResult<()> {
        let current_depth = self.scope_depth();
        self.deconstruct_at_depth(current_depth)
    }

    #[allow(dead_code)]
    pub fn dump_symbols(&self) {
        todo!()
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

    pub fn add_liveness_mapping(
        &mut self,
        name: String,
        region_ptr: LMIRValue,
        liveness_ptr: LMIRValue,
        mir_type: MIRType,
    ) {
        self.liveness_table.insert(
            name,
            LivenessEntry {
                region_ptr,
                liveness_ptr,
                mir_type,
                scope_depth: self.scope_depth(),
            },
        );
    }

    pub fn get_liveness_mapping(&self, mir_reg: &str) -> Option<&LivenessEntry> {
        self.liveness_table.get(mir_reg)
    }

    pub fn scope_depth(&self) -> usize {
        self.liveness_table.scope_depth()
    }

    pub fn current_mir_prototype(&self) -> &MIRFunctionPrototype {
        &self.fun().mir_prototype
    }

    pub fn current_prototype(&self) -> &LMIRFunctionPrototype {
        &self.fun().prototype
    }

    pub fn get_prototype(&self, name: &str) -> Option<&LMIRFunctionPrototype> {
        self.fn_map.get(name)
    }

    pub fn return_buffer_size(&self) -> Option<usize> {
        self.fun().return_buffer_size
    }

    pub fn get_symbol(&self, name: &CXIdent) -> Option<LMIRValue> {
        self.symbol_table.get(name.as_str()).cloned()
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

    fn current_block_closed(&self) -> bool {
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
        value_type: LMIRType,
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
        value_type: LMIRType,
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

    pub fn get_value_type(&self, value: &LMIRValue) -> LMIRType {
        match value {
            LMIRValue::NULL => LMIRType::unit(),

            LMIRValue::Register { _type, .. } => _type.clone(),

            LMIRValue::FloatImmediate { _type, .. } => LMIRTypeKind::Float(*_type).into(),
            LMIRValue::IntImmediate { _type, .. } => LMIRTypeKind::Integer(*_type).into(),

            LMIRValue::ParameterRef(param_index) => {
                let context = self.fun();
                let param = context
                    .prototype
                    .params
                    .get(*param_index as usize)
                    .expect("Parameter index out of bounds in function prototype");

                param._type.clone()
            }
            LMIRValue::Global(global_index) => {
                let global = self
                    .global_variables
                    .get(*global_index as usize)
                    .expect("Global variable index out of bounds");

                match &global._type {
                    LMIRGlobalType::StringLiteral(..) => LMIRType::default_pointer(),
                    LMIRGlobalType::Variable { _type, .. } => _type.clone(),
                }
            }

            LMIRValue::FunctionRef(_) => LMIRType::default_pointer(),
        }
    }

    pub fn match_int_const(&self, value: i32, _type: &LMIRType) -> LMIRValue {
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
            _type,
        }
    }

    pub fn match_float_const(&self, value: f64, _type: &LMIRType) -> LMIRValue {
        match &_type.kind {
            LMIRTypeKind::Float(_type) => self.float_const(value, *_type),

            _ => panic!("PANIC: Attempted to match float constant with non-float type: {_type}"),
        }
    }

    pub fn float_const(&self, value: f64, _type: LMIRFloatType) -> LMIRValue {
        LMIRValue::FloatImmediate {
            val: FloatWrapper::from(value),
            _type,
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
            fn_map: self.fn_map,
            fn_defs: self.functions,

            global_vars: self.global_variables,
        }
    }
}
