use std::collections::HashMap;

use crate::mir_lowering::types::convert_cx_prototype;
use crate::{BCUnit, BytecodeResult};
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCType, BCTypeKind};
use cx_bytecode_data::*;
use cx_typechecker_data::mir::expression::MIRRegister;
use cx_typechecker_data::mir::program::MIRUnit;
use cx_util::format::dump_all;
use cx_util::identifier::CXIdent;
use cx_util::unsafe_float::FloatWrapper;
use cx_util::CXResult;

#[derive(Debug)]
pub struct BCBuilder {
    functions: Vec<BCFunction>,
    global_variables: Vec<BCGlobalValue>,

    pub fn_map: BCFunctionMap,
    symbol_table: HashMap<MIRRegister, BCValue>,

    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub struct BytecodeFunctionContext {
    prototype: BCFunctionPrototype,
    current_block: usize,
    register_counter: u32,

    blocks: Vec<BCBasicBlock>,
}

impl BCBuilder {
    pub fn new(mir: &MIRUnit) -> Self {
        BCBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),

            fn_map: mir
                .prototypes
                .iter()
                .map(|proto| (proto.name.mangle(), convert_cx_prototype(proto)))
                .collect(),
            symbol_table: HashMap::new(),

            function_context: None,
        }
    }

    pub fn new_register(&mut self) -> BCRegister {
        let context = self.fun_mut();

        let reg_id = context.register_counter;
        context.register_counter += 1;

        BCRegister::new(format!("{}", reg_id))
    }

    pub fn new_function(&mut self, fn_prototype: BCFunctionPrototype) {
        self.function_context = Some(BytecodeFunctionContext {
            prototype: fn_prototype,
            current_block: 0,
            register_counter: 0,

            blocks: Vec::new(),
        });

        self.symbol_table.clear();
    }

    pub fn finish_function(&mut self) {
        let context = self.function_context.take().unwrap();

        self.functions.push(BCFunction {
            prototype: context.prototype,
            blocks: context.blocks,
        });
    }

    pub fn dump_current_fn(&self) {
        dump_all(self.fun().blocks.iter());
    }

    fn fun_mut(&mut self) -> &mut BytecodeFunctionContext {
        self.function_context
            .as_mut()
            .expect("Attempted to access function context with no current function selected")
    }

    fn fun(&self) -> &BytecodeFunctionContext {
        self.function_context
            .as_ref()
            .expect("Attempted to access function context with no current function selected")
    }

    pub fn insert_symbol(&mut self, mir_value: MIRRegister, bc_value: BCValue) {
        self.symbol_table.insert(mir_value, bc_value);
    }

    #[allow(dead_code)]
    pub fn dump_symbols(&self) {
        println!("--- Symbol Table ---");
        for (key, value) in self.symbol_table.iter() {
            println!("{} => {}", key, value);
        }
    }

    pub fn get_global_symbol(&self, name: &str) -> Option<BCValue> {
        for (index, global_var) in self.global_variables.iter().enumerate() {
            if global_var.name.as_str() == name {
                return Some(BCValue::Global(index as u32));
            }
        }

        None
    }

    pub fn get_prototype(&self, name: &str) -> Option<&BCFunctionPrototype> {
        self.fn_map.get(name)
    }

    pub fn get_symbol(&self, name: &MIRRegister) -> Option<BCValue> {
        self.symbol_table.get(name).cloned()
    }

    pub fn add_global_variable(&mut self, value: BCGlobalValue) -> u32 {
        self.global_variables.push(value);

        (self.global_variables.len() - 1) as u32
    }

    pub fn create_static_string(&mut self, value: String) -> BCValue {
        let global_index = self.global_variables.len() as u32;

        self.global_variables.push(BCGlobalValue {
            name: CXIdent::from(format!("str_{}", global_index)),
            _type: BCGlobalType::StringLiteral(value),
            linkage: LinkageType::Static,
        });

        BCValue::Global(global_index)
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
        instruction: BCInstructionKind,
        value_type: BCType,
        result_expected: bool,
    ) -> CXResult<BCValue> {
        if self.current_block_closed() {
            return Ok(BCValue::NULL);
        }

        let result = if result_expected {
            Some(self.new_register())
        } else {
            None
        };

        let context = self.fun_mut();
        let current_block = context.current_block;

        context.blocks[current_block].body.push(BCInstruction {
            kind: instruction,
            value_type: value_type.clone(),
            result: result.clone(),
        });

        match result {
            Some(reg) => Ok(BCValue::Register {
                register: reg,
                _type: value_type,
            }),
            None => Ok(BCValue::NULL),
        }
    }

    pub fn add_instruction_translated(
        &mut self,
        instruction: BCInstructionKind,
        value_type: BCType,
        result: Option<MIRRegister>,
    ) -> CXResult<BCValue> {
        if self.current_block_closed() {
            return Ok(BCValue::NULL);
        }

        let (result, result_val) = if let Some(result) = result.clone() {
            let bc_result = self.new_register();
            let bc_result_val = BCValue::Register {
                register: bc_result.clone(),
                _type: value_type.clone(),
            };

            self.insert_symbol(result, bc_result_val.clone());
            (Some(bc_result), bc_result_val)
        } else {
            (None, BCValue::NULL)
        };

        let context = self.fun_mut();
        let current_block = context.current_block;

        context.blocks[current_block].body.push(BCInstruction {
            kind: instruction,
            value_type,
            result: result.clone(),
        });

        Ok(result_val)
    }

    pub fn fn_ref(&mut self, name: &str) -> BytecodeResult<String> {
        if self.fn_map.contains_key(name) {
            Some(name.to_string())
        } else {
            None
        }
    }

    pub fn get_value_type(&self, value: &BCValue) -> BCType {
        match value {
            BCValue::NULL => BCType::unit(),

            BCValue::Register { _type, .. } => _type.clone(),

            BCValue::FloatImmediate { _type, .. } => BCTypeKind::Float(_type.clone()).into(),
            BCValue::IntImmediate { _type, .. } => BCTypeKind::Integer(_type.clone()).into(),
            BCValue::BoolImmediate { .. } => BCType::from(BCTypeKind::Bool),

            BCValue::ParameterRef(param_index) => {
                let context = self.fun();
                let param = context
                    .prototype
                    .params
                    .get(*param_index as usize)
                    .expect("Parameter index out of bounds in function prototype");

                param._type.clone()
            }
            BCValue::Global(global_index) => {
                let global = self
                    .global_variables
                    .get(*global_index as usize)
                    .expect("Global variable index out of bounds");

                match &global._type {
                    BCGlobalType::StringLiteral(..) => BCType::default_pointer(),
                    BCGlobalType::Variable { _type, .. } => _type.clone(),
                }
            }

            BCValue::FunctionRef(_) => BCType::default_pointer(),
        }
    }

    pub fn match_int_const(&self, value: i32, _type: &BCType) -> BCValue {
        match &_type.kind {
            BCTypeKind::Integer(_type) => self.int_const(value, _type.clone()),

            _ => {
                panic!("PANIC: Attempted to match integer constant with non-integer type: {_type}")
            }
        }
    }

    pub fn int_const(&self, value: i32, _type: BCIntegerType) -> BCValue {
        BCValue::IntImmediate {
            val: value as i64,
            _type: _type.clone(),
        }
    }

    pub fn match_float_const(&self, value: f64, _type: &BCType) -> BCValue {
        match &_type.kind {
            BCTypeKind::Float(_type) => self.float_const(value, _type.clone()),

            _ => panic!("PANIC: Attempted to match float constant with non-float type: {_type}"),
        }
    }

    pub fn float_const(&self, value: f64, _type: BCFloatType) -> BCValue {
        BCValue::FloatImmediate {
            val: FloatWrapper::from(value),
            _type: _type.clone(),
        }
    }

    pub fn create_block(&mut self, id: BCBlockID) {
        let context = self.fun_mut();

        context.blocks.push(BCBasicBlock {
            id,
            body: Vec::new(),
        });
    }

    pub fn set_current_block(&mut self, block: BCBlockID) {
        let fun = self.fun();

        let block_id = fun.blocks.iter().position(|b| b.id == block);

        self.fun_mut().current_block = block_id.expect("Block ID not found in function blocks");
    }

    pub fn current_block(&self) -> BCBlockID {
        let fun = self.fun();

        fun.blocks[fun.current_block].id.clone()
    }

    pub fn last_instruction(&self) -> Option<&BCInstruction> {
        let context = self.fun();

        context.blocks.last()?.body.last()
    }

    pub fn current_block_last_inst(&self) -> Option<&BCInstruction> {
        let context = self.fun();

        context.blocks.get(context.current_block)?.body.last()
    }

    pub fn current_function_name(&self) -> Option<&str> {
        self.function_context
            .as_ref()
            .map(|ctx| ctx.prototype.name.as_str())
    }

    pub fn finish(self) -> BCUnit {
        BCUnit {
            fn_map: self.fn_map,
            fn_defs: self.functions,

            global_vars: self.global_variables,
        }
    }
}
