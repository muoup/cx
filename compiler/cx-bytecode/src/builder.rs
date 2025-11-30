use std::collections::HashMap;

use crate::mir_lowering::cx_maps::convert_cx_func_map;
use crate::{BytecodeResult, MIRUnit};
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCType, BCTypeKind};
use cx_bytecode_data::*;
use cx_typechecker_data::ast::TCAST;
use cx_typechecker_data::mir::expression::BCRegister;
use cx_util::format::dump_all;
use cx_util::unsafe_float::FloatWrapper;
use cx_util::CXResult;

#[derive(Debug)]
pub struct MIRBuilder {
    functions: Vec<MIRFunction>,
    global_variables: Vec<BCGlobalValue>,

    pub fn_map: BCFunctionMap,
    symbol_table: HashMap<BCRegister, BCValue>,

    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub struct BytecodeFunctionContext {
    prototype: MIRFunctionPrototype,
    current_block: usize,
    register_counter: u32,

    blocks: Vec<MIRBlock>,
}

impl MIRBuilder {
    pub fn new(ast: &TCAST) -> Self {
        MIRBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),

            fn_map: convert_cx_func_map(&ast.fn_map),
            symbol_table: HashMap::new(),

            function_context: None,
        }
    }

    pub fn new_register(&mut self) -> BCRegister {
        let context = self.fun_mut();

        let reg_id = context.register_counter;
        context.register_counter += 1;

        BCRegister::from(format!("{}", reg_id).as_str())
    }

    pub fn new_function(&mut self, fn_prototype: MIRFunctionPrototype) {
        self.function_context = Some(BytecodeFunctionContext {
            prototype: fn_prototype,
            current_block: 0,
            register_counter: 0,

            blocks: Vec::new(),
        });

        let entry_block = BCBlockID::from("entry");
        self.create_block(entry_block.clone());
        self.set_current_block(entry_block);
    }

    pub fn finish_function(&mut self) {
        let context = self.function_context.take().unwrap();

        self.functions.push(MIRFunction {
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

    pub fn insert_symbol(&mut self, mir_value: BCRegister, bc_value: BCValue) {
        self.symbol_table.insert(mir_value, bc_value);
    }

    pub fn get_symbol(&self, name: &BCRegister) -> Option<BCValue> {
        self.symbol_table.get(name).cloned()
    }

    pub fn add_global_variable(&mut self, value: BCGlobalValue) -> u32 {
        self.global_variables.push(value);

        (self.global_variables.len() - 1) as u32
    }

    fn current_block_closed(&self) -> bool {
        let Some(last_inst) = self.current_block_last_inst() else {
            return false;
        };

        last_inst.kind.is_block_terminating()
    }

    pub fn add_instruction(
        &mut self,
        instruction: BCInstructionKind,
        value_type: BCType,
        result: Option<BCRegister>,
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

    pub fn get_value_type(&self, value: &BCValue) -> Option<BCType> {
        match value {
            BCValue::NULL => Some(BCType::unit()),

            BCValue::Register { _type, .. } => Some(_type.clone()),

            BCValue::LoadOf(type_, _) => Some(type_.clone()),
            BCValue::FloatImmediate { type_, .. } => Some(BCTypeKind::Float(type_.clone()).into()),
            BCValue::IntImmediate { type_, .. } => Some(BCTypeKind::Integer(type_.clone()).into()),

            BCValue::ParameterRef(param_index) => {
                let context = self.fun();
                let param = context.prototype.params.get(*param_index as usize)?;

                Some(param._type.clone())
            }
            BCValue::Global(global_index) => {
                let global = self.global_variables.get(*global_index as usize)?;

                match &global._type {
                    BCGlobalType::StringLiteral(..) => Some(BCType::default_pointer()),
                    BCGlobalType::Variable { _type, .. } => Some(_type.clone()),
                }
            }

            BCValue::FunctionRef(_) => Some(BCType::default_pointer()),
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
            type_: _type.clone(),
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
            type_: _type.clone(),
        }
    }

    pub fn create_block(&mut self, id: BCBlockID) {
        let context = self.fun_mut();

        context.blocks.push(MIRBlock {
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

    pub fn finish(self) -> Option<MIRUnit> {
        Some(MIRUnit {
            fn_map: self.fn_map,
            fn_defs: self.functions,

            global_vars: self.global_variables,
        })
    }

    pub fn load_value(&mut self, ptr: BCValue, _type: BCType) -> Option<BCValue> {
        if _type.is_structure() {
            return Some(ptr);
        }

        Some(BCValue::LoadOf(_type, Box::new(ptr)))
    }
}
