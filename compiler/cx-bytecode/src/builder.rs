use std::collections::{HashMap, HashSet};

use crate::mir_lowering::cx_maps::convert_cx_func_map;
use crate::{BytecodeResult, MIRUnit};
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCTypeKind, BCType};
use cx_bytecode_data::*;
use cx_typechecker_data::ast::TCAST;
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_typechecker_data::mir::expression::MIRRegister;
use cx_typechecker_data::mir::types::{CXIntegerType, CXType};
use cx_util::format::dump_all;
use cx_util::{CXResult, log_error};
use cx_util::unsafe_float::FloatWrapper;

#[derive(Debug)]
pub struct MIRBuilder {
    functions: Vec<MIRFunction>,

    global_variables: Vec<BCGlobalValue>,

    pub fn_map: BCFunctionMap,

    symbol_table: HashMap<MIRRegister, BCValue>,
    declaration_scope: Vec<Vec<DeclarationLifetime>>,
    pub(crate) defined_deconstructors: HashSet<CXType>,

    in_deferred_block: bool,
    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub struct BytecodeFunctionContext {
    prototype: MIRFunctionPrototype,
    current_block: BlockID,

    merge_stack: Vec<BlockID>,
    continue_stack: Vec<BlockID>,

    blocks: Vec<MIRBlock>,
    deferred_blocks: Vec<MIRBlock>,
}

#[derive(Debug, Clone)]
pub struct DeclarationLifetime {
    pub value_id: BCValue,
    pub _type: CXType,
}

impl MIRBuilder {
    pub fn new(ast: &TCAST) -> Self {
        MIRBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),

            fn_map: convert_cx_func_map(&ast.fn_map),
            in_deferred_block: false,

            symbol_table: HashMap::new(),
            declaration_scope: Vec::new(),
            defined_deconstructors: HashSet::new(),

            function_context: None,
        }
    }

    pub fn new_function(&mut self, fn_prototype: MIRFunctionPrototype) {
        let defers = false;

        self.in_deferred_block = false;
        self.function_context = Some(BytecodeFunctionContext {
            prototype: fn_prototype,
            current_block: BlockID::Block(0),

            merge_stack: Vec::new(),
            continue_stack: Vec::new(),

            blocks: Vec::new(),
            deferred_blocks: Vec::new(),
        });

        let entry_block = self.create_block();

        if defers {
            self.setup_deferring_block();
        }

        self.set_current_block(entry_block);
    }

    pub fn finish_function(&mut self) {
        let context = self.function_context.take().unwrap();

        self.functions.push(MIRFunction {
            prototype: context.prototype,

            blocks: context.blocks,
            defer_blocks: context.deferred_blocks,
        });
    }

    pub fn dump_current_fn(&self) {
        dump_all(self.fun().blocks.iter());
        dump_all(self.fun().deferred_blocks.iter());
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

    pub fn get_symbol(&self, name: &MIRRegister) -> Option<BCValue> {
        self.symbol_table.get(name).cloned()
    }
    
    pub fn add_global_variable(&mut self, value: BCGlobalValue) -> u32 {       
        self.global_variables.push(value);
        
        (self.global_variables.len() - 1) as u32
    }

    pub fn function_defers(&self) -> bool {
        let ctx = self.function_context.as_ref().unwrap();
        !ctx.deferred_blocks.is_empty()
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
        result: Option<MIRRegister>
    ) -> CXResult<BCValue> {
        if self.current_block_closed() {
            return Ok(BCValue::NULL);
        }

        let context = self.fun_mut();
        let current_block = context.current_block;

        let body = match current_block {
            BlockID::Block(id) => &mut context.blocks.get_mut(id as usize)?.body,
            BlockID::DeferredBlock(id) => &mut context.deferred_blocks.get_mut(id as usize)?.body,
        };

        body.push(BCInstruction {
            kind: instruction,
            value_type,
        });

        let val = BCValue::BlockResult {
            block_id: context.current_block,
            value_id: (body.len() - 1) as ElementID,
        };
        
        if let Some(result) = result.clone() {
            self.insert_symbol(result, val.clone());
        }
        
        Ok(val)
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
                       
            BCValue::LoadOf(type_, _) => Some(type_.clone()),
            BCValue::FloatImmediate { type_, .. } => Some(BCTypeKind::Float(type_.clone()).into()),
            BCValue::IntImmediate { type_, .. } => Some(BCTypeKind::Integer(type_.clone()).into()),
            
            BCValue::ParameterRef(param_index) => {
                let context = self.fun();
                let param = context.prototype.params.get(*param_index as usize)?;

                Some(param._type.clone())
            },
            BCValue::Global(global_index) => {
                let global = self.global_variables.get(*global_index as usize)?;

                match &global._type {
                    BCGlobalType::StringLiteral(..) => Some(BCType::default_pointer()),
                    BCGlobalType::Variable { _type, .. } => Some(_type.clone()),
                }
            },
            
            BCValue::FunctionRef(_) => Some(BCType::default_pointer()),
            BCValue::BlockResult { block_id, value_id } => {
                let context = self.fun();

                let block = match block_id {
                    BlockID::Block(id) => context.blocks.get(*id as usize)?,
                    BlockID::DeferredBlock(id) => context.deferred_blocks.get(*id as usize)?,
                };

                let instruction = block.body.get(*value_id as usize)?;

                Some(instruction.value_type.clone())
            },
        }
    }

    pub fn match_int_const(&self, value: i32, _type: &BCType) -> BCValue {
        match _type.kind {
            BCTypeKind::Bool => BCValue::IntImmediate {
                val: value as i64,
                type_: BCType::from(BCTypeKind::Bool),
            },
            BCTypeKind::Integer(_type) => self.int_const(value, _type),

            _ => {
                panic!("PANIC: Attempted to match integer constant with non-integer type: {_type}")
            }
        }
    }

    pub fn int_const(&self, value: i32, _type: BCIntegerType) -> BCValue {
        BCValue::IntImmediate {
            val: value as i64,
            type_: _type.clone()
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
            type_: _type.clone()
        }
    }

    pub fn start_cont_point(&mut self) -> BlockID {
        self.push_scope();
        let cond_block = self.create_block();

        let context = self.fun_mut();
        context.continue_stack.push(cond_block);

        cond_block
    }

    pub fn setup_deferring_block(&mut self) {
        self.in_deferred_block = true;
        self.set_current_block(BlockID::DeferredBlock(0));

        let context = self.fun_mut();

        context.deferred_blocks.push(MIRBlock {
            debug_name: "deferred".to_owned(),
            body: Vec::new(),
        });

        let return_type = context.prototype.return_type.clone();

        if !context.prototype.return_type.is_void() {
            self.add_instruction(
                BCInstructionKind::Phi {
                    predecessors: Vec::new(),
                },
                return_type,
            );
        }

        self.in_deferred_block = false;
    }

    pub fn enter_deferred_logic(&mut self) {
        if !self.function_defers() {
            self.setup_deferring_block();
        }

        self.in_deferred_block = true;

        let context = self.fun_mut();
        let last_block = BlockID::DeferredBlock(context.deferred_blocks.len() as ElementID - 1);

        self.set_current_block(last_block);
    }

    pub fn exit_deferred_logic(&mut self) {
        self.in_deferred_block = false;

        let context = self.fun_mut();
        let last_block = BlockID::Block(context.blocks.len() as ElementID - 1);

        self.set_current_block(last_block);
    }

    pub fn start_scope(&mut self) -> BlockID {
        self.push_scope();
        let merge_block = self.create_named_block("merge");

        let context = self.fun_mut();
        context.merge_stack.push(merge_block);

        merge_block
    }

    pub fn get_merge(&mut self) -> Option<BlockID> {
        let context = self.fun_mut();

        context.merge_stack.last().cloned()
    }

    pub fn get_continue(&mut self) -> Option<BlockID> {
        let context = self.fun_mut();

        context.continue_stack.last().cloned()
    }

    pub fn end_scope(&mut self) {
        self.pop_scope();
        let context = self.fun_mut();

        let merge_block = context.merge_stack.pop().unwrap();
        context.current_block = merge_block;
    }

    pub fn end_cond(&mut self) {
        self.pop_scope();
        let context = self.fun_mut();

        context.continue_stack.pop().unwrap();
    }

    pub fn set_current_block(&mut self, block: BlockID) {
        self.fun_mut().current_block = block;
    }

    pub fn current_block(&self) -> BlockID {
        self.fun().current_block
    }

    pub fn create_block(&mut self) -> BlockID {
        self.create_named_block("")
    }

    pub fn create_named_block(&mut self, name: &str) -> BlockID {
        let context = self.fun_mut();

        let add_to = match &context.current_block {
            BlockID::Block(_) => &mut context.blocks,
            BlockID::DeferredBlock(_) => &mut context.deferred_blocks,
        };

        add_to.push(MIRBlock {
            debug_name: name.to_string(),
            body: Vec::new(),
        });

        match &context.current_block {
            BlockID::Block(_) => BlockID::Block((add_to.len() - 1) as ElementID),
            BlockID::DeferredBlock(_) => BlockID::DeferredBlock((add_to.len() - 1) as ElementID),
        }
    }

    pub fn get_deconstructor(&self, _type: &CXType) -> Option<MIRFunctionPrototype> {
        if self.defined_deconstructors.contains(_type) {
            deconstructor_prototype(_type)
        } else {
            None
        }
    }

    pub fn get_destructor(&self, _type: &CXType) -> Option<String> {
        let Some(mangled_name) = CXFunctionKind::destructor_mangle_ty(_type)
            else { return None; };

        if self.fn_map.contains_key(&mangled_name) {
            Some(mangled_name)
        } else {
            None
        }
    }

    pub fn last_instruction(&self) -> Option<&BCInstruction> {
        let context = self.fun();

        context.blocks.last()?.body.last()
    }

    pub fn current_block_last_inst(&self) -> Option<&BCInstruction> {
        let context = self.fun();

        let current_block = match context.current_block {
            BlockID::Block(id) => context.blocks.get(id as usize)?,
            BlockID::DeferredBlock(id) => context.deferred_blocks.get(id as usize)?,
        };

        current_block.body.last()
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

    // Common helper routines
    pub fn call(&mut self, name: &str, args: Vec<BCValue>) -> Option<BCValue> {
        let Some(fn_prototype) = self.fn_map.get(name).cloned() else {
            log_error!("Attempted to call unknown function: {}", name);
        };

        Self::call_proto(self, fn_prototype, args)
    }
    
    pub fn call_proto(&mut self, prototype: MIRFunctionPrototype, args: Vec<BCValue>) -> Option<BCValue> {
        let ret_type = prototype.return_type.clone();

        self.add_instruction(
            BCInstructionKind::DirectCall {
                args,
                method_sig: prototype,
            },
            ret_type,
        )
    }

    pub fn struct_access(
        &mut self,
        val: BCValue,
        _type: &BCType,
        index: usize,
    ) -> Option<BCValue> {
        let BCTypeKind::Struct { .. } = &_type.kind else {
            return None;
        };

        let access = get_cx_struct_field_by_index(self, _type, index)?;

        self.add_instruction(
            BCInstructionKind::StructAccess {
                field_offset: access.offset,
                field_index: access.index,

                struct_: val,
                struct_type: _type.clone(),
            },
            access._type.clone(),
        )
    }

    pub fn get_tag_addr(&mut self, val: &BCValue, _type: &BCType) -> Option<BCValue> {
        self.struct_access(val.clone(), _type, 1)
    }

    pub fn get_tag(&mut self, val: &BCValue, _type: &BCType) -> Option<BCValue> {
        let tag_field = self.get_tag_addr(val, _type)?;

        self.load_value(tag_field, BCType::from(BCTypeKind::Integer { bytes: 4 }))
    }

    pub fn set_tag(&mut self, val: &BCValue, _type: &BCType, tag: u32) -> Option<BCValue> {
        let tag_val = self.int_const(tag as i32, 4, true);
        let tag_field = self.get_tag_addr(val, _type)?;

        self.add_instruction(
            BCInstructionKind::Store {
                memory: tag_field,
                value: tag_val,

                type_: BCType::from(BCTypeKind::Integer { bytes: 4 }),
            },
            BCType::unit(),
        )
    }

    pub fn load_value(&mut self, ptr: BCValue, _type: BCType) -> Option<BCValue> {
        if _type.is_structure() {
            return Some(ptr);
        }

        Some(BCValue::LoadOf(_type, Box::new(ptr)))
    }
}
