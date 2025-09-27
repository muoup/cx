use crate::cx_maps::convert_cx_func_map;
use crate::instruction_gen::{generate_instruction, implicit_defer_return, implicit_return};
use crate::{BytecodeResult, ProgramBytecode};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::*;
use cx_data_typechecker::ast::{TCExpr, TCAST};
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXType};
use cx_data_typechecker::CXFnMap;
use cx_util::format::dump_all;
use cx_util::log_error;
use cx_util::mangling::{mangle_deconstructor, mangle_destructor};
use cx_util::scoped_map::ScopedMap;
use crate::aux_routines::get_cx_struct_field_by_index;
use crate::deconstructor::deconstruct_variable;

#[derive(Debug)]
pub struct BytecodeBuilder {
    functions: Vec<BytecodeFunction>,

    global_variables: Vec<BCGlobalValue>,

    pub fn_map: BCFunctionMap,
    
    symbol_table: ScopedMap<MIRValue>,
    declaration_scope: Vec<Vec<DeclarationLifetime>>,

    in_deferred_block: bool,
    function_context: Option<BytecodeFunctionContext>
}

#[derive(Debug)]
pub struct BytecodeFunctionContext {
    prototype: BCFunctionPrototype,
    current_block: BlockID,

    merge_stack: Vec<BlockID>,
    continue_stack: Vec<BlockID>,

    blocks: Vec<FunctionBlock>,
    deferred_blocks: Vec<FunctionBlock>,
}

#[derive(Debug, Clone)]
pub struct DeclarationLifetime {
    pub value_id: MIRValue,
    pub _type: CXType
}

impl BytecodeBuilder {
    pub fn new(ast: &TCAST) -> Self {
        BytecodeBuilder {
            functions: Vec::new(),
            global_variables: Vec::new(),

            fn_map: convert_cx_func_map(&ast.fn_map),
            in_deferred_block: false,

            symbol_table: ScopedMap::new(),
            declaration_scope: Vec::new(),

            function_context: None
        }
    }

    pub fn new_function(&mut self, fn_prototype: BCFunctionPrototype) {
        let defers = false;
        
        self.in_deferred_block = false;
        self.function_context = Some(
            BytecodeFunctionContext {
                prototype: fn_prototype,
                current_block: BlockID::Block(0),

                merge_stack: Vec::new(),
                continue_stack: Vec::new(),
                
                blocks: Vec::new(),
                deferred_blocks: Vec::new(),
            }
        );

        let entry_block = self.create_block();
        
        if defers {
            self.setup_deferring_block();
        }
        
        self.set_current_block(
            entry_block
        );
    }

    pub fn finish_function(&mut self) {
        let prototype = self.fun().prototype.clone();
        
        implicit_return(self, &prototype)
            .expect("INTERNAL PANIC: Failed to add implicit return to function");
        implicit_defer_return(self, &prototype)
            .expect("INTERNAL PANIC: Failed to add implicit defer return to function");
        
        let context = self.function_context.take().unwrap();

        self.functions.push(
            BytecodeFunction {
                prototype: context.prototype,
                
                blocks: context.blocks,
                defer_blocks: context.deferred_blocks,
            }
        );
    }
    
    pub fn dump_current_fn(&self) {
        dump_all(self.fun().blocks.iter());
        dump_all(self.fun().deferred_blocks.iter());
    }

    fn fun_mut(&mut self) -> &mut BytecodeFunctionContext {
        self.function_context.as_mut()
            .expect("Attempted to access function context with no current function selected")
    }

    fn fun(&self) -> &BytecodeFunctionContext {
        self.function_context.as_ref()
            .expect("Attempted to access function context with no current function selected")
    }
    
    pub fn push_scope(&mut self) {
        self.symbol_table.push_scope();
        self.declaration_scope.push(Vec::new());
    }

    pub fn pop_scope(&mut self) -> Option<()> {
        self.symbol_table.pop_scope();
        let decls = self.declaration_scope.pop()?;

        for DeclarationLifetime { value_id, _type } in decls.into_iter().rev() {
            deconstruct_variable(self, &value_id, &_type)?;
        }
        Some(())
    }

    pub fn generate_scoped(&mut self, expr: &TCExpr) -> BytecodeResult<MIRValue> {
        self.push_scope();
        let val = generate_instruction(self, expr)?;
        self.pop_scope()?;

        Some(val)
    }

    pub fn insert_declaration(&mut self, declaration: DeclarationLifetime) {
        self.declaration_scope.last_mut()
            .expect("INTERNAL PANIC: Attempted to insert declaration with no current scope")
            .push(declaration);
    }

    pub fn insert_symbol(&mut self, name: String, value_id: MIRValue) {
        self.symbol_table.insert(name, value_id);
    }

    pub fn get_symbol(&self, name: &str) -> Option<MIRValue> {
        self.symbol_table.get(name).cloned()
    }

    pub fn insert_global_symbol(&mut self, value: BCGlobalValue) {
        let key = value.name.to_string();
        self.global_variables.push(value);
        let index = (self.global_variables.len() - 1) as u32;

        self.insert_symbol(key, MIRValue::Global(index));
    }

    pub fn global_symbol_exists(&self, name: &str) -> bool {
        self.symbol_table.get(name).is_some()
    }

    pub fn function_defers(&self) -> bool {
        let ctx = self.function_context.as_ref().unwrap();
        !ctx.deferred_blocks.is_empty()
    }

    pub fn add_instruction(
        &mut self,
        instruction: VirtualInstruction,
        value_type: BCType
    ) -> Option<MIRValue> {
        if self.current_block_closed() {
            return Some(MIRValue::NULL);
        }

        let context = self.fun_mut();
        let current_block = context.current_block;

        let body = match current_block {
            BlockID::Block(id)
                => &mut context.blocks.get_mut(id as usize)?.body,
            BlockID::DeferredBlock(id)
                => &mut context.deferred_blocks.get_mut(id as usize)?.body,

            _ => unreachable!("INTERNAL PANIC: Attempted to add instruction to invalid block {current_block}")
        };

        body.push(BlockInstruction { instruction, value_type });

        Some(
            MIRValue::BlockResult {
                block_id: context.current_block, 
                value_id: (body.len() - 1) as ElementID,
            }
        )
    }

    fn current_block_closed(&self) -> bool {
        let Some(last_inst) = self.current_block_last_inst() else {
            return false;
        };

        last_inst.instruction.is_block_terminating()
    }

    pub fn add_instruction_cxty(
        &mut self,
        instruction: VirtualInstruction,
        value_type: CXType
    ) -> Option<MIRValue> {
        let value_type = self.convert_cx_type(&value_type)?;
        
        self.add_instruction(
            instruction,
            value_type
        )
    }
    
    pub fn fn_ref(&mut self, name: &str) -> BytecodeResult<String> {
        if self.fn_map.contains_key(name) {
            Some(name.to_string())
        } else {
            None
        }
    }
    
    pub(crate) fn add_return(
        &mut self,
        value_id: Option<MIRValue>
    ) -> Option<MIRValue> {
        if self.function_defers() {
            self.add_defer_jump(self.fun().current_block, value_id)
        } else {
            self.add_instruction(
                VirtualInstruction::Return { value: value_id },
                BCType::unit()
            )
        }
    }
    
    pub(crate) fn add_defer_jump(
        &mut self,
        block_id: BlockID,
        value_id: Option<MIRValue>
    ) -> Option<MIRValue> {
        let inst = self.add_instruction(
            VirtualInstruction::GotoDefer,
            BCType::unit()
        )?;

        if let Some(value_id) = value_id {
            self.add_defer_merge(block_id, value_id);
        };
        
        Some(inst)
    }
    
    pub fn add_defer_merge(
        &mut self,
        from_block: BlockID,
        value: MIRValue
    ) {
        let first_defer_inst = &mut self.fun_mut()
            .deferred_blocks
            .first_mut()
            .unwrap()
            .body
            .first_mut()
            .unwrap()
            .instruction;
        
        let VirtualInstruction::Phi { predecessors } = first_defer_inst else {
            let fdi = format!("{first_defer_inst}");
            
            self.dump_current_fn();
            
            panic!("INTERNAL PANIC: Attempted to add defer merge to non-Phi instruction for function {} block {}, first instruction: {}",
                self.fun().prototype.name, self.fun().current_block, fdi);
        };
        
        predecessors.push((value, from_block));
    }

    pub fn match_int_const(&self, value: i32, _type: &BCType) -> MIRValue {
        match _type.kind {
            BCTypeKind::Bool => MIRValue::IntImmediate {
                val: value as i64,
                type_: BCType::from(BCTypeKind::Bool)
            },
            BCTypeKind::Signed { bytes } => self.int_const(value, bytes, true),
            BCTypeKind::Unsigned { bytes } => self.int_const(value, bytes, false),

            _ => panic!("PANIC: Attempted to match integer constant with non-integer type: {}", _type)
        }
    }
    
    pub fn int_const(&self, value: i32, bytes: u8, signed: bool) -> MIRValue {
        MIRValue::IntImmediate {
            val: value as i64,
            type_: match signed {
                true => BCType::from(BCTypeKind::Signed { bytes }),
                false => BCType::from(BCTypeKind::Unsigned { bytes }),
            }
        }
    }

    pub fn match_float_const(&self, value: f64, _type: &BCType) -> MIRValue {
        match _type.kind {
            BCTypeKind::Float { bytes } => self.float_const(value, bytes),

            _ => panic!("PANIC: Attempted to match float constant with non-float type: {}", _type)
        }
    }

    pub fn float_const(&self, value: f64, bytes: u8) -> MIRValue {
        MIRValue::FloatImmediate {
            val: value.to_bits() as i64,
            type_: BCTypeKind::Float { bytes }.into()
        }
    }

    pub fn start_cont_point(&mut self) -> BlockID {
        self.push_scope();
        let cond_block = self.create_block();

        let context = self.fun_mut();
        context.continue_stack.push(cond_block);

        cond_block
    }
    
    pub fn setup_deferring_block(&mut self){
        self.in_deferred_block = true;
        self.set_current_block(BlockID::DeferredBlock(0));
        
        let context = self.fun_mut();
        
        context.deferred_blocks.push(FunctionBlock {
            debug_name: "deferred".to_owned(),
            body: Vec::new()
        });
        
        let return_type = context.prototype.return_type.clone();
        
        if !context.prototype.return_type.is_void() {
            self.add_instruction(
                VirtualInstruction::Phi { predecessors: Vec::new() },
                return_type
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
        
        add_to.push(FunctionBlock {
            debug_name: name.to_string(),
            body: Vec::new()
        });

        match &context.current_block {
            BlockID::Block(_) => BlockID::Block((add_to.len() - 1) as ElementID),
            BlockID::DeferredBlock(_) => BlockID::DeferredBlock((add_to.len() - 1) as ElementID)
        }
    }

    pub fn get_deconstructor(&self, _type: &CXType) -> Option<String> {
        let Some(name) = _type.get_name() else {
            return None;
        };

        let mangled_name = mangle_deconstructor(name);

        if self.fn_map.contains_key(&mangled_name) {
            Some(mangled_name)
        } else {
            None
        }
    }

    pub fn get_destructor(&self, _type: &CXType) -> Option<String> {
        let mangled_name = mangle_destructor(_type.get_name()?);
        
        if self.fn_map.contains_key(&mangled_name) {
            Some(mangled_name)
        } else {
            None
        }
    }

    pub fn last_instruction(&self) -> Option<&BlockInstruction> {
        let context = self.fun();

        context.blocks.last()?.body.last()
    }

    fn current_block_last_inst(&self) -> Option<&BlockInstruction> {
        let context = self.fun();

        let current_block = match context.current_block {
            BlockID::Block(id) => context.blocks.get(id as usize)?,
            BlockID::DeferredBlock(id) => context.deferred_blocks.get(id as usize)?,
            _ => unreachable!("INTERNAL PANIC: Attempted to access last instruction of invalid block")
        };

        current_block.body.last()
    }
    
    pub fn current_function_name(&self) -> Option<&str> {
        self.function_context.as_ref().map(|ctx| ctx.prototype.name.as_str())
    }

    pub fn finish(self) -> Option<ProgramBytecode> {
        Some(
            ProgramBytecode {
                fn_map: self.fn_map,
                fn_defs: self.functions,

                global_vars: self.global_variables,
            }
        )
    }

    // Common helper routines
    pub fn call(&mut self, name: &str, args: Vec<MIRValue>) -> Option<MIRValue> {
        let Some(fn_prototype) = self.fn_map.get(name).cloned() else {
            log_error!("Attempted to call unknown function: {}", name);
        };

        let ret_type = fn_prototype.return_type.clone();

        self.add_instruction(
            VirtualInstruction::DirectCall {
                args,
                method_sig: fn_prototype
            },
            ret_type
        )
    }

    pub fn struct_access(&mut self, val: MIRValue, _type: &BCType, index: usize) -> Option<MIRValue> {
        let BCTypeKind::Struct { .. } = &_type.kind else {
            return None;
        };

        let access = get_cx_struct_field_by_index(self, _type, index)?;

        self.add_instruction(
            VirtualInstruction::StructAccess {
                field_offset: access.offset,
                field_index: access.index,

                struct_: val,
                struct_type: _type.clone()
            },
            access._type.clone()
        )
    }

    pub fn get_tag_addr(&mut self, val: &MIRValue, _type: &BCType) -> Option<MIRValue> {
        self.struct_access(val.clone(), _type, 1)
    }

    pub fn get_tag(&mut self, val: &MIRValue, _type: &BCType) -> Option<MIRValue> {
        let tag_field = self.get_tag_addr(val, _type)?;

        self.load_value(tag_field, BCType::from(BCTypeKind::Unsigned { bytes: 4 }) )
    }

    pub fn set_tag(&mut self, val: &MIRValue, _type: &BCType, tag: u32) -> Option<MIRValue> {
        let tag_val = self.int_const(tag as i32, 4, true);
        let tag_field = self.get_tag_addr(val, _type)?;

        self.add_instruction(
            VirtualInstruction::Store {
                memory: tag_field,
                value: tag_val,

                type_: BCType::from(BCTypeKind::Unsigned { bytes: 4 })
            },
            BCType::unit()
        )
    }

    pub fn load_value(&mut self, ptr: MIRValue, _type: BCType) -> Option<MIRValue> {
        if _type.is_structure() { return Some(ptr); }

        Some(MIRValue::LoadOf(_type, Box::new(ptr)))
    }
}