use crate::{BytecodeResult, ProgramBytecode};
use cx_data_ast::parse::ast::{CXExpr};
use cx_data_typechecker::cx_types::CXType;
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::*;
use cx_data_typechecker::ast::TCExpr;
use cx_data_typechecker::{CXFnData, CXTypeData};
use cx_util::format::dump_all;
use cx_util::log_error;
use cx_util::mangling::mangle_destructor;
use cx_util::scoped_map::ScopedMap;
use crate::aux_routines::deconstruct_scope;
use crate::cx_maps::convert_cx_func_map;
use crate::instruction_gen::{generate_instruction, implicit_defer_return, implicit_return};

#[derive(Debug)]
pub struct BytecodeBuilder {
    global_strings: Vec<String>,
    functions: Vec<BytecodeFunction>,
    
    pub cx_type_map: CXTypeData,
    pub cx_function_map: CXFnData,

    pub fn_map: BCFunctionMap,
    
    pub symbol_table: ScopedMap<ValueID>,
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
    pub value_id: ValueID,
    pub _type: CXType
}

impl BytecodeBuilder {
    pub fn new(type_map: CXTypeData, fn_map: CXFnData) -> Self {
        BytecodeBuilder {
            global_strings: Vec::new(),
            functions: Vec::new(),

            fn_map: convert_cx_func_map(&fn_map),

            cx_type_map: type_map,
            cx_function_map: fn_map,
            
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
                current_block: BlockID {
                    in_deferral: false,
                    id: 0,
                },
                
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
        let decls = self.declaration_scope.pop();

        deconstruct_scope(self, decls?.as_slice())
    }

    pub fn generate_scoped(&mut self, expr: &TCExpr) -> BytecodeResult<ValueID> {
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

    pub fn insert_symbol(&mut self, name: String, value_id: ValueID) {
        self.symbol_table.insert(name, value_id);
    }

    pub fn get_symbol(&self, name: &str) -> Option<ValueID> {
        self.symbol_table.get(name).cloned()
    }

    pub fn function_defers(&self) -> bool {
        let ctx = self.function_context.as_ref().unwrap();
        !ctx.deferred_blocks.is_empty()
    }
    
    pub fn add_instruction(
        &mut self,
        instruction: VirtualInstruction,
        value_type: BCType
    ) -> Option<ValueID> {
        let in_deferred_block = self.in_deferred_block;
        let context = self.fun_mut();
        let current_block = context.current_block;

        let body = if in_deferred_block {
            &mut context.deferred_blocks[current_block.id as usize].body
        } else {
            &mut context.blocks[current_block.id as usize].body
        };

        body.push(BlockInstruction {
            instruction,
            value: VirtualValue {
                type_: value_type
            }
        });

        Some(
            ValueID {
                block_id: context.current_block, 
                value_id: (body.len() - 1) as ElementID,
            }
        )
    }

    pub fn add_instruction_cxty(
        &mut self,
        instruction: VirtualInstruction,
        value_type: CXType
    ) -> Option<ValueID> {
        let value_type = self.convert_cx_type(&value_type)?;
        
        self.add_instruction(
            instruction,
            value_type
        )
    }
    
    pub fn fn_ref_unchecked(
        &mut self, name: &str
    ) -> BytecodeResult<ValueID> {
        self.add_instruction(
            VirtualInstruction::FunctionReference { name: name.to_owned() },
            BCType::default_pointer()
        )
    }
    
    pub fn fn_ref(&mut self, name: &str) -> BytecodeResult<Option<ValueID>> {
        if self.fn_map.contains_key(name) {
            self.fn_ref_unchecked(name).map(Some)
        } else {
            Some(None)
        }
    }
    
    pub(crate) fn add_return(
        &mut self,
        value_id: Option<ValueID>
    ) -> Option<ValueID> {
        if self.function_defers() {
            self.add_defer_jump(self.fun().current_block, value_id)
        } else {
            let return_block = self.create_named_block("return");
            
            self.add_instruction(
                VirtualInstruction::Jump { target: return_block },
                BCType::unit()
            );
            
            self.set_current_block(return_block);
            
            self.add_instruction(
                VirtualInstruction::Return { value: value_id },
                BCType::unit()
            )
        }
    }
    
    pub(crate) fn add_defer_jump(
        &mut self,
        block_id: BlockID,
        value_id: Option<ValueID>
    ) -> Option<ValueID> {
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
        value: ValueID
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
    
    pub fn bool_const(
        &mut self, value: bool
    ) -> Option<ValueID> {
        self.add_instruction(
            VirtualInstruction::Immediate { value: if value { 1 } else { 0 } },
            BCType::from(BCTypeKind::Bool)
        )
    }
    
    pub fn int_const_match(
        &mut self, value: i32, bc_type: &BCType
    ) -> Option<ValueID> {
        match bc_type.kind {
            BCTypeKind::Signed { bytes } =>
                self.int_const(value, bytes, true),
            BCTypeKind::Unsigned { bytes } =>
                self.int_const(value, bytes, false),
            BCTypeKind::Bool =>
                self.bool_const(value != 0),
            
            _ => panic!("INTERNAL PANIC: Attempted to create integer constant with non-integer type: {bc_type:?}")
        }
    }
    
    pub fn int_const(
        &mut self,
        value: i32,
        bytes: u8,
        signed: bool
    ) -> Option<ValueID> {
        let value_type = BCType::from(
            match signed {
                true => BCTypeKind::Signed { bytes },
                false => BCTypeKind::Unsigned { bytes },
            }
        );

        self.add_instruction(
            VirtualInstruction::Immediate { value },
            value_type
        )
    }

    pub fn get_variable(&self, value_id: ValueID) -> Option<&VirtualValue> {
        let block = if self.in_deferred_block {
            &self.fun().deferred_blocks
        } else {
            &self.fun().blocks
        };
        
        Some(
            &block
                .get(value_id.block_id.id as usize)?
                .body
                .get(value_id.value_id as usize)?
                .value
        )
    }

    pub fn get_type(&self, value_id: ValueID) -> Option<&BCType> {
        let Some(value) = self.get_variable(value_id) else {
            panic!("INTERNAL PANIC: Failed to get variable for value id: {value_id:?}");
        };
        
        Some(&value.type_)
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
        self.set_current_block(BlockID { in_deferral: true, id: 0 });
        
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
        let last_block = BlockID {
            in_deferral: true,
            id: context.deferred_blocks.len() as ElementID - 1
        };
        
        self.set_current_block(last_block);
    }
    
    pub fn exit_deferred_logic(&mut self) {
        self.in_deferred_block = false;

        let context = self.fun_mut();
        let last_block = BlockID {
            in_deferral: false,
            id: context.blocks.len() as ElementID - 1
        };
        
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
        self.in_deferred_block = block.in_deferral;
    }

    pub fn create_global_string(&mut self, string: String) -> u32 {
        self.global_strings.push(string.clone());
        self.global_strings.len() as u32 - 1
    }
    
    pub fn current_block(&self) -> BlockID {
        self.fun().current_block
    }

    pub fn create_block(&mut self) -> BlockID {
        self.create_named_block("")
    }
    
    pub fn create_named_block(&mut self, name: &str) -> BlockID {
        let in_deferred_block = self.in_deferred_block;
        let context = self.fun_mut();

        let add_to = if in_deferred_block {
            &mut context.deferred_blocks
        } else {
            &mut context.blocks
        };
        
        add_to.push(FunctionBlock {
            debug_name: "".to_owned(),
            body: Vec::new()
        });

        BlockID {
            in_deferral: in_deferred_block,
            id: (add_to.len() - 1) as ElementID
        }
    }
    
    pub fn get_destructor(&self, _type: &CXType) -> Option<String> {
        let mangled_name = mangle_destructor(_type.get_name()?);
        
        if self.cx_function_map.standard.contains_key(&mangled_name) {
            Some(mangled_name)
        } else {
            None
        }
    }

    pub fn last_instruction(&self) -> Option<&BlockInstruction> {
        let context = self.fun();

        context.blocks.last()?.body.last()
    }
    
    pub fn current_function_name(&self) -> Option<&str> {
        self.function_context.as_ref().map(|ctx| ctx.prototype.name.as_str())
    }

    pub fn finish(self) -> Option<ProgramBytecode> {
        Some(
            ProgramBytecode {
                fn_map: self.fn_map,

                global_strs: self.global_strings,
                fn_defs: self.functions
            }
        )
    }
}