use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRExpressionKind, MIRRegister, MIRValue},
    program::{MIRBasicBlock, MIRFunction},
    types::{MIRFunctionPrototype, MIRType},
};
use cx_util::identifier::CXIdent;

use crate::{environment::DEFER_ACCUMULATION_REGISTER, type_checking::move_semantics::{acknowledge_destructed_object, invoke_remaining_destructions}};

pub(crate) struct MIRBuilder {
    pub generated_functions: Vec<MIRFunction>,
    pub function_context: Option<MIRFunctionContext>,
}

#[derive(Clone, Debug)]
pub enum BlockPointer {
    Standard(usize),
    Defer(usize),
}

#[derive(Clone, Debug)]
pub(crate) struct Lifetime {
    pub name: String,
    pub _type: MIRType,
}

pub(crate) struct MIRFunctionContext {
    pub current_prototype: MIRFunctionPrototype,
    
    pub standard_blocks: Vec<MIRBasicBlock>,
    pub defer_blocks: Vec<MIRBasicBlock>,

    pub lifetime_stack: Vec<Vec<Lifetime>>,
    
    pub current_block: BlockPointer,
    pub defer_last_pointer: usize,

    pub temp_register_counter: usize,
    pub temp_block_counter: usize,
}

impl MIRBuilder {
    pub fn new() -> Self {
        MIRBuilder {
            generated_functions: Vec::new(),
            function_context: None,
        }
    }

    pub fn start_function(&mut self, prototype: MIRFunctionPrototype) {
        let function_context = MIRFunctionContext {
            current_prototype: prototype,
            standard_blocks: vec![MIRBasicBlock {
                id: CXIdent::new("entry"),
                instructions: Vec::new(),
            }],
            defer_blocks: Vec::new(),
            
            lifetime_stack: Vec::new(),

            current_block: BlockPointer::Standard(0),
            defer_last_pointer: 0,

            temp_register_counter: 0,
            temp_block_counter: 0,
        };

        self.function_context = Some(function_context);
        self.push_scope();
    }

    pub fn new_register(&mut self) -> MIRRegister {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let id = format!("{}", func_ctx.temp_register_counter);
        func_ctx.temp_register_counter += 1;
        MIRRegister::new(id)
    }
    
    pub fn current_block(&self) -> &MIRBasicBlock {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        match &func_ctx.current_block {
            BlockPointer::Standard(index) => &func_ctx.standard_blocks[*index],
            BlockPointer::Defer(index) => &func_ctx.defer_blocks[*index],
        }
    }

    pub fn current_block_mut(&mut self) -> &mut MIRBasicBlock {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        match &func_ctx.current_block {
            BlockPointer::Standard(index) => &mut func_ctx.standard_blocks[*index],
            BlockPointer::Defer(index) => &mut func_ctx.defer_blocks[*index],
        }
    }
    
    pub fn current_block_closed(&self) -> bool {
        let block = self.current_block();
        let last_instruction = block.instructions.last();
        
        last_instruction
            .map(|instr| instr.is_block_terminator())
            .unwrap_or(false)
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction) {
        self.current_block_mut().instructions.push(instruction);
    }
    
    pub fn lifetime_stack_ref(&self) -> &[Vec<Lifetime>] {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        &func_ctx.lifetime_stack
    }
    
    pub fn add_lifetime(&mut self, lifetime: Lifetime) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let current_scope = func_ctx.lifetime_stack.last_mut().unwrap();
        current_scope.push(lifetime);
    }
    
    pub fn push_scope(&mut self) {
        let Some(func_ctx) = &mut self.function_context else {
            return;
        };
        
        func_ctx.lifetime_stack.push(Vec::new());
    }
    
    pub fn pop_scope(&mut self) {
        let Some(func_ctx) = &mut self.function_context else {
            return;
        };
        
        let scope = func_ctx.lifetime_stack.pop().unwrap();
        
        for lifetime in scope.into_iter().rev() {
            acknowledge_destructed_object(self, lifetime);
        }
    }

    pub fn add_jump(&mut self, target_block: CXIdent) {
        self.add_instruction(MIRInstruction::Jump {
            target: target_block,
        });
    }
    
    pub fn add_return(&mut self, value: Option<MIRValue>) {
        if self.in_defer() {
            if value.is_some() {
                // Case 1: The return block has a return value, this overrides the defer accumulation register
                // FIXME: We might need to handle cleaning up the defer accumulation register here
                invoke_remaining_destructions(self);
                self.add_instruction(MIRInstruction::Return {
                    value,
                });
            } else {
                let value = MIRValue::Register {
                    register: MIRRegister::new(DEFER_ACCUMULATION_REGISTER),
                    _type: self.current_prototype().return_type.clone(),
                };
                
                invoke_remaining_destructions(self);
                self.add_instruction(MIRInstruction::Return {
                    value: Some(value),
                });
            }
        } else if let Some(defer_start) = self.get_defer_start() {
            self.add_instruction(MIRInstruction::Jump {
                target: defer_start,
            });
            
            let current_block = self.current_block().id.clone();
            
            if let Some(value) = value {
                let Some(func_ctx) = &mut self.function_context else {
                    unreachable!()
                };
                
                let return_acc = func_ctx
                    .defer_blocks
                    .first_mut()
                    .unwrap()
                    .instructions
                    .first_mut()
                    .unwrap();
                
                let MIRInstruction::Phi { predecessors, .. } = return_acc else {
                    unreachable!()
                };
                
                predecessors.push((value, current_block));
            }
        } else {
            invoke_remaining_destructions(self);
            self.add_instruction(MIRInstruction::Return {
                value,
            });
        }
    }

    pub fn new_block_id(&mut self) -> CXIdent {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let id = func_ctx.temp_block_counter;
        func_ctx.temp_block_counter += 1;
        
        CXIdent::new(format!("block_{}", id))
    }
    
    pub fn new_named_block_id(&mut self, name: &str) -> CXIdent {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };
        
        let id = func_ctx.temp_block_counter;
        func_ctx.temp_block_counter += 1;
        
        CXIdent::new(format!("block_{}_{}", id, name))
    }
    
    pub fn get_defer_start(&self) -> Option<CXIdent> {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };
        
        func_ctx.defer_blocks.first()
            .map(|b| b.id.clone())
    }
    
    pub fn get_defer_end(&self) -> usize {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        func_ctx.defer_last_pointer
    }
    
    pub fn set_defer_end(&mut self, id: usize) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        func_ctx.defer_last_pointer = id;
    }

    pub fn add_and_set_block(&mut self, block_id: CXIdent) {
        let block_index = self.add_block(block_id.clone());

        self.function_context.as_mut().unwrap().current_block = block_index;
    }

    pub fn add_block(&mut self, block_id: CXIdent) -> BlockPointer {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        match &func_ctx.current_block {
            BlockPointer::Standard(_) => {
                func_ctx.standard_blocks.push(MIRBasicBlock {
                    id: block_id,
                    instructions: Vec::new(),
                });

                BlockPointer::Standard(func_ctx.standard_blocks.len() - 1)
            }
            BlockPointer::Defer(_) => {
                func_ctx.defer_blocks.push(MIRBasicBlock {
                    id: block_id,
                    instructions: Vec::new(),
                });

                BlockPointer::Defer(func_ctx.defer_blocks.len() - 1)
            }
        }
    }
    
    pub fn set_pointer(&mut self, pointer: BlockPointer) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        func_ctx.current_block = pointer;
    }
    
    #[allow(dead_code)]
    pub fn set_block(&mut self, block_id: CXIdent) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        if let Some((block_index, _)) = func_ctx
            .defer_blocks
            .iter()
            .enumerate()
            .find(|(_, block)| block.id == block_id)
        {
            self.set_pointer(BlockPointer::Defer(block_index));
            return;
        }

        if let Some((block_index, _)) = func_ctx
            .standard_blocks
            .iter()
            .enumerate()
            .find(|(_, block)| block.id == block_id)
        {
            self.set_pointer(BlockPointer::Standard(block_index));
            return;
        }

        unreachable!("Block ID not found: {}", block_id);
    }

    pub fn current_prototype(&self) -> &MIRFunctionPrototype {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        &func_ctx.current_prototype
    }

    pub fn in_defer(&self) -> bool {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };

        matches!(func_ctx.current_block, BlockPointer::Defer(_))
    }

    pub fn finish_function(&mut self) {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };
        
        // TODO: Refactor defer feature to proper scoped chains
        // Current implementation issues:
        // - Defer blocks are flat, don't track scope nesting
        // - Returns don't properly jump through defer chains
        // - Break/continue don't execute deferred expressions
        // - Single defer chain per function instead of scope-aware chains
        if !func_ctx.defer_blocks.is_empty() {
            let pointer = BlockPointer::Defer(self.get_defer_end());
            self.set_pointer(pointer);
            self.add_return(None);
        }

        let Some(func_ctx) = self.function_context.take() else {
            unreachable!()
        };

        // TODO: Update to use MIRFunction.body expression instead of basic_blocks
        // For now, keep basic_blocks to maintain existing functionality during refactoring
        let _full_blocks: Vec<_> = func_ctx.standard_blocks.into_iter()
            .chain(func_ctx.defer_blocks)
            .collect();

        let mir_function = MIRFunction {
            prototype: func_ctx.current_prototype,
            body: MIRExpressionKind::Unit, // TODO: Replace with actual expression body
        };

        self.generated_functions.push(mir_function);
    }
}
