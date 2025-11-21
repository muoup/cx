use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRRegister},
    program::{MIRBasicBlock, MIRFunction},
    types::CXFunctionPrototype,
};
use cx_util::identifier::CXIdent;

pub(crate) struct MIRBuilder {
    pub generated_functions: Vec<MIRFunction>,
    pub function_context: Option<MIRFunctionContext>,
}

#[derive(Clone, Debug)]
pub enum BlockPointer {
    Standard(usize),
    Defer(usize),
}

pub(crate) struct MIRFunctionContext {
    pub current_prototype: CXFunctionPrototype,

    pub standard_blocks: Vec<MIRBasicBlock>,
    pub defer_blocks: Vec<MIRBasicBlock>,

    pub current_block: BlockPointer,
    pub defer_last_pointer: usize,

    pub temp_counter: usize,
}

impl MIRBuilder {
    pub fn new() -> Self {
        MIRBuilder {
            generated_functions: Vec::new(),
            function_context: None,
        }
    }

    pub fn start_function(&mut self, prototype: CXFunctionPrototype) {
        let function_context = MIRFunctionContext {
            current_prototype: prototype,
            standard_blocks: vec![MIRBasicBlock {
                id: CXIdent::from("entry"),
                expressions: Vec::new(),
            }],
            defer_blocks: Vec::new(),

            current_block: BlockPointer::Standard(0),
            defer_last_pointer: 0,

            temp_counter: 0,
        };

        self.function_context = Some(function_context);
    }

    pub fn new_register(&mut self) -> MIRRegister {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let id = format!("{}", func_ctx.temp_counter);
        func_ctx.temp_counter += 1;
        MIRRegister::from(id)
    }

    pub fn current_block(&mut self) -> &mut MIRBasicBlock {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        match &func_ctx.current_block {
            BlockPointer::Standard(index) => &mut func_ctx.standard_blocks[*index],
            BlockPointer::Defer(index) => &mut func_ctx.defer_blocks[*index],
        }
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction) {
        self.current_block().expressions.push(instruction);
    }

    pub fn add_jump(&mut self, target_block: CXIdent) {
        self.add_instruction(MIRInstruction::Jump {
            target: target_block,
        });
    }

    pub fn new_block_id(&mut self) -> CXIdent {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        CXIdent::from(format!("block_{}", func_ctx.standard_blocks.len()))
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
                    expressions: Vec::new(),
                });

                BlockPointer::Standard(func_ctx.standard_blocks.len() - 1)
            }
            BlockPointer::Defer(_) => {
                func_ctx.defer_blocks.push(MIRBasicBlock {
                    id: block_id,
                    expressions: Vec::new(),
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

    pub fn current_prototype(&self) -> &CXFunctionPrototype {
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
        let Some(func_ctx) = self.function_context.take() else {
            unreachable!()
        };

        let mir_function = MIRFunction {
            prototype: func_ctx.current_prototype,
            basic_blocks: func_ctx.standard_blocks,
        };

        self.generated_functions.push(mir_function);
    }
}
