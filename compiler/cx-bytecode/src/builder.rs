use cx_bytecode_data::{
    bc_type::BCFunctionPrototype,
    compilation_unit::{BCBlock, BCFunction, BCUnit},
    instruction::BCInstruction,
};
use cx_mir_data::MIRFunction;
use cx_util::identifier::CXIdent;

pub struct BytecodeBuilder<'a> {
    program: BCUnit,
    current_function: Option<CurrentFunction<'a>>,
}

struct CurrentFunction<'a> {
    base: &'a MIRFunction,
    prototype: BCFunctionPrototype,
    params: Vec<CXIdent>,
    blocks: Vec<BCBlock>,
    pointer: InstructionPointer,
}

struct InstructionPointer {
    block_idx: u16,
    instruction_idx: u16,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new() -> Self {
        Self {
            program: BCUnit {
                function_prototypes: Vec::new(),
            },
            current_function: None,
        }
    }

    pub fn add_instruction(&mut self, instruction: BCInstruction) {
        if let Some(current_function) = &mut self.current_function {
            let block_idx = current_function.pointer.block_idx as usize;
            let instruction_idx = current_function.pointer.instruction_idx as usize;

            if block_idx >= current_function.blocks.len() {
                unreachable!("Invalid block index");
            }

            if instruction_idx > current_function.blocks[block_idx].instructions.len() {
                unreachable!("Invalid instruction index");
            }

            current_function.blocks[block_idx]
                .instructions
                .insert(instruction_idx, instruction);

            current_function.pointer.instruction_idx += 1;
        } else {
            unreachable!("No function is being built");
        }
    }

    pub fn parameter_id(&self, index: usize) -> CXIdent {
        if let Some(current_function) = &self.current_function {
            if index >= current_function.params.len() {
                unreachable!("Parameter index out of bounds");
            }

            current_function.params[index].clone()
        } else {
            unreachable!("No function is being built");
        }
    }

    pub fn new_function(
        &mut self,
        base: &'a MIRFunction,
        prototype: BCFunctionPrototype,
        params: Vec<CXIdent>,
    ) {
        if self.current_function.is_some() {
            unreachable!("A function is already being built");
        }

        self.current_function = Some(CurrentFunction {
            base,
            prototype,
            params,
            blocks: vec![],
            pointer: InstructionPointer {
                block_idx: 0,
                instruction_idx: 0,
            },
        });
    }

    pub fn finish_function(&mut self) {
        if let Some(current_function) = self.current_function.take() {
            let function = BCFunction {
                prototype: current_function.prototype,
                params: current_function.params,
                blocks: current_function.blocks,
            };

            println!("Produced Function: {}", function);

            self.program
                .function_prototypes
                .push(function.prototype.clone());
        } else {
            panic!("No function is being built");
        }
    }

    pub fn defer_offset(&self) -> usize {
        if let Some(current_function) = &self.current_function {
            current_function.base.blocks.len()
        } else {
            unreachable!("No function is being built");
        }
    }
    
    pub fn get_block(&self, idx: usize) -> CXIdent {
        if let Some(current_function) = &self.current_function {
            current_function.blocks[idx].name.clone()
        } else {
            unreachable!("No function is being built");
        }
    }

    pub fn add_block(&mut self, name: CXIdent) -> usize {
        if let Some(current_function) = &mut self.current_function {
            current_function.blocks.push(BCBlock {
                name,
                instructions: Vec::new(),
            });

            current_function.blocks.len() - 1
        } else {
            unreachable!("No function is being built");
        }
    }

    pub fn set_block_pointer(&mut self, block_id: usize) {
        if let Some(current_function) = &mut self.current_function {
            if block_id >= current_function.blocks.len() {
                unreachable!("Invalid block index");
            }

            current_function.pointer.block_idx = block_id as u16;
            current_function.pointer.instruction_idx =
                current_function.blocks[block_id].instructions.len() as u16;
        } else {
            unreachable!("No function is being built");
        }
    }
}
