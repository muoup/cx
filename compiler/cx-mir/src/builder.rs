use cx_mir_data::{
    expression::{MIRInstruction, MIRRegister},
    program::{MIRBasicBlock, MIRFunction},
};
use cx_typechecker_data::cx_types::CXFunctionPrototype;
use cx_util::identifier::CXIdent;
use std::collections::HashMap;

pub(crate) struct MIRBuilder {
    pub generated_functions: Vec<MIRFunction>,
    function_context: Option<MIRFunctionContext>,
}

pub(crate) struct MIRFunctionContext {
    current_prototype: CXFunctionPrototype,
    basic_blocks: Vec<MIRBasicBlock>,
    current_block: usize,
    temp_counter: usize,
    variable_map: HashMap<CXIdent, MIRRegister>,
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
            basic_blocks: vec![MIRBasicBlock {
                id: CXIdent::from("entry"),
                expressions: Vec::new(),
            }],
            current_block: 0,
            temp_counter: 0,
            variable_map: HashMap::new(),
        };

        self.function_context = Some(function_context);
    }

    pub fn new_temp_register(&mut self) -> MIRRegister {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };
        let new_reg = CXIdent::from(format!("temp.{}", func_ctx.temp_counter));
        func_ctx.temp_counter += 1;
        new_reg
    }

    pub fn new_block_id(&mut self) -> CXIdent {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };
        let new_id = CXIdent::from(format!("bb{}", func_ctx.temp_counter));
        func_ctx.temp_counter += 1;
        new_id
    }

    pub fn declare_variable(&mut self, name: CXIdent, reg: MIRRegister) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };
        func_ctx.variable_map.insert(name, reg);
    }

    pub fn get_variable(&self, name: &CXIdent) -> Option<MIRRegister> {
        let Some(func_ctx) = &self.function_context else {
            unreachable!()
        };
        func_ctx.variable_map.get(name).cloned()
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        let current_block = &mut func_ctx.basic_blocks[func_ctx.current_block];
        current_block.expressions.push(instruction);
    }

    pub fn add_block(&mut self, block_id: CXIdent) -> usize {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        func_ctx.basic_blocks.push(MIRBasicBlock {
            id: block_id,
            expressions: Vec::new(),
        });
        func_ctx.current_block = func_ctx.basic_blocks.len() - 1;
        func_ctx.current_block
    }

    pub fn set_block(&mut self, block_index: usize) {
        let Some(func_ctx) = &mut self.function_context else {
            unreachable!()
        };

        if block_index < func_ctx.basic_blocks.len() {
            func_ctx.current_block = block_index;
        }
    }

    pub fn finish_function(&mut self) {
        let Some(func_ctx) = self.function_context.take() else {
            unreachable!()
        };

        let mir_function = MIRFunction {
            prototype: func_ctx.current_prototype,
            basic_blocks: func_ctx.basic_blocks,
        };

        self.generated_functions.push(mir_function);
    }
}
