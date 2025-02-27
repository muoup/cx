use crate::lex::token::OperatorType;
use crate::parse::ast::ValueType;
use crate::parse::verify::context::{FnMap, FunctionPrototype, TypeMap};
use crate::parse::verify::VerifiedAST;

pub type ElementID = u32;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueID {
    pub block_id: ElementID,
    pub value_id: ElementID
}

#[derive(Debug, Clone)]
pub struct VirtualValue {
    pub type_: ValueType
}

#[derive(Debug)]
pub struct VerifiedFunction {
    pub prototype: FunctionPrototype,
    pub blocks: Vec<FunctionBlock>
}

#[derive(Debug)]
pub struct FunctionBlock {
    pub body: Vec<BlockInstruction>
}

#[derive(Debug)]
pub(crate) struct BytecodeBuilder {
    global_strings: Vec<String>,
    functions: Vec<VerifiedFunction>,

    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub(crate) struct BytecodeFunctionContext {
    fn_prototype: FunctionPrototype,
    current_function: ElementID,
    current_block: ElementID,

    blocks: Vec<FunctionBlock>
}

impl BytecodeBuilder {
    pub(crate) fn new() -> Self {
        BytecodeBuilder {
            global_strings: Vec::new(),
            functions: Vec::new(),

            function_context: None
        }
    }

    pub(crate) fn new_function(&mut self, prototype: FunctionPrototype) {
        self.function_context = Some(BytecodeFunctionContext {
            fn_prototype: prototype,
            current_function: 0,
            current_block: 0,
            blocks: Vec::new()
        });

        let entry_block = self.create_block();
        self.set_current_block(
            entry_block
        )
    }

    pub(crate) fn finish_function(&mut self) {
        let context = self.function_context.take().unwrap();

        self.functions.push(VerifiedFunction {
            prototype: context.fn_prototype,
            blocks: context.blocks
        });
    }

    fn fun_mut(&mut self) -> &mut BytecodeFunctionContext {
        self.function_context.as_mut()
            .expect("Attempted to access function context with no current function selected")
    }

    fn fun(&self) -> &BytecodeFunctionContext {
        self.function_context.as_ref()
            .expect("Attempted to access function context with no current function selected")
    }

    pub(crate) fn add_instruction(
        &mut self,
        instruction: VirtualInstruction,
        value_type: ValueType
    ) -> Option<ValueID> {
        let context = self.fun_mut();

        let virtual_value = VirtualValue {
            type_: value_type
        };

        let body = &mut context.blocks[context.current_block as usize].body;

        body.push(BlockInstruction {
            instruction,
            value: virtual_value.clone()
        });

        Some(
            ValueID {
                block_id: context.current_block,
                value_id: (body.len() - 1) as ElementID,
            }
        )
    }

    pub(crate) fn get_variable(&self, value_id: ValueID) -> Option<&VirtualValue> {
        self.fun()
            .blocks[value_id.block_id as usize]
            .body.get(value_id.value_id as usize)
            .map(|v| &v.value)
    }

    pub(crate) fn get_type(&self, value_id: ValueID) -> Option<&ValueType> {
        self.get_variable(value_id)
            .map(|v| &v.type_)
    }

    pub(crate) fn set_current_block(&mut self, block: ElementID) {
        self.fun_mut().current_block = block;
    }

    pub(crate) fn create_global_string(&mut self, string: String) -> u32 {
        self.global_strings.push(string.clone());
        self.global_strings.len() as u32 - 1
    }

    pub(crate) fn create_block(&mut self) -> ElementID {
        let context = self.fun_mut();

        context.blocks.push(FunctionBlock {
            body: Vec::new()
        });

        (context.blocks.len() - 1) as ElementID
    }

    pub(crate) fn finish(self, fn_map: FnMap, type_map: TypeMap) -> Option<VerifiedAST> {
        Some(
            VerifiedAST {
                fn_map,
                type_map,
                global_strs: self.global_strings,
                fn_defs: self.functions
            }
        )
    }
}

#[derive(Debug)]
pub struct BlockInstruction {
    pub instruction: VirtualInstruction,
    pub value: VirtualValue
}

#[derive(Debug)]
pub enum VirtualInstruction {
    FunctionParameter {
        name: String,
        type_: ValueType,
        param_index: u32
    },

    Allocate {
        size: usize
    },

    Load {
        value: ValueID,
        type_: ValueType
    },

    StructAccess {
        struct_: ValueID,
        type_: ValueType,
        field_index: usize,
        field_offset: usize
    },

    Store {
        memory: ValueID,
        value: ValueID
    },

    Assign {
        target: ValueID,
        value: ValueID
    },

    IntegerBinOp {
        op: OperatorType,
        left: ValueID,
        right: ValueID
    },

    FloatBinOp {
        op: OperatorType,
        left: ValueID,
        right: ValueID
    },

    Literal {
        val: u64
    },

    StringLiteral {
        str_id: ElementID
    },

    DirectCall {
        function: String,
        args: Vec<ValueID>
    },

    Branch {
        condition: ValueID,
        true_block: ElementID,
        false_block: ElementID
    },

    Jump {
        target: ElementID
    },

    Return {
        value: Option<ValueID>
    },

    NOP
}