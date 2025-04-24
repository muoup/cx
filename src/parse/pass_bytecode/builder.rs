use crate::lex::token::OperatorType;
use crate::parse::value_type::ValueType;
use crate::parse::pass_bytecode::ProgramBytecode;
use crate::parse::pass_molded::{CXBinOp, FunctionMap, TypeMap};
use crate::util::ScopedMap;

pub type ElementID = u32;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ValueID {
    pub block_id: ElementID,
    pub value_id: ElementID
}

impl ValueID {
    pub const NULL: Self = ValueID {
        block_id: u32::MAX,
        value_id: u32::MAX
    };
}

#[derive(Debug, Clone)]
pub struct VirtualValue {
    pub type_: ValueType
}

#[derive(Debug)]
pub struct BytecodeParameter {
    pub name: Option<String>,
    pub type_: ValueType
}

#[derive(Debug)]
pub struct BytecodeFunctionPrototype {
    pub name: String,
    pub return_type: ValueType,
    pub args: Vec<BytecodeParameter>
}

#[derive(Debug)]
pub struct BytecodeFunction {
    pub prototype: BytecodeFunctionPrototype,
    pub blocks: Vec<FunctionBlock>
}

#[derive(Debug)]
pub struct FunctionBlock {
    pub body: Vec<BlockInstruction>
}

#[derive(Debug)]
pub(crate) struct BytecodeBuilder {
    global_strings: Vec<String>,
    functions: Vec<BytecodeFunction>,

    pub type_map: TypeMap,
    pub fn_map: FunctionMap,

    pub current_block: u16,
    pub current_instruction: u16,

    pub symbol_table: ScopedMap<ValueID>,

    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub(crate) struct BytecodeFunctionContext {
    prototype: BytecodeFunctionPrototype,
    current_block: ElementID,

    blocks: Vec<FunctionBlock>
}

impl BytecodeBuilder {
    pub(crate) fn new(type_map: TypeMap, fn_map: FunctionMap) -> Self {
        BytecodeBuilder {
            global_strings: Vec::new(),
            functions: Vec::new(),

            type_map,
            fn_map,

            symbol_table: ScopedMap::new(),
            current_block: 0,
            current_instruction: 0,

            function_context: None
        }
    }

    pub(crate) fn new_function(&mut self, fn_prototype: BytecodeFunctionPrototype) {
        self.function_context = Some(
            BytecodeFunctionContext {
                prototype: fn_prototype,
                current_block: 0,
                blocks: Vec::new()
            }
        );

        let entry_block = self.create_block();
        self.set_current_block(
            entry_block
        )
    }

    pub(crate) fn finish_function(&mut self) {
        let context = self.function_context.take().unwrap();

        self.functions.push(
            BytecodeFunction {
                prototype: context.prototype,
                blocks: context.blocks
            }
        );
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
        let current_block = context.current_block;

        let body = &mut context.blocks[current_block as usize].body;

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

    pub(crate) fn last_instruction(&self) -> Option<&BlockInstruction> {
        let context = self.fun();

        let block = context.blocks.get(self.current_block as usize)?;
        block.body.last()
    }

    pub(crate) fn finish(self) -> Option<ProgramBytecode> {
        Some(
            ProgramBytecode {
                fn_map: self.fn_map,
                type_map: self.type_map,

                global_strs: self.global_strings,
                fn_defs: self.functions,
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
        param_index: u32
    },

    Allocate {
        size: usize
    },

    Load {
        value: ValueID,
    },

    Immediate {
        value: i32
    },

    StructAccess {
        struct_: ValueID,
        field_index: usize,
        field_offset: usize
    },

    Store {
        memory: ValueID,
        value: ValueID,
        type_: ValueType
    },

    Assign {
        target: ValueID,
        value: ValueID
    },

    ZExtend {
        value: ValueID,
    },

    SExtend {
        value: ValueID,
    },

    IntegerBinOp {
        op: CXBinOp,
        left: ValueID,
        right: ValueID
    },

    FloatBinOp {
        op: CXBinOp,
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