use crate::ProgramBytecode;
use cx_data_ast::parse::ast::{CXExpr, CXFunctionPrototype, CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::types::BCType;
use cx_data_bytecode::{BlockInstruction, BytecodeFunction, BCFunctionPrototype, ElementID, FunctionBlock, ValueID, VirtualInstruction, VirtualValue, BCTypeMap, BCFunctionMap};
use cx_data_bytecode::node_type_map::ExprTypeMap;
use cx_util::log_error;
use cx_util::scoped_map::ScopedMap;
use crate::cx_maps::{convert_cx_func_map, convert_cx_type_map};

#[derive(Debug)]
pub struct BytecodeBuilder {
    global_strings: Vec<String>,
    functions: Vec<BytecodeFunction>,

    pub cx_type_map: CXTypeMap,
    pub cx_function_map: CXFunctionMap,
    
    pub type_map: BCTypeMap,
    pub fn_map: BCFunctionMap,
    
    pub expr_type_map: ExprTypeMap,

    pub current_block: u16,
    pub current_instruction: u16,

    pub symbol_table: ScopedMap<ValueID>,

    function_context: Option<BytecodeFunctionContext>,
}

#[derive(Debug)]
pub struct BytecodeFunctionContext {
    prototype: BCFunctionPrototype,
    current_block: ElementID,

    merge_stack: Vec<ElementID>,
    continue_stack: Vec<ElementID>,

    blocks: Vec<FunctionBlock>
}

impl BytecodeBuilder {
    pub fn new(type_map: CXTypeMap, fn_map: CXFunctionMap, expr_type_map: ExprTypeMap) -> Self {
        BytecodeBuilder {
            global_strings: Vec::new(),
            functions: Vec::new(),

            type_map: convert_cx_type_map(&type_map),
            fn_map: convert_cx_func_map(&type_map, &fn_map),
            expr_type_map,
            
            cx_type_map: type_map,
            cx_function_map: fn_map,

            symbol_table: ScopedMap::new(),
            current_block: 0,
            current_instruction: 0,

            function_context: None
        }
    }

    pub fn new_function(&mut self, fn_prototype: &CXFunctionPrototype) {
        let fn_prototype = self.convert_cx_prototype(fn_prototype).unwrap();
        self.function_context = Some(
            BytecodeFunctionContext {
                prototype: fn_prototype,
                current_block: 0,
                blocks: Vec::new(),
                merge_stack: Vec::new(),
                continue_stack: Vec::new()
            }
        );

        let entry_block = self.create_block();
        self.set_current_block(
            entry_block
        )
    }

    pub fn finish_function(&mut self) {
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
    
    pub fn add_instruction_bt(
        &mut self,
        instruction: VirtualInstruction,
        value_type: BCType
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

    pub fn add_instruction(
        &mut self,
        instruction: VirtualInstruction,
        value_type: CXType
    ) -> Option<ValueID> {
        let value_type = self.convert_cx_type(&value_type)?;
        
        self.add_instruction_bt(
            instruction,
            value_type
        )
    }

    pub fn get_variable(&self, value_id: ValueID) -> Option<&VirtualValue> {
        self.fun()
            .blocks[value_id.block_id as usize]
            .body.get(value_id.value_id as usize)
            .map(|v| &v.value)
    }

    pub fn get_expr_type(&self, expr: &CXExpr) -> Option<CXType> {
        self.expr_type_map.get(expr).cloned()
    }
    
    pub fn get_expr_intrinsic_type(&self, expr: &CXExpr) -> Option<CXTypeKind> {
        let Some(cx_type) = self.get_expr_type(expr) else {
            log_error!("INTERNAL PANIC: Failed to get intrinsic type for expression: {:?}\nKey: {}\n{:#?}", expr, &expr as *const _ as u64, self.expr_type_map);
        };
        
        cx_type.intrinsic_type(&self.cx_type_map).cloned()
    }
    
    pub fn get_type(&self, value_id: ValueID) -> Option<&BCType> {
        let Some(value) = self.get_variable(value_id) else {
            panic!("INTERNAL PANIC: Failed to get variable for value id: {:?}", value_id);
        };
        
        Some(&value.type_)
    }
    
    pub fn start_cont_point(&mut self) -> ElementID {
        let cond_block = self.create_block();

        let context = self.fun_mut();
        context.continue_stack.push(cond_block.clone());

        cond_block
    }

    pub fn start_scope(&mut self) -> ElementID {
        let merge_block = self.create_block();

        let context = self.fun_mut();
        context.merge_stack.push(merge_block.clone());

        merge_block
    }

    pub fn get_merge(&mut self) -> Option<ElementID> {
        let context = self.fun_mut();

        context.merge_stack.last().cloned()
    }

    pub fn get_continue(&mut self) -> Option<ElementID> {
        let context = self.fun_mut();

        context.continue_stack.last().cloned()
    }

    pub fn end_scope(&mut self) {
        let context = self.fun_mut();

        let merge_block = context.merge_stack.pop().unwrap();
        context.current_block = merge_block;
    }

    pub fn end_cond(&mut self) {
        let context = self.fun_mut();

        context.continue_stack.pop().unwrap();
    }

    pub fn set_current_block(&mut self, block: ElementID) {
        self.fun_mut().current_block = block;
    }

    pub fn create_global_string(&mut self, string: String) -> u32 {
        self.global_strings.push(string.clone());
        self.global_strings.len() as u32 - 1
    }

    pub fn create_block(&mut self) -> ElementID {
        let context = self.fun_mut();

        context.blocks.push(FunctionBlock {
            body: Vec::new()
        });

        (context.blocks.len() - 1) as ElementID
    }

    pub fn last_instruction(&self) -> Option<&BlockInstruction> {
        let context = self.fun();

        let block = context.blocks.last()?;
        block.body.last()
    }

    pub fn finish(self) -> Option<ProgramBytecode> {
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