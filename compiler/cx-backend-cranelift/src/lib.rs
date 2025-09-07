use std::collections::HashMap;
use cranelift::codegen::{ir, Context};
use cranelift::codegen::ir::GlobalValue;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Block, FunctionBuilder, GlobalValueData, InstBuilder, MemFlags, Value};
use cranelift_module::{DataId, FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_data_bytecode::{BCFunctionMap, BCFunctionPrototype, BlockID, ProgramBytecode, ValueID};
use cx_data_bytecode::types::BCType;
use cx_util::format::dump_data;
use cx_util::log_error;
use crate::codegen::{codegen_fn_prototype, codegen_function};
use crate::globals::generate_global;

mod codegen;
mod value_type;
mod routines;
mod instruction;
mod inst_calling;
mod globals;

#[derive(Debug, Clone)]
pub(crate) enum CodegenValue {
    Value(Value),
    FunctionID {
        fn_name: String,
        id: FuncId
    },
    NULL
}

impl CodegenValue {
    pub(crate) fn as_value(&self) -> Value {
        match self {
            CodegenValue::Value(value) => *value,

            _ => panic!("Expected Value, got: {self:?}")
        }
    }

    pub(crate) fn as_func_id(&self) -> FuncId {
        match self {
            CodegenValue::FunctionID { id, .. } => *id,

            _ => panic!("Expected FunctionID, got: {self:?}")
        }
    }

    pub(crate) fn as_func_name(&self) -> &str {
        match self {
            CodegenValue::FunctionID { fn_name, .. } => fn_name.as_str(),
            _ => panic!("Expected FunctionID, got: {self:?}")
        }
    }
}

pub(crate) type VariableTable = HashMap<ValueID, CodegenValue>;

pub struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) target_frontend_config: &'a TargetFrontendConfig,

    pub(crate) function_ids: &'a mut HashMap<String, FuncId>,
    pub(crate) fn_map: &'a BCFunctionMap,

    pub(crate) block_map: Vec<Block>,
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) fn_params: Vec<Value>,
    
    pub(crate) defer_offset: usize,

    pub(crate) variable_table: VariableTable,
    pub(crate) pointer_type: ir::Type,
}

pub(crate) struct GlobalState<'a> {
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,
    pub(crate) target_frontend_config: TargetFrontendConfig,

    pub(crate) fn_map: &'a BCFunctionMap,

    pub(crate) function_ids: HashMap<String, FuncId>,
    pub(crate) function_sigs: &'a mut HashMap<String, ir::Signature>,
}

impl FunctionState<'_> {
    pub(crate) fn get_block(&mut self, block_id: BlockID) -> Block {
        let id = match block_id {
            BlockID::Block(id) => id as usize,
            BlockID::DeferredBlock(id) => (id as usize) + self.defer_offset,

            _ => panic!("Invalid block type for block ID: {:?}", block_id)
        };
        
        self.block_map.get(id)
            .cloned()
            .unwrap_or_else(|| panic!("Block with ID {id} not found in block map"))
    }
}

pub fn bytecode_aot_codegen(bc: &ProgramBytecode, output: &str) -> Option<Vec<u8>> {
    let settings_builder = settings::builder();
    let flags = settings::Flags::new(settings_builder);

    let native_builder = cranelift_native::builder().unwrap();
    let isa = native_builder.finish(flags).unwrap();

    let mut global_state = GlobalState {
        object_module: ObjectModule::new(
            ObjectBuilder::new(
                isa.clone(),
                output,
                cranelift_module::default_libcall_names(),
            ).unwrap()
        ),

        fn_map: &bc.fn_map,

        context: Context::new(),
        target_frontend_config: isa.frontend_config(),
        function_ids: HashMap::new(),
        function_sigs: &mut HashMap::new(),
    };

    for global_var in bc.global_vars.iter() {
        generate_global(&mut global_state, global_var)?;
    }

    for fn_prototype in bc.fn_map.values() {
        codegen_fn_prototype(&mut global_state, fn_prototype);
    }

    for func in &bc.fn_defs {
        let Some(func_id) = global_state.function_ids.get(&func.prototype.name).cloned() else {
            log_error!(
                "Function not found in function map: {}",
                func.prototype.name
            );
        };
        let func_sig = global_state.function_sigs.remove(&func.prototype.name).unwrap_or_else(|| {
            panic!("Function signature redefine: {}", func.prototype.name);
        });

        codegen_function(&mut global_state, func_id, func_sig, func)?;
    }

    global_state.object_module.finish()
        .emit()
        .ok()
}

impl FunctionState<'_> {
    pub(crate) fn get_variable(&mut self, id: &ValueID) -> Option<CodegenValue> {
        match id {
            ValueID::NULL
                => Some(CodegenValue::NULL),
            ValueID::Global(index) => {
                let global_val_ref = self.object_module
                    .declare_data_in_func(
                        DataId::from_u32(*index as u32),
                        &mut self.builder.func
                    );

                let global_val = self.builder.ins()
                    .global_value(self.pointer_type, global_val_ref);

                Some(CodegenValue::Value(global_val))
            },
            ValueID::Block(..)
                => self.variable_table.get(id).cloned(),
        }
    }
}