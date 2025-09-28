use std::collections::HashMap;
use cranelift::codegen::{ir, Context};
use cranelift::prelude::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Block, FunctionBuilder, InstBuilder, Value};
use cranelift_module::{DataId, FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_data_mir::{BCFunctionMap, BlockID, ProgramMIR, MIRValue};
use cx_data_mir::types::MIRTypeKind;
use cx_util::log_error;
use crate::codegen::{codegen_fn_prototype, codegen_function};
use crate::globals::generate_global;
use crate::value_type::get_cranelift_type;

mod codegen;
mod value_type;
mod routines;
mod instruction;
mod inst_calling;
mod globals;

#[derive(Debug, Clone)]
pub(crate) enum CodegenValue {
    Value(Value),
    NULL
}

impl CodegenValue {
    pub(crate) fn as_value(&self) -> Value {
        match self {
            CodegenValue::Value(value) => *value,

            _ => panic!("Expected Value, got: {self:?}")
        }
    }
}

pub(crate) type VariableTable = HashMap<MIRValue, CodegenValue>;

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

            _ => panic!("Invalid block type for block ID: {block_id:?}")
        };
        
        self.block_map.get(id)
            .cloned()
            .unwrap_or_else(|| panic!("Block with ID {id} not found in block map"))
    }

    pub(crate) fn get_value(&mut self, mir_value: &MIRValue) -> Option<CodegenValue> {
        match mir_value {
            MIRValue::NULL => Some(CodegenValue::NULL),

            MIRValue::ParameterRef(i) => Some(CodegenValue::Value(Value::from_u32(*i))),

            MIRValue::IntImmediate { val, type_ } => {
                let int_type = get_cranelift_type(type_);
                let value = self.builder.ins().iconst(int_type, *val);
                Some(CodegenValue::Value(value))
            },

            MIRValue::FloatImmediate { val, type_ } => {
                let value = f64::from_bits(*val as u64);

                match &type_.kind {
                    MIRTypeKind::Float { bytes: 4 } => {
                        let value = self.builder.ins().f32const(value as f32);
                        Some(CodegenValue::Value(value))
                    },
                    MIRTypeKind::Float { bytes: 8 } => {
                        let value = self.builder.ins().f64const(value);
                        Some(CodegenValue::Value(value))
                    },
                    _ => log_error!("Unsupported float type in FloatLiteral: {:?}", type_)
                }
            },

            MIRValue::LoadOf(_type, val) => {
                let Some(addr) = self.get_value(val) else {
                    log_error!("Failed to get address for LoadOf: {:?}", val);
                };

                let addr = addr.as_value();
                let loaded = self.builder.ins().load(
                    get_cranelift_type(_type),
                    ir::MemFlags::new(),
                    addr,
                    0
                );

                Some(CodegenValue::Value(loaded))
            },

            MIRValue::Global(id) => {
                let global_ref = self.object_module
                    .declare_data_in_func(
                        DataId::from_u32(*id),
                        self.builder.func
                    );

                let gv = self.builder.ins()
                    .global_value(self.pointer_type, global_ref);

                Some(CodegenValue::Value(gv))
            },

            _ => {
                let Some(var) = self.variable_table.get(mir_value).cloned() else {
                    log_error!("Variable not found in variable table: {:?}", mir_value);
                };

                Some(var)
            }
        }
    }
}

pub fn bytecode_aot_codegen(bc: &ProgramMIR, output: &str) -> Option<Vec<u8>> {
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