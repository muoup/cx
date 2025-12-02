use crate::codegen::{codegen_fn_prototype, codegen_function};
use crate::globals::generate_global;
use crate::value_type::get_cranelift_type;
use cranelift::codegen::{ir, Context};
use cranelift::prelude::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Block, FunctionBuilder, InstBuilder, Value};
use cranelift_module::{DataId, FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_bytecode_data::types::BCTypeKind;
use cx_bytecode_data::{BCBlockID, BCValue, BCUnit};
use cx_util::log_error;
use std::collections::HashMap;

mod codegen;
mod globals;
mod inst_calling;
mod instruction;
mod routines;
mod value_type;

#[derive(Debug, Clone)]
pub(crate) enum CodegenValue {
    Value(Value),
    NULL,
}

impl CodegenValue {
    pub(crate) fn as_value(&self) -> Value {
        match self {
            CodegenValue::Value(value) => *value,

            _ => panic!("Expected Value, got: {self:?}"),
        }
    }
}

pub(crate) type VariableTable = HashMap<BCValue, CodegenValue>;

pub struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) target_frontend_config: &'a TargetFrontendConfig,

    pub(crate) function_ids: &'a mut HashMap<String, FuncId>,

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

    pub(crate) function_ids: HashMap<String, FuncId>,
    pub(crate) function_sigs: &'a mut HashMap<String, ir::Signature>,
}

impl FunctionState<'_> {
    pub(crate) fn get_block(&mut self, block_id: BCBlockID) -> Block {
        let id = match block_id {
            BCBlockID::Block(id) => id as usize,
            BCBlockID::DeferredBlock(id) => (id as usize) + self.defer_offset,
        };

        self.block_map
            .get(id)
            .cloned()
            .unwrap_or_else(|| panic!("Block with ID {id} not found in block map"))
    }

    pub(crate) fn get_value(&mut self, mir_value: &BCValue) -> Option<CodegenValue> {
        match mir_value {
            BCValue::NULL => Some(CodegenValue::NULL),

            BCValue::ParameterRef(i) => Some(CodegenValue::Value(Value::from_u32(*i))),

            BCValue::IntImmediate { val, _type: _type } => {
                let int_type = get_cranelift_type(_type);
                let value = self.builder.ins().iconst(int_type, *val);
                Some(CodegenValue::Value(value))
            }

            BCValue::FloatImmediate { val, _type: _type } => {
                match &_type.kind {
                    BCTypeKind::Float { bytes: 4 } => {
                        let as_f32 : f32 = val.into();
                        let value = self.builder.ins().f32const(as_f32);
                        Some(CodegenValue::Value(value))
                    }
                    BCTypeKind::Float { bytes: 8 } => {
                        let as_f64: f64 = val.into();
                        let value = self.builder.ins().f64const(as_f64);
                        Some(CodegenValue::Value(value))
                    }
                    _ => log_error!("Unsupported float type in FloatLiteral: {:?}", _type),
                }
            }

            BCValue::LoadOf(_type, val) => {
                let Some(addr) = self.get_value(val) else {
                    log_error!("Failed to get address for LoadOf: {:?}", val);
                };

                let addr = addr.as_value();
                let loaded = self.builder.ins().load(
                    get_cranelift_type(_type),
                    ir::MemFlags::new(),
                    addr,
                    0,
                );

                Some(CodegenValue::Value(loaded))
            }

            BCValue::Global(id) => {
                let global_ref = self
                    .object_module
                    .declare_data_in_func(DataId::from_u32(*id), self.builder.func);

                let gv = self
                    .builder
                    .ins()
                    .global_value(self.pointer_type, global_ref);

                Some(CodegenValue::Value(gv))
            }

            _ => {
                let Some(var) = self.variable_table.get(mir_value).cloned() else {
                    log_error!("Variable not found in variable table: {:?}", mir_value);
                };

                Some(var)
            }
        }
    }
}

pub fn bytecode_aot_codegen(bc: &BCUnit, output: &str) -> Option<Vec<u8>> {
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
            )
            .unwrap(),
        ),

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
        
        let func_sig = global_state
            .function_sigs
            .remove(&func.prototype.name)
            .unwrap_or_else(|| {
                panic!("Function prototype not found for: {}", func.prototype.name);
            });

        codegen_function(&mut global_state, func_id, func_sig, func)?;
    }

    global_state.object_module.finish().emit().ok()
}
