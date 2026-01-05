use crate::codegen::{codegen_fn_prototype, codegen_function};
use crate::globals::generate_global;
use crate::value_type::get_cranelift_type;
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::{ir, Context};
use cranelift::prelude::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Block, FunctionBuilder, InstBuilder, Value};
use cranelift_module::{DataId, FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_bytecode_data::types::{BCFloatType, BCTypeKind};
use cx_bytecode_data::{BCBlockID, BCRegister, BCUnit, BCValue};
use cx_util::identifier::CXIdent;
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

pub(crate) type VariableTable = HashMap<BCRegister, CodegenValue>;

pub struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) target_frontend_config: &'a TargetFrontendConfig,

    pub(crate) function_ids: &'a mut HashMap<String, FuncId>,

    pub(crate) block_map: HashMap<CXIdent, Block>,
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) fn_params: Vec<Value>,

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
    pub(crate) fn get_function(&mut self, name: &str) -> Option<(FuncId, FuncRef)> {
        let func_id = self.function_ids.get(name).cloned()?;
        let func_ref = self
            .object_module
            .declare_func_in_func(func_id, self.builder.func);

        Some((func_id, func_ref))
    }

    pub(crate) fn get_block(&mut self, id: &BCBlockID) -> Block {
        self.block_map.get(id).cloned().unwrap()
    }

    pub(crate) fn get_value(&mut self, bc_value: &BCValue) -> Option<CodegenValue> {
        match bc_value {
            BCValue::NULL => Some(CodegenValue::NULL),

            BCValue::ParameterRef(i) => Some(CodegenValue::Value(Value::from_u32(*i))),

            BCValue::FunctionRef(name) => {
                let (_func_id, func_ref) = self.get_function(name.as_str())?;
                let as_value = self
                    .builder
                    .ins()
                    .func_addr(self.pointer_type, func_ref);
                
                Some(CodegenValue::Value(as_value))
            }

            BCValue::IntImmediate { val, _type } => {
                let int_type = get_cranelift_type(&BCTypeKind::Integer(*_type).into());
                let value = self.builder.ins().iconst(int_type, *val);
                Some(CodegenValue::Value(value))
            }

            BCValue::FloatImmediate {
                val,
                _type: BCFloatType::F32,
            } => {
                let as_f32: f32 = val.into();
                let value = self.builder.ins().f32const(as_f32);
                Some(CodegenValue::Value(value))
            }

            BCValue::FloatImmediate {
                val,
                _type: BCFloatType::F64,
            } => {
                let as_f64: f64 = val.into();
                let value = self.builder.ins().f64const(as_f64);
                Some(CodegenValue::Value(value))
            }
            
            BCValue::BoolImmediate(val) => {
                let bool_value = if *val { 1 } else { 0 };
                let value = self.builder.ins().iconst(ir::types::I8, bool_value);
                Some(CodegenValue::Value(value))
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

            BCValue::Register { register, _type } => {
                let Some(var) = self.variable_table.get(register).cloned() else {
                    log_error!("Variable not found in variable table: {:?}", bc_value);
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
