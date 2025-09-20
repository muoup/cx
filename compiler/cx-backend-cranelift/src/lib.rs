use std::collections::HashMap;
use cranelift::codegen::{ir, Context};
use cranelift::prelude::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Block, FunctionBuilder, InstBuilder, Value};
use cranelift_module::{DataId, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_data_bytecode::{BCFunctionMap, BCFunctionPrototype, BlockID, ProgramBytecode, MIRValue};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_util::log_error;
use crate::codegen::{codegen_fn_prototype, codegen_function};
use crate::routines::string_literal;
use crate::value_type::get_cranelift_type;

mod codegen;
mod value_type;
mod routines;
mod instruction;
mod inst_calling;

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

pub(crate) type VariableTable = HashMap<MIRValue, CodegenValue>;

pub struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) target_frontend_config: &'a TargetFrontendConfig,

    pub(crate) function_ids: &'a mut HashMap<String, FuncId>,
    pub(crate) global_strs: &'a Vec<DataId>,

    pub(crate) fn_map: &'a BCFunctionMap,

    pub(crate) block_map: Vec<Block>,
    pub(crate) builder: FunctionBuilder<'a>,
    pub(crate) fn_params: Vec<Value>,
    
    pub(crate) defer_offset: usize,
    pub(crate) in_defer: bool,

    pub(crate) variable_table: VariableTable,

    pub(crate) pointer_type: ir::Type,
}

pub(crate) struct GlobalState<'a> {
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,
    pub(crate) target_frontend_config: TargetFrontendConfig,

    pub(crate) fn_map: &'a BCFunctionMap,
    pub(crate) global_strs: Vec<DataId>,

    pub(crate) function_ids: HashMap<String, FuncId>,
    pub(crate) function_sigs: &'a mut HashMap<String, ir::Signature>,
}

impl FunctionState<'_> {
    pub(crate) fn get_block(&mut self, block_id: BlockID) -> Block {
        let id = if !block_id.in_deferral {
            block_id.id as usize
        } else {
            block_id.id as usize + self.defer_offset
        };
        
        self.block_map.get(id)
            .cloned()
            .unwrap_or_else(|| panic!("Block with ID {id} not found in block map"))
    }

    pub(crate) fn get_value(&mut self, mir_value: &MIRValue) -> Option<CodegenValue> {
        match mir_value {
            MIRValue::IntImmediate { val, type_ } => {
                let int_type = get_cranelift_type(type_);
                let value = self.builder.ins().iconst(int_type, *val);
                Some(CodegenValue::Value(value))
            },
            MIRValue::FloatImmediate { val, type_ } => {
                let value = f64::from_bits(*val as u64);

                match &type_.kind {
                    BCTypeKind::Float { bytes: 4 } => {
                        let value = self.builder.ins().f32const(value as f32);
                        Some(CodegenValue::Value(value))
                    },
                    BCTypeKind::Float { bytes: 8 } => {
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
            MIRValue::NULL => Some(CodegenValue::NULL),

            _ => self.variable_table.get(mir_value).cloned()
        }
    }
}

pub fn bytecode_aot_codegen(ast: &ProgramBytecode, output: &str) -> Option<Vec<u8>> {
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

        fn_map: &ast.fn_map,

        context: Context::new(),
        target_frontend_config: isa.frontend_config(),
        global_strs: Vec::new(),
        function_ids: HashMap::new(),
        function_sigs: &mut HashMap::new(),
    };

    for global_str in ast.global_strs.iter() {
        let global_val = string_literal(&mut global_state.object_module, global_str);

        global_state.global_strs.push(global_val);
    }

    for fn_prototype in ast.fn_map.values() {
        codegen_fn_prototype(&mut global_state, fn_prototype);
    }

    for func in &ast.fn_defs {
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