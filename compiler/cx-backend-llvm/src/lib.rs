use crate::attributes::*;
use crate::typing::{
    any_to_basic_type, any_to_basic_val, bc_llvm_prototype, bc_llvm_type, convert_linkage,
};
use cx_lmir::{
    ElementID, LMIRABISlot, LMIRBasicBlock, LMIRBlockID, LMIRBlockTarget, LMIRFunction,
    LMIRFunctionMap, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRRegister, LMIRReturnABI,
    LMIRUnit, LMIRValue,
};
use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_target::ArchitectureConfig;
use cx_util::identifier::CXIdent;
use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicType, FunctionType, IntType};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue, GlobalValue, PhiValue,
};

use crate::globals::generate_global_variable;
use crate::instruction::reset_num;
use cx_pipeline_data::OptimizationLevel;
use cx_util::format::dump_data;
use inkwell::basic_block::BasicBlock;
use std::collections::HashMap;

mod arithmetic;
mod attributes;
mod globals;
mod instruction;
mod routines;
pub(crate) mod typing;

pub(crate) struct GlobalState<'a> {
    architecture: &'a ArchitectureConfig,
    module: Module<'a>,
    context: &'a Context,
    pointer_int_type: IntType<'a>,

    globals: Vec<GlobalValue<'a>>,

    functions: HashMap<String, FunctionType<'a>>,
    function_map: &'a LMIRFunctionMap,
}

pub(crate) struct FunctionState<'a, 'b> {
    context: &'a Context,
    function_value: &'b FunctionValue<'a>,

    current_function: CXIdent,
    signature: LMIRFunctionSignature,

    builder: Builder<'a>,
    block_map: HashMap<LMIRBlockID, BasicBlock<'a>>,
    value_map: HashMap<LMIRValue, CodegenValue<'a>>,
    block_params: HashMap<LMIRBlockID, Vec<(LMIRRegister, PhiValue<'a>)>>,
}

impl<'a> FunctionState<'a, '_> {
    pub(crate) fn get_value(&self, val: &LMIRValue) -> Option<CodegenValue<'a>> {
        match val {
            LMIRValue::ParameterRef(index) => {
                let param_val = self
                    .function_value
                    .get_nth_param(*index)
                    .unwrap_or_else(|| {
                        panic!(
                            "Parameter index {index} out of bounds for function {}",
                            self.current_function
                        )
                    })
                    .as_any_value_enum();

                Some(CodegenValue::Value(param_val))
            }

            LMIRValue::IntImmediate { val, _type } => {
                let int_type = bc_llvm_type(self.context, _type)?;
                let int_val = int_type
                    .into_int_type()
                    .const_int(*val as u64, true)
                    .as_any_value_enum();

                Some(CodegenValue::Value(int_val))
            }

            LMIRValue::FloatImmediate { val, _type } => {
                let float_type = bc_llvm_type(self.context, _type)?;
                let float_val = float_type
                    .into_float_type()
                    .const_float(val.into())
                    .as_any_value_enum();

                Some(CodegenValue::Value(float_val))
            }

            LMIRValue::FunctionRef(_) => {
                panic!("Function references should be handled at a higher level")
            }

            LMIRValue::Register { .. } | LMIRValue::Global(..) => self.value_map.get(val).cloned(),

            LMIRValue::NULL => Some(CodegenValue::Null),
        }
    }

    pub(crate) fn get_block(&self, block_id: &LMIRBlockID) -> Option<BasicBlock<'a>> {
        self.block_map.get(block_id).copied()
    }

    pub(crate) fn add_block_arguments(
        &self,
        target: &LMIRBlockTarget,
        predecessor: BasicBlock<'a>,
    ) -> Option<()> {
        let params = self.block_params.get(&target.block)?;
        assert_eq!(
            params.len(),
            target.args.len(),
            "LMIR edge to {} has {} arguments for {} parameters",
            target.block,
            target.args.len(),
            params.len(),
        );

        for ((_, phi), argument) in params.iter().zip(&target.args) {
            let value = self.get_value(argument)?.as_basic_value()?;
            phi.add_incoming(&[(&value, predecessor)]);
        }
        Some(())
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CodegenValue<'a> {
    Value(AnyValueEnum<'a>),
    AggregateSlots(Vec<(LMIRABISlot, BasicValueEnum<'a>)>),
    Null,
}

impl<'a> CodegenValue<'a> {
    pub fn get_value(&self) -> AnyValueEnum<'a> {
        match self {
            CodegenValue::Value(value) => *value,

            _ => panic!("Expected a value, found: {self:?}"),
        }
    }

    pub fn as_basic_value(&self) -> Option<BasicValueEnum<'a>> {
        match self {
            CodegenValue::Value(value) => any_to_basic_val(*value),
            CodegenValue::AggregateSlots(_) | CodegenValue::Null => None,
        }
    }
}

pub fn lmir_aot_codegen(
    bytecode: &LMIRUnit,
    output_path: &str,
    optimization_level: OptimizationLevel,
) -> CXResult<Vec<u8>> {
    let context = Context::create();
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native");

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).expect("Failed to get target from triple");
    let (pass_manager_str, inkwell_optimization_level) = match optimization_level {
        OptimizationLevel::O0 => ("default<O0>", inkwell::OptimizationLevel::None),
        OptimizationLevel::O1 => ("default<O1>", inkwell::OptimizationLevel::Less),
        OptimizationLevel::O2 => ("default<O2>", inkwell::OptimizationLevel::Default),
        OptimizationLevel::O3 => ("default<O3>", inkwell::OptimizationLevel::Aggressive),
        OptimizationLevel::Osize => ("default<Os>", inkwell::OptimizationLevel::Default),
        OptimizationLevel::Ofast => ("default<O3>", inkwell::OptimizationLevel::Aggressive),
    };
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            inkwell_optimization_level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("Failed to create target machine");
    let target_data = target_machine.get_target_data();
    let pointer_size = target_data.get_pointer_byte_size(None) as usize;
    let pointer_alignment =
        target_data.get_abi_alignment(&context.ptr_type(inkwell::AddressSpace::from(0))) as usize;
    if bytecode.architecture.pointer_size() != pointer_size
        || bytecode.architecture.pointer_alignment() != pointer_alignment
    {
        return Err(CXErr::new(
            CXStdErrMessage::error(
                "CODEGEN ERROR",
                format!(
                    "LMIR target uses pointer size/alignment {}/{}, but LLVM target uses {}/{}",
                    bytecode.architecture.pointer_size(),
                    bytecode.architecture.pointer_alignment(),
                    pointer_size,
                    pointer_alignment,
                ),
            ),
            CXInternalContext::error("LMIR and LLVM target configurations disagree"),
        ));
    }

    let module = context.create_module(output_path);
    module.set_triple(&triple);
    module.set_data_layout(&target_data.get_data_layout());

    let mut global_state = GlobalState {
        architecture: &bytecode.architecture,
        module,
        context: &context,
        pointer_int_type: context.ptr_sized_int_type(&target_data, None),

        globals: Vec::new(),
        functions: HashMap::new(),

        function_map: &bytecode.fn_map,
    };

    for prototypes in global_state.function_map.values() {
        cache_prototype(&mut global_state, prototypes).unwrap();
    }

    for global in bytecode.global_vars.iter() {
        generate_global_variable(&mut global_state, global)
            .unwrap_or_else(|| panic!("Failed to generate global variable: {}", global.name));
    }

    for func in bytecode.fn_defs.iter() {
        fn_aot_codegen(func, &global_state).unwrap_or_else(|| {
            panic!(
                "Failed to generate function code for function: {}",
                func.prototype.name
            )
        });
    }

    global_state.module.verify().unwrap_or_else(|err| {
        dump_data(&global_state.module.print_to_string().to_string_lossy());
        panic!("Module verification failed: {}", err.to_string());
    });
    global_state
        .module
        .run_passes(
            pass_manager_str,
            &target_machine,
            PassBuilderOptions::create(),
        )
        .expect("Failed to run passes");

    if !output_path.contains("std/") {
        dump_data(&format!(
            "{}",
            global_state.module.print_to_string().to_string_lossy()
        ));
    }

    let buff = target_machine
        .write_to_memory_buffer(&global_state.module, inkwell::targets::FileType::Object)
        .expect("Failed to export module to file");

    Ok(buff.as_slice().to_vec())
}

fn fn_aot_codegen(bytecode: &LMIRFunction, global_state: &GlobalState) -> Option<()> {
    reset_num();

    let func_val = global_state
        .module
        .get_function(bytecode.prototype.name.as_str())
        .unwrap_or_else(|| {
            panic!(
                "Failed to get function from module: {}",
                bytecode.prototype.name
            )
        });
    let builder = global_state.context.create_builder();

    let mut function_state = FunctionState {
        context: global_state.context,
        function_value: &func_val,

        current_function: bytecode.prototype.name.clone(),
        signature: bytecode.prototype.signature.clone(),

        builder,
        block_map: HashMap::new(),
        value_map: HashMap::new(),
        block_params: HashMap::new(),
    };

    for (i, global) in global_state.globals.iter().enumerate() {
        function_state.value_map.insert(
            LMIRValue::Global(i as ElementID),
            CodegenValue::Value(global.as_any_value_enum()),
        );
    }

    let entry = global_state.context.append_basic_block(func_val, "entry");
    function_state
        .block_map
        .insert(CXIdent::from("entry"), entry);

    for block in &bytecode.blocks {
        let llvm_block = global_state
            .context
            .append_basic_block(func_val, block.id.as_str());
        function_state
            .block_map
            .insert(block.id.clone(), llvm_block);
    }

    for block in &bytecode.blocks {
        let llvm_block = function_state
            .get_block(&block.id)
            .unwrap_or_else(|| panic!("Block with ID {} not found in function", block.id));
        function_state.builder.position_at_end(llvm_block);

        let mut params = Vec::with_capacity(block.params.len());
        for parameter in &block.params {
            let llvm_type = if parameter._type.is_memory_resident() {
                global_state
                    .context
                    .ptr_type(inkwell::AddressSpace::from(0))
                    .as_basic_type_enum()
            } else {
                any_to_basic_type(bc_llvm_type(global_state.context, &parameter._type)?)?
            };
            let phi = function_state
                .builder
                .build_phi(
                    llvm_type,
                    &format!("block_param_{}", parameter.register.name),
                )
                .unwrap();
            function_state.value_map.insert(
                LMIRValue::Register {
                    register: parameter.register.clone(),
                    _type: parameter._type.clone(),
                },
                CodegenValue::Value(phi.as_basic_value().as_any_value_enum()),
            );
            params.push((parameter.register.clone(), phi));
        }
        function_state.block_params.insert(block.id.clone(), params);
    }

    // Set the entry block as the current block
    function_state.builder.position_at_end(entry);
    let Ok(_) = function_state
        .builder
        .build_unconditional_branch(function_state.get_block(&bytecode.blocks[0].id).unwrap())
    else {
        panic!(
            "Failed to build unconditional branch to entry block: {}",
            bytecode.blocks[0].id
        )
    };

    for block in bytecode.blocks.iter() {
        codegen_block(global_state, &mut function_state, &block.id, block);
    }

    Some(())
}

fn codegen_block<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &mut FunctionState<'a, 'b>,
    block_id: &LMIRBlockID,
    block: &LMIRBasicBlock,
) {
    let block_val = function_state
        .get_block(block_id)
        .unwrap_or_else(|| panic!("Block with ID {block_id} not found in function"));
    function_state.builder.position_at_end(block_val);

    for inst in block.body.iter() {
        let Some(value) = instruction::generate_instruction(global_state, function_state, inst)
        else {
            panic!(
                "Failed to generate instruction: {inst} in function: {}",
                function_state.current_function
            );
        };

        if let Some(result_reg) = &inst.result {
            let bc_reg = LMIRValue::Register {
                register: result_reg.clone(),
                _type: inst.value_type.clone(),
            };

            function_state.value_map.insert(bc_reg, value.clone());
        }

        if inst.kind.is_block_terminating() {
            break;
        }
    }
}

fn cache_prototype<'a>(
    global_state: &mut GlobalState<'a>,
    prototype: &'a LMIRFunctionPrototype,
) -> Option<()> {
    let llvm_prototype = bc_llvm_prototype(global_state, prototype).unwrap();

    let func = global_state.module.add_function(
        prototype.name.as_str(),
        llvm_prototype,
        Some(convert_linkage(prototype.linkage)),
    );

    // Put each function in its own ELF section for --gc-sections DCE
    func.set_section(Some(&format!(".text.{}", prototype.name)));

    let signature = prototype.signature();
    for i in 0..signature.expanded_param_count() {
        let param_type = signature
            .expanded_param_type(global_state.architecture, i)
            .unwrap();
        get_type_attributes(global_state.context, &param_type)
            .into_iter()
            .for_each(|attr| {
                func.add_attribute(AttributeLoc::Param(i as u32), attr);
            });
    }

    if matches!(&signature.return_abi, LMIRReturnABI::IndirectSret { .. }) {
        let pointee = bc_llvm_type(global_state.context, &signature.return_type)?;
        func.add_attribute(
            AttributeLoc::Param(0),
            attr_sret(global_state.context, pointee),
        );
    }
    global_state
        .functions
        .insert(prototype.name.to_string(), func.get_type());

    Some(())
}
