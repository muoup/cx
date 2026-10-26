use super::calls::{apply_call_abi_attributes, build_direct_return_from_memory};
use super::inst_num;
use crate::log::{LLVMError, LLVMResult};
use crate::routines::get_function;
use crate::typing::any_to_basic_val;
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::{LMIRBlockTarget, LMIRReturnABI, LMIRValue};
use cx_log::catalogue::backend as catalogue;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicType;
use inkwell::values::{AnyValueEnum, PhiValue};

fn edge_destination<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    target: &LMIRBlockTarget,
    name: &str,
) -> LLVMResult<(BasicBlock<'a>, bool)> {
    if target.args.is_empty() {
        Ok((function_state.get_block(&target.block)?, false))
    } else {
        Ok((
            global_state
                .context
                .append_basic_block(*function_state.function_value, name),
            true,
        ))
    }
}

fn finish_edge<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    edge: BasicBlock<'a>,
    target: &LMIRBlockTarget,
) -> LLVMResult<()> {
    function_state.builder.position_at_end(edge);
    function_state.add_block_arguments(target, edge)?;
    function_state
        .builder
        .build_unconditional_branch(function_state.get_block(&target.block)?)
        .map_err(LLVMError::from_error)?;
    Ok(())
}

pub(super) fn generate_jump<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    target: &LMIRBlockTarget,
) -> LLVMResult<CodegenValue<'a>> {
    let predecessor = function_state
        .builder
        .get_insert_block()
        .unwrap_or_else(|| unreachable!("no LLVM insertion block for jump"));
    function_state.add_block_arguments(target, predecessor)?;
    function_state
        .builder
        .build_unconditional_branch(function_state.get_block(&target.block)?)
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_indirect_jump<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    address: &LMIRValue,
    targets: &[LMIRBlockTarget],
) -> LLVMResult<CodegenValue<'a>> {
    let address = function_state.get_value(address)?.as_basic_value()?;
    let predecessor = function_state
        .builder
        .get_insert_block()
        .unwrap_or_else(|| unreachable!("no LLVM insertion block for indirect jump"));
    let mut destinations = Vec::with_capacity(targets.len());
    for target in targets {
        function_state.add_block_arguments(target, predecessor)?;
        destinations.push(function_state.get_block(&target.block)?);
    }
    function_state
        .builder
        .build_indirect_branch(address, &destinations)
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_branch<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    condition: &LMIRValue,
    true_target: &LMIRBlockTarget,
    false_target: &LMIRBlockTarget,
) -> LLVMResult<CodegenValue<'a>> {
    let AnyValueEnum::IntValue(condition) = function_state.get_value(condition)?.get_value()?
    else {
        return Err(LLVMError::new(
            &catalogue::ENTITY_REQUIREMENT,
            ("branch condition".into(), "an integer value".into(), None),
        ));
    };

    let (true_edge, finish_true) = edge_destination(
        global_state,
        function_state,
        true_target,
        &format!("edge_true_{}", inst_num()),
    )?;
    let (false_edge, finish_false) = edge_destination(
        global_state,
        function_state,
        false_target,
        &format!("edge_false_{}", inst_num()),
    )?;
    function_state
        .builder
        .build_conditional_branch(condition, true_edge, false_edge)
        .map_err(LLVMError::from_error)?;

    if finish_true {
        finish_edge(function_state, true_edge, true_target)?;
    }
    if finish_false {
        finish_edge(function_state, false_edge, false_target)?;
    }
    Ok(CodegenValue::Null)
}

pub(super) fn generate_assert<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    condition: &LMIRValue,
    message: &LMIRValue,
) -> LLVMResult<CodegenValue<'a>> {
    let AnyValueEnum::IntValue(condition) = function_state.get_value(condition)?.get_value()?
    else {
        return Err(LLVMError::new(
            &catalogue::ENTITY_REQUIREMENT,
            (
                "assertion condition".into(),
                "an integer value".into(),
                None,
            ),
        ));
    };
    let message = function_state.get_value(message)?.as_basic_value()?;
    let predecessor = function_state
        .builder
        .get_insert_block()
        .unwrap_or_else(|| unreachable!("no LLVM insertion block for assertion"));

    let (fail, phi) = match function_state.assert_fail.get() {
        Some(fail) => fail,
        None => {
            let fail = assert_fail_block(global_state, function_state, message.get_type())?;
            function_state.assert_fail.set(Some(fail));
            fail
        }
    };
    phi.add_incoming(&[(&message, predecessor)]);

    let next = global_state.context.append_basic_block(
        *function_state.function_value,
        &format!("assert_ok_{}", inst_num()),
    );
    function_state.builder.position_at_end(predecessor);
    function_state
        .builder
        .build_conditional_branch(condition, next, fail)
        .map_err(LLVMError::from_error)?;
    function_state.builder.position_at_end(next);
    Ok(CodegenValue::Null)
}

fn assert_fail_block<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    message_type: impl BasicType<'a>,
) -> LLVMResult<(BasicBlock<'a>, PhiValue<'a>)> {
    let symbol = ASSERTION.symbol_name();
    let signature = &global_state
        .function_map
        .get(&symbol)
        .ok_or_else(|| {
            LLVMError::new(
                &catalogue::MISSING_ENTITY,
                (format!("function '{symbol}'"), "LMIR function map".into()),
            )
        })?
        .signature;
    let function = get_function(global_state, &symbol, signature)?;

    let block = global_state
        .context
        .append_basic_block(*function_state.function_value, "assert_fail");
    function_state.builder.position_at_end(block);
    let phi = function_state
        .builder
        .build_phi(message_type, "assert_message")
        .map_err(LLVMError::from_error)?;
    let condition = function.get_type().get_param_types()[0]
        .into_int_type()
        .const_zero();
    let call = function_state
        .builder
        .build_direct_call(
            function,
            &[condition.into(), phi.as_basic_value().into()],
            inst_num().as_str(),
        )
        .map_err(LLVMError::from_error)?;
    apply_call_abi_attributes(global_state, &call, signature)?;
    function_state
        .builder
        .build_unreachable()
        .map_err(LLVMError::from_error)?;
    Ok((block, phi))
}

pub(super) fn generate_jump_table<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    targets: &[(u64, LMIRBlockTarget)],
    default: &LMIRBlockTarget,
) -> LLVMResult<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()?
        .into_int_value();
    let value_type = value.get_type();
    let mut edges = Vec::with_capacity(targets.len());
    let llvm_targets = targets
        .iter()
        .map(|(case, target)| -> LLVMResult<_> {
            let (edge, needs_finish) = edge_destination(
                global_state,
                function_state,
                target,
                &format!("edge_case_{}", inst_num()),
            )?;
            if needs_finish {
                edges.push((edge, target));
            }
            Ok((value_type.const_int(*case, false), edge))
        })
        .collect::<LLVMResult<Vec<_>>>()?;
    let (default_edge, finish_default) = edge_destination(
        global_state,
        function_state,
        default,
        &format!("edge_default_{}", inst_num()),
    )?;

    function_state
        .builder
        .build_switch(value, default_edge, llvm_targets.as_slice())
        .map_err(LLVMError::from_error)?;
    for (edge, target) in edges {
        finish_edge(function_state, edge, target)?;
    }
    if finish_default {
        finish_edge(function_state, default_edge, default)?;
    }
    Ok(CodegenValue::Null)
}

pub(super) fn generate_return<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: Option<&LMIRValue>,
) -> LLVMResult<CodegenValue<'a>> {
    let Some(value) = value else {
        function_state
            .builder
            .build_return(None)
            .map_err(LLVMError::from_error)?;
        return Ok(CodegenValue::Null);
    };

    let value = function_state.get_value(value)?;
    if function_state.signature.return_type.is_memory_resident()
        && matches!(
            function_state.signature.return_abi,
            LMIRReturnABI::Direct { .. }
        )
    {
        let return_value =
            build_direct_return_from_memory(global_state, function_state, value.get_value()?)?;
        function_state
            .builder
            .build_return(Some(&return_value))
            .map_err(LLVMError::from_error)?;
        return Ok(CodegenValue::Null);
    }

    let basic_value = any_to_basic_val(value.get_value()?)?;
    function_state
        .builder
        .build_return(Some(&basic_value))
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_unreachable<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
) -> LLVMResult<CodegenValue<'a>> {
    function_state
        .builder
        .build_unreachable()
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}
