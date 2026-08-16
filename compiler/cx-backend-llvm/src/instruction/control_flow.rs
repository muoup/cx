use super::calls::build_direct_return_from_memory;
use super::inst_num;
use crate::typing::any_to_basic_val;
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRBlockTarget, LMIRReturnABI, LMIRValue};
use inkwell::basic_block::BasicBlock;

fn edge_destination<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    target: &LMIRBlockTarget,
    name: &str,
) -> Option<(BasicBlock<'a>, bool)> {
    if target.args.is_empty() {
        Some((function_state.get_block(&target.block)?, false))
    } else {
        Some((
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
) -> Option<()> {
    function_state.builder.position_at_end(edge);
    function_state.add_block_arguments(target, edge)?;
    function_state
        .builder
        .build_unconditional_branch(function_state.get_block(&target.block)?)
        .ok()?;
    Some(())
}

pub(super) fn generate_jump<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    target: &LMIRBlockTarget,
) -> Option<CodegenValue<'a>> {
    let predecessor = function_state.builder.get_insert_block()?;
    function_state.add_block_arguments(target, predecessor)?;
    function_state
        .builder
        .build_unconditional_branch(function_state.get_block(&target.block)?)
        .ok()?;
    Some(CodegenValue::Null)
}

pub(super) fn generate_branch<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    condition: &LMIRValue,
    true_target: &LMIRBlockTarget,
    false_target: &LMIRBlockTarget,
) -> Option<CodegenValue<'a>> {
    let mut condition = function_state
        .get_value(condition)?
        .get_value()
        .into_int_value();
    if condition.get_type().get_bit_width() > 1 {
        condition = function_state
            .builder
            .build_int_truncate(
                condition,
                global_state.context.bool_type(),
                inst_num().as_str(),
            )
            .unwrap();
    }

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
        .ok()?;

    if finish_true {
        finish_edge(function_state, true_edge, true_target)?;
    }
    if finish_false {
        finish_edge(function_state, false_edge, false_target)?;
    }
    Some(CodegenValue::Null)
}

pub(super) fn generate_jump_table<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    targets: &[(u64, LMIRBlockTarget)],
    default: &LMIRBlockTarget,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let value_type = value.get_type();
    let mut edges = Vec::with_capacity(targets.len());
    let llvm_targets = targets
        .iter()
        .map(|(case, target)| {
            let (edge, needs_finish) = edge_destination(
                global_state,
                function_state,
                target,
                &format!("edge_case_{}", inst_num()),
            )?;
            if needs_finish {
                edges.push((edge, target));
            }
            Some((value_type.const_int(*case, false), edge))
        })
        .collect::<Option<Vec<_>>>()?;
    let (default_edge, finish_default) = edge_destination(
        global_state,
        function_state,
        default,
        &format!("edge_default_{}", inst_num()),
    )?;

    function_state
        .builder
        .build_switch(value, default_edge, llvm_targets.as_slice())
        .ok()?;
    for (edge, target) in edges {
        finish_edge(function_state, edge, target)?;
    }
    if finish_default {
        finish_edge(function_state, default_edge, default)?;
    }
    Some(CodegenValue::Null)
}

pub(super) fn generate_return<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: Option<&LMIRValue>,
) -> Option<CodegenValue<'a>> {
    let Some(value) = value else {
        function_state.builder.build_return(None).unwrap();
        return Some(CodegenValue::Null);
    };

    let value = function_state.get_value(value)?;
    if function_state.signature.return_type.is_memory_resident()
        && matches!(
            function_state.signature.return_abi,
            LMIRReturnABI::Direct { .. }
        )
    {
        let return_value =
            build_direct_return_from_memory(global_state, function_state, value.get_value())?;
        function_state
            .builder
            .build_return(Some(&return_value))
            .unwrap();
        return Some(CodegenValue::Null);
    }

    let basic_value = any_to_basic_val(value.get_value())?;
    function_state
        .builder
        .build_return(Some(&basic_value))
        .unwrap();
    Some(CodegenValue::Null)
}

pub(super) fn generate_unreachable<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
) -> Option<CodegenValue<'a>> {
    function_state.builder.build_unreachable().ok()?;
    Some(CodegenValue::Null)
}
