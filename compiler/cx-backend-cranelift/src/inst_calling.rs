use crate::value_type::get_cranelift_abi_type;
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::{ArgumentPurpose, FuncRef, Inst};
use cranelift::prelude::{Signature, Value};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use cx_lmir::{LMIRFunctionSignature, LMIRParameterABI, LMIRReturnABI, LMIRValue};
use cx_util::CXResult;

pub(crate) fn prepare_function_sig(
    object_module: &mut ObjectModule,
    signature: &LMIRFunctionSignature,
) -> CXResult<Signature> {
    let mut sig = Signature::new(object_module.target_config().default_call_conv);

    match &signature.return_abi {
        LMIRReturnABI::Void => {}
        LMIRReturnABI::Direct { slots } => {
            for slot in slots {
                sig.returns.push(get_cranelift_abi_type(&slot._type)?);
            }
        }
        LMIRReturnABI::IndirectSret {
            returns_pointer: true,
            ..
        } => sig.returns.push(ir::AbiParam::new(
            object_module.target_config().pointer_type(),
        )),
        LMIRReturnABI::IndirectSret {
            returns_pointer: false,
            ..
        } => {}
    }

    if let LMIRReturnABI::IndirectSret {
        returns_pointer, ..
    } = &signature.return_abi
    {
        let param = if *returns_pointer {
            ir::AbiParam::new(object_module.target_config().pointer_type())
        } else {
            ir::AbiParam::special(
                object_module.target_config().pointer_type(),
                ArgumentPurpose::StructReturn,
            )
        };
        sig.params.push(param);
    }

    for param in signature.params.iter() {
        match &param.abi {
            LMIRParameterABI::Direct { slots } => {
                for slot in slots {
                    sig.params.push(get_cranelift_abi_type(&slot._type)?);
                }
            }
            LMIRParameterABI::Indirect { byval: true, .. } => {
                sig.params.push(ir::AbiParam::special(
                    object_module.target_config().pointer_type(),
                    ArgumentPurpose::StructArgument(param._type.size() as u32),
                ))
            }
            LMIRParameterABI::Indirect { byval: false, .. } => sig.params.push(ir::AbiParam::new(
                object_module.target_config().pointer_type(),
            )),
        }
    }

    Ok(sig)
}

pub(crate) fn prepare_method_call<'a>(
    context: &'a mut FunctionState,
    func: &LMIRValue,
    args: &'a [LMIRValue],
) -> CXResult<(CodegenValue, Vec<Value>)> {
    let val = context.get_value(func)?;
    let params = prepare_parameters(context, args)?;

    Ok((val, params))
}

pub(crate) fn prepare_parameters<'a>(
    context: &'a mut FunctionState,
    args: &'a [LMIRValue],
) -> CXResult<Vec<Value>> {
    let mut params = Vec::new();
    for arg in args {
        match context.get_value(arg)? {
            CodegenValue::Value(value) => params.push(value),
            CodegenValue::Aggregate(values) => params.extend(values),
            CodegenValue::AggregateSlots(values) => {
                params.extend(values.into_iter().map(|(_, value)| value))
            }
            CodegenValue::NULL => {}
        }
    }
    Ok(params)
}

pub(crate) fn get_method_return(context: &FunctionState, inst: Inst) -> CodegenValue {
    let results = context.builder.inst_results(inst);
    match results {
        [] => CodegenValue::NULL,
        [value] => CodegenValue::Value(*value),
        values => match &context.signature.return_abi {
            LMIRReturnABI::Direct { slots } => CodegenValue::AggregateSlots(
                slots.iter().cloned().zip(values.iter().copied()).collect(),
            ),
            _ => CodegenValue::Aggregate(values.to_vec()),
        },
    }
}

pub(crate) fn get_func_ref(
    context: &mut FunctionState,
    func_id: FuncId,
    signature: &LMIRFunctionSignature,
    args: &[Value],
) -> CXResult<FuncRef> {
    if !signature.var_args || args.len() == signature.expanded_param_count() {
        return Ok(context
            .object_module
            .declare_func_in_func(func_id, context.builder.func));
    }

    let mut sig = prepare_function_sig(context.object_module, signature)?;

    for i in signature.expanded_param_count()..args.len() {
        let arg_type = context.builder.func.dfg.value_type(args[i]);

        sig.params.push(ir::AbiParam::new(arg_type));
    }

    // S/O the rustc codegen team for this, see: https://github.com/rust-lang/rustc_codegen_cranelift/blob/master/src/abi/mod.rs
    let func_ref = context
        .object_module
        .declare_func_in_func(func_id, context.builder.func);

    let sig_ref = context
        .builder
        .func
        .dfg
        .ext_funcs
        .get(func_ref)
        .unwrap()
        .signature;

    context.builder.func.dfg.signatures[sig_ref] = sig;

    Ok(func_ref)
}
