use crate::routines::allocate_variable;
use crate::value_type::get_cranelift_abi_type;
use crate::{CodegenValue, FunctionState};
use cranelift::codegen::ir;
use cranelift::codegen::ir::{FuncRef, Inst};
use cranelift::prelude::{Signature, Value};
use cranelift_module::{FuncId, Module};
use cranelift_object::ObjectModule;
use cx_data_bytecode::{BCFunctionPrototype, BCParameter, ValueID};

pub(crate) fn prepare_function_sig(
    object_module: &mut ObjectModule,
    prototype: &BCFunctionPrototype,
) -> Option<Signature> {
    let mut sig = Signature::new(
        object_module.target_config().default_call_conv
    );

    if !prototype.return_type.is_void() {
        sig.returns.push(get_cranelift_abi_type(&prototype.return_type));

        if prototype.return_type.is_structure() {
            sig.params.push(get_cranelift_abi_type(&prototype.return_type));
        }
    }

    for BCParameter { type_, .. } in prototype.params.iter() {
        sig.params.push(get_cranelift_abi_type(type_));
    }

    Some(sig)
}

pub(crate) fn prepare_method_call<'a>(
    context: &'a mut FunctionState,
    func: ValueID,
    prototype: &BCFunctionPrototype,
    args: &'a [ValueID],
) -> Option<(CodegenValue, Vec<Value>)> {
    let val = context.variable_table.get(&func).cloned().unwrap();
    let mut params = args.iter()
        .map(|arg| context.variable_table.get(arg).unwrap().as_value())
        .collect::<Vec<_>>();

    if prototype.return_type.is_structure() {
        let type_size = prototype.return_type.fixed_size();
        let temp_buffer = allocate_variable(context, type_size as u32, None)?;

        params.insert(0, temp_buffer);
    }

    Some((val, params))
}

pub(crate) fn get_method_return(
    context: &FunctionState,
    inst: Inst
) -> Option<CodegenValue> {
    context.builder.inst_results(inst)
        .first()
        .cloned()
        .map(|res| CodegenValue::Value(res))
}

pub(crate) fn get_func_ref(
    context: &mut FunctionState,
    func_id: FuncId,
    name: &str,
    args: &[Value],
) -> Option<FuncRef> {
    let prototype = context
        .fn_map
        .get(name)?;

    if !prototype.var_args || args.len() == prototype.params.len() {
        return Some(
            context
            .object_module
            .declare_func_in_func(func_id, context.builder.func)
        )
    }

    let mut sig = prepare_function_sig(context.object_module, prototype)?;

    for i in prototype.params.len()..args.len() {
        let arg_type = context.builder.func.dfg.value_type(args[i]);

        sig.params.push(
            ir::AbiParam::new(arg_type)
        );
    }

    // S/O the rustc codegen team for this, see: https://github.com/rust-lang/rustc_codegen_cranelift/blob/master/src/abi/mod.rs
    let func_ref = context
        .object_module
        .declare_func_in_func(func_id, context.builder.func);

    let sig_ref = context.builder
        .func.dfg.ext_funcs.get(func_ref)?.signature;

    context.builder.func.dfg
        .signatures[sig_ref] = sig;

    Some(func_ref)
}