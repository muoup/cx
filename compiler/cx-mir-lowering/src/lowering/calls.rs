use crate::lowering::memory;
use cx_lmir::types::LMIRIntegerType;
use cx_lmir::{
    LMIRFunctionSignature, LMIRInstructionKind, LMIRParameterABI, LMIRReturnABI, LMIRValue,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRConstant, MIRFnSignature, MIRRegister, MIRTypeID, MIRTypeKind, MIRValue};

use crate::context::FunctionContext;

use super::{typing::classify_signature, values};

pub(super) fn lower_call(
    context: &mut FunctionContext<'_, '_>,
    out: Option<MIRRegister>,
    callee: &MIRValue,
    args: &[MIRValue],
) {
    let signature = signature(context, callee);
    let mut lowered = Vec::new();
    let sret = if matches!(signature.return_abi, LMIRReturnABI::IndirectSret { .. }) {
        let result = out.expect("indirect return has no MIR result");
        let ty = context.body.register(result).unwrap().ty;
        let address = memory::allocate(context, ty);
        lowered.push(address.clone());
        Some(address)
    } else {
        None
    };
    for (index, argument) in args.iter().enumerate() {
        if let Some(parameter) = signature.params.get(index) {
            let param_ty = parameter_type(context, callee, index);
            match &parameter.abi {
                LMIRParameterABI::Direct { slots } if parameter.ty.is_memory_resident() => {
                    let address = values::lower_rvalue(context, argument, param_ty);
                    for slot in slots {
                        let source = memory::offset(context, address.clone(), slot.offset as i64);
                        lowered.push(memory::temp(
                            context,
                            LMIRInstructionKind::Load {
                                memory: source,
                                ty: slot.ty.clone(),
                            },
                            slot.ty.clone(),
                        ));
                    }
                }
                LMIRParameterABI::Direct { slots } if slots.is_empty() => {}
                LMIRParameterABI::Direct { .. } => {
                    lowered.push(values::lower_rvalue(context, argument, param_ty));
                }
                LMIRParameterABI::Indirect { .. } | LMIRParameterABI::ByValue { .. } => {
                    let source = values::lower_rvalue(context, argument, param_ty);
                    let copy = memory::allocate(context, param_ty);
                    memory::store(context, copy.clone(), source, param_ty);
                    lowered.push(copy);
                }
            }
        } else {
            lowered.push(values::lower_read(context, argument));
        }
    }
    let kind = if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
        LMIRInstructionKind::DirectCall {
            func: context
                .global
                .unit
                .function(*id)
                .unwrap()
                .prototype()
                .symbol_name
                .clone(),
            args: lowered,
            method_sig: signature.clone(),
        }
    } else {
        let ty = callee_type(context, callee);
        let value_ty = match context.types().definition(ty).unwrap().kind() {
            MIRTypeKind::MemoryReference { inner, .. } => *inner,
            _ => ty,
        };
        LMIRInstructionKind::IndirectCall {
            func_ptr: values::lower_rvalue(context, callee, value_ty),
            args: lowered,
            method_sig: signature.clone(),
        }
    };
    if let Some(result) = out {
        if let Some(address) = sret {
            memory::void(context, kind);
            memory::assign(
                context,
                result,
                LMIRInstructionKind::Alias { value: address },
            );
        } else if context
            .ty(context.body.register(result).unwrap().ty)
            .is_memory_resident()
        {
            let ty = context.body.register(result).unwrap().ty;
            let returned = memory::temp(context, kind, context.ty(ty));
            let address = memory::allocate(context, ty);
            memory::void(
                context,
                LMIRInstructionKind::Store {
                    memory: address.clone(),
                    value: returned,
                    ty: context.ty(ty),
                },
            );
            memory::assign(
                context,
                result,
                LMIRInstructionKind::Alias { value: address },
            );
        } else {
            memory::assign(context, result, kind);
        }
    } else {
        memory::void(context, kind);
    }
}

pub(super) fn lower_return(context: &mut FunctionContext<'_, '_>, value: Option<&MIRValue>) {
    let return_ty = context.function.prototype().signature.return_type();
    let lowered = value.map(|value| values::lower_rvalue(context, value, return_ty));
    if let (LMIRReturnABI::IndirectSret { alignment }, Some(source)) =
        (&context.prototype.signature.return_abi, lowered.as_ref())
    {
        let size = calculate_type_layout(context.types(), return_ty).size();
        memory::void(
            context,
            LMIRInstructionKind::Memcpy {
                dest: LMIRValue::ParameterRef(0),
                src: source.clone(),
                size: context.integer(size as i128, LMIRIntegerType::I64),
                alignment: *alignment,
            },
        );
        memory::void(context, LMIRInstructionKind::Return { value: None });
    } else {
        memory::void(context, LMIRInstructionKind::Return { value: lowered });
    }
}

fn signature(context: &FunctionContext<'_, '_>, callee: &MIRValue) -> LMIRFunctionSignature {
    if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
        let name = context
            .global
            .unit
            .function(*id)
            .unwrap()
            .prototype()
            .symbol_name
            .as_str();
        return context.global.prototypes[name].signature.clone();
    }
    let ty = callee_type(context, callee);
    classify_signature(callable(context, ty), context.types())
}

fn parameter_type(context: &FunctionContext<'_, '_>, callee: &MIRValue, index: usize) -> MIRTypeID {
    if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
        return context
            .global
            .unit
            .function(*id)
            .unwrap()
            .prototype()
            .signature
            .params()[index]
            .ty();
    }
    let ty = callee_type(context, callee);
    callable(context, ty).params()[index].ty()
}

fn callee_type(context: &FunctionContext<'_, '_>, callee: &MIRValue) -> MIRTypeID {
    match callee {
        MIRValue::Register(id) => context.body.register(*id).unwrap().ty,
        MIRValue::PlaceRef(id) => context.body.place(*id).unwrap().ty,
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => reference.ty,
        _ => panic!("indirect call has no function type"),
    }
}

fn callable<'a>(context: &'a FunctionContext<'_, '_>, ty: MIRTypeID) -> &'a MIRFnSignature {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Function { signature } => signature,
        MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner, .. } => {
            callable(context, *inner)
        }
        _ => panic!("call target is not a function"),
    }
}
