use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::MIRTypeKind;
use cx_mir::{MIRInternalIntrinsic, MIRVAIntrinsic};
use cx_util::identifier::CXIdent;

use crate::context::FunctionContext;
use crate::lowering::values::{global_address, lower_read, lower_value, write_target};

use super::output;

pub(super) fn lower(context: &mut FunctionContext<'_, '_>, op: &MIRInternalIntrinsic) {
    use MIRInternalIntrinsic as I;
    match op {
        I::PlaceAddress { out, place } => {
            write_target(context, *out, context.places[place].clone());
        }
        I::GlobalAddress { out, global } => {
            let address = global_address(context, *global);
            write_target(context, *out, address);
        }
        I::ReferenceAddress { out, reference } => {
            let ty = match reference {
                cx_mir::MIRValue::Register(id) => Some(context.body.register(*id).unwrap().ty),
                cx_mir::MIRValue::PlaceRef(id) => Some(context.body.place(*id).unwrap().ty),
                _ => None,
            };
            let inner = ty.and_then(|ty| match context.types().definition(ty).unwrap().kind() {
                MIRTypeKind::MemoryReference { inner, .. }
                    if matches!(
                        context.types().definition(*inner).unwrap().kind(),
                        MIRTypeKind::MemoryReference { .. }
                    ) =>
                {
                    Some(*inner)
                }
                _ => None,
            });
            let address = lower_value(context, reference);
            let value = if let Some(inner) = inner {
                context.load(address, inner)
            } else {
                address
            };
            write_target(context, *out, value);
        }
        I::ArrayAddress { out, array } => {
            let address = lower_value(context, array);
            write_target(context, *out, address);
        }
        I::StringAddress { out, string } => {
            let address = LMIRValue::Global(context.global.string(string));
            write_target(context, *out, address);
        }
        I::GetFnPtr { out, fn_id } => {
            let name = context
                .global
                .unit
                .function(*fn_id)
                .unwrap()
                .prototype()
                .symbol_name
                .to_string();
            output(
                context,
                *out,
                LMIRInstructionKind::GetFunctionAddr { func: name },
            );
        }
        I::Bitcast {
            out,
            value,
            target_ty,
        } => {
            let source = match value {
                cx_mir::MIRValue::Register(id) => {
                    let ty = context.body.register(*id).unwrap().ty;
                    match context.types().definition(ty).unwrap().kind() {
                        MIRTypeKind::MemoryReference { inner, .. }
                            if matches!(
                                context.types().definition(*target_ty).unwrap().kind(),
                                MIRTypeKind::Integer { .. } | MIRTypeKind::Float { .. }
                            ) =>
                        {
                            Some(*inner)
                        }
                        _ => None,
                    }
                }
                _ => None,
            };
            let value = if let Some(ty) = source {
                let address = lower_value(context, value);
                context.load(address, ty)
            } else {
                lower_read(context, value)
            };
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::BitCast,
                },
            );
        }
        I::Assume { condition } => {
            let condition = lower_read(context, condition);
            context.void(LMIRInstructionKind::CompilerAssumption { condition });
        }
        I::Assert { condition, message } => {
            let condition = lower_read(context, condition);
            let message = LMIRValue::Global(
                context
                    .global
                    .string(message.as_deref().unwrap_or("assertion failed")),
            );
            let symbol = ASSERTION.symbol_name();
            let signature = context.global.prototypes[&symbol].signature.clone();
            context.void(LMIRInstructionKind::DirectCall {
                func: CXIdent::new(symbol),
                args: vec![condition, message],
                method_sig: signature,
            });
        }
    }
}

pub(super) fn variadic(context: &mut FunctionContext<'_, '_>, op: &MIRVAIntrinsic) {
    match op {
        MIRVAIntrinsic::VaStart { list, last } => {
            let list = lower_value(context, list);
            let last = lower_value(context, last);
            context.void(LMIRInstructionKind::VaStart { list, last });
        }
        MIRVAIntrinsic::VaEnd { list } => {
            let list = lower_value(context, list);
            context.void(LMIRInstructionKind::VaEnd { list });
        }
        MIRVAIntrinsic::VaArg { out, list, ty } => {
            let list = lower_value(context, list);
            output(
                context,
                *out,
                LMIRInstructionKind::VaArg {
                    list,
                    _type: context.ty(*ty),
                },
            );
        }
    }
}
