use crate::lowering::memory;
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRValue};
use cx_mir::{MIRInternalIntrinsic, MIRVAIntrinsic};

use crate::context::FunctionContext;
use crate::lowering::values::{global_address, read, write_target};

use super::output;

pub(super) fn lower(context: &mut FunctionContext<'_, '_>, op: &MIRInternalIntrinsic) {
    use MIRInternalIntrinsic as I;
    match op {
        I::AdoptPlace { place, address } => {
            let result = context.places[place].clone();
            let address = read(context, address);
            let LMIRValue::Register { register, ty } = result else {
                unreachable!("adopted place must have a reserved pointer register")
            };
            context.emit(
                LMIRInstructionKind::Alias { value: address },
                ty,
                Some(register),
            );
        }
        I::PlaceAddress { out, place } => {
            write_target(context, out, context.places[place].clone(), None);
        }
        I::GlobalAddress { out, global } => {
            let address = global_address(context, *global);
            write_target(context, out, address, None);
        }
        I::ReferenceAddress { out, reference } => {
            let address = read(context, reference);
            write_target(context, out, address, None);
        }
        I::ArrayAddress { out, array } => {
            let address = read(context, array);
            write_target(context, out, address, None);
        }
        I::StringAddress { out, string } => {
            let address = LMIRValue::Global(context.global.string(string));
            write_target(context, out, address, None);
        }
        I::Bitcast { out, value, .. } => {
            let value = read(context, value);
            output(
                context,
                out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::BitCast,
                },
            );
        }
        I::Assume { condition } => {
            let condition = read(context, condition);
            memory::void(
                context,
                LMIRInstructionKind::CompilerAssumption { condition },
            );
        }
        I::Assert { condition, message } => {
            let condition = read(context, condition);
            let message = LMIRValue::Global(
                context
                    .global
                    .string(message.as_deref().unwrap_or("assertion failed")),
            );
            memory::void(context, LMIRInstructionKind::Assert { condition, message });
        }
    }
}

pub(super) fn variadic(context: &mut FunctionContext<'_, '_>, op: &MIRVAIntrinsic) {
    match op {
        MIRVAIntrinsic::VaStart { list, last } => {
            let list = read(context, list);
            let last = read(context, last);
            memory::void(context, LMIRInstructionKind::VaStart { list, last });
        }
        MIRVAIntrinsic::VaEnd { list } => {
            let list = read(context, list);
            memory::void(context, LMIRInstructionKind::VaEnd { list });
        }
        MIRVAIntrinsic::VaArg { out, list, ty } => {
            let list = read(context, list);
            output(
                context,
                out,
                LMIRInstructionKind::VaArg {
                    list,
                    ty: context.ty(*ty),
                },
            );
        }
    }
}
