use crate::lowering::memory;
use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRValue};
use cx_mir::{MIRInternalIntrinsic, MIRVAIntrinsic};
use cx_util::identifier::CXIdent;

use crate::context::FunctionContext;
use crate::lowering::values::{global_address, lower_read, lower_value, write_target};

use super::output;

pub(super) fn lower(context: &mut FunctionContext<'_, '_>, op: &MIRInternalIntrinsic) {
    use MIRInternalIntrinsic as I;
    match op {
        I::AdoptPlace { place, address } => {
            let result = context.places[place].clone();
            let address = lower_value(context, address);
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
            write_target(context, *out, context.places[place].clone());
        }
        I::GlobalAddress { out, global } => {
            let address = global_address(context, *global);
            write_target(context, *out, address);
        }
        I::ReferenceAddress { out, reference } => {
            let address = lower_value(context, reference);
            write_target(context, *out, address);
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
        I::Bitcast { out, value, .. } => {
            let value = lower_read(context, value);
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
            memory::void(
                context,
                LMIRInstructionKind::CompilerAssumption { condition },
            );
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
            memory::void(
                context,
                LMIRInstructionKind::DirectCall {
                    func: CXIdent::new(symbol),
                    args: vec![condition, message],
                    method_sig: signature,
                },
            );
        }
    }
}

pub(super) fn variadic(context: &mut FunctionContext<'_, '_>, op: &MIRVAIntrinsic) {
    match op {
        MIRVAIntrinsic::VaStart { list, last } => {
            let list = lower_value(context, list);
            let last = lower_value(context, last);
            memory::void(context, LMIRInstructionKind::VaStart { list, last });
        }
        MIRVAIntrinsic::VaEnd { list } => {
            let list = lower_value(context, list);
            memory::void(context, LMIRInstructionKind::VaEnd { list });
        }
        MIRVAIntrinsic::VaArg { out, list, ty } => {
            let list = lower_value(context, list);
            output(
                context,
                *out,
                LMIRInstructionKind::VaArg {
                    list,
                    ty: context.ty(*ty),
                },
            );
        }
    }
}
