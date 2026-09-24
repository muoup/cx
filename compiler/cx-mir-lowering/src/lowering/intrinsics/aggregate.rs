use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{LMIRInstructionKind, LMIRPtrBinOp, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRAggregateIntrinsic, MIRTarget, MIRTypeID, MIRTypeKind};

use crate::context::FunctionContext;
use crate::lowering::values::{
    aggregate_member, field_location, lower_read, lower_rvalue, tagged_union_tag_offset,
    target_type, write_target,
};

pub(super) fn lower(context: &mut FunctionContext<'_, '_>, op: &MIRAggregateIntrinsic) {
    use MIRAggregateIntrinsic as A;
    match op {
        A::AggregateInit { out, ty, fields } => {
            let address = memory::allocate(context, *ty);
            memory::void(
                context,
                LMIRInstructionKind::ZeroMemory {
                    memory: address.clone(),
                    _type: context.ty(*ty),
                },
            );
            let kind = context.types().definition(*ty).unwrap().kind().clone();
            for (index, field) in fields {
                if matches!(kind, MIRTypeKind::TaggedUnion { .. }) {
                    let tag = memory::offset(
                        context,
                        address.clone(),
                        tagged_union_tag_offset(context, *ty) as i64,
                    );
                    memory::void(
                        context,
                        LMIRInstructionKind::Store {
                            memory: tag,
                            value: context.integer(*index as i128, LMIRIntegerType::I8),
                            _type: LMIRType::with_implicit_abi(
                                context.types().architecture(),
                                LMIRTypeKind::Integer(LMIRIntegerType::I8),
                            ),
                        },
                    );
                }
                let (offset, field_ty) = aggregate_member(context, *ty, *index);
                let destination = memory::offset(context, address.clone(), offset as i64);
                let value = lower_rvalue(context, field, field_ty);
                memory::store(context, destination, value, field_ty);
            }
            write_target(context, *out, address);
        }
        A::StructField {
            out,
            base,
            field,
            struct_ty,
        } => {
            let (offset, field_ty, _) = field_location(context, *struct_ty, *field);
            let base = lower_read(context, base);
            let address = memory::offset(context, base, offset as i64);
            write_projection(context, *out, address, field_ty, false);
        }
        A::ArrayIndex {
            out,
            base,
            index,
            element_ty,
        } => {
            let base = lower_read(context, base);
            let index = lower_read(context, index);
            let stride = calculate_type_layout(context.types(), *element_ty).size();
            let address = memory::temp(
                context,
                LMIRInstructionKind::PointerBinOp {
                    op: LMIRPtrBinOp::ADD,
                    ptr_type: context.ty(*element_ty),
                    type_size: TypeSize::from(stride),
                    left: base,
                    right: index,
                },
                context.pointer(),
            );
            write_projection(context, *out, address, *element_ty, false);
        }
        A::SumIndex { out, value, sum_ty } => {
            let base = lower_read(context, value);
            let address = memory::offset(
                context,
                base,
                tagged_union_tag_offset(context, *sum_ty) as i64,
            );
            let tag_type = LMIRType::with_implicit_abi(
                context.types().architecture(),
                LMIRTypeKind::Integer(LMIRIntegerType::I8),
            );
            let value = memory::temp(
                context,
                LMIRInstructionKind::Load {
                    memory: address,
                    _type: tag_type.clone(),
                },
                tag_type,
            );
            write_target(context, *out, value);
        }
        A::SumVariant {
            out,
            base,
            variant,
            sum_ty,
        } => {
            let address = lower_read(context, base);
            let payload = variant_type(context, *sum_ty, *variant);
            let as_value = target_type(context, *out) == payload;
            write_projection(context, *out, address, payload, as_value);
        }
        A::SumVariantL {
            out,
            source,
            variant,
            sum_ty,
        } => {
            let address = lower_read(context, source);
            let payload = variant_type(context, *sum_ty, *variant);
            let value = memory::load(context, address, payload);
            let destination = context.places[out].clone();
            memory::store(context, destination, value, payload);
        }
    }
}

fn write_projection(
    context: &mut FunctionContext<'_, '_>,
    target: MIRTarget,
    address: LMIRValue,
    value_ty: MIRTypeID,
    as_value: bool,
) {
    let result_ty = target_type(context, target);
    let value = if !as_value && matches!(
        context.types().definition(result_ty).unwrap().kind(),
        MIRTypeKind::MemoryReference { .. }
    ) {
        address
    } else {
        memory::load(context, address, value_ty)
    };
    write_target(context, target, value);
}

fn variant_type(context: &FunctionContext<'_, '_>, ty: MIRTypeID, variant: usize) -> MIRTypeID {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::TaggedUnion { variants } => variants[variant].ty(),
        _ => panic!("variant access on non-tagged union"),
    }
}
