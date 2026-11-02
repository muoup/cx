use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{LMIRInstructionKind, LMIRPtrBinOp};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRAggregateIntrinsic, MIRTarget, MIRTypeKind};

use crate::context::FunctionContext;
use crate::lowering::values::{
    member_location, read, store_member, tagged_union_tag_offset, write_target,
};

pub(super) fn lower(context: &mut FunctionContext<'_, '_>, op: &MIRAggregateIntrinsic) {
    use MIRAggregateIntrinsic as A;
    match op {
        A::AggregateInit { out, ty, fields } => {
            let address = match out {
                MIRTarget::Place(place) => context.places[place].clone(),
                _ => memory::allocate(context, *ty),
            };
            memory::void(
                context,
                LMIRInstructionKind::ZeroMemory {
                    memory: address.clone(),
                    ty: context.ty(*ty),
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
                            ty: LMIRType::with_implicit_abi(
                                context.types().architecture(),
                                LMIRTypeKind::Integer(LMIRIntegerType::I8),
                            ),
                        },
                    );
                }
                let location = member_location(context, *ty, *index);
                let value = read(context, field);
                store_member(context, address.clone(), location, value);
            }
            if !matches!(out, MIRTarget::Place(_)) {
                write_target(context, *out, address, None);
            }
        }
        A::StructField {
            out,
            base,
            field,
            struct_ty,
        } => {
            // A bitfield projects to its storage unit; bitfield-aware stores extract from it
            let (offset, _, _) = member_location(context, *struct_ty, *field);
            let base = read(context, base);
            let address = memory::offset(context, base, offset as i64);
            write_target(context, *out, address, None);
        }
        A::ArrayIndex {
            out,
            base,
            index,
            element_ty,
        } => {
            let base = read(context, base);
            let index = read(context, index);
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
            write_target(context, *out, address, None);
        }
        A::SumIndex { out, value, sum_ty } => {
            let base = read(context, value);
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
                    ty: tag_type.clone(),
                },
                tag_type,
            );
            write_target(context, *out, value, None);
        }
        A::SumVariant { out, base, .. } => {
            let address = read(context, base);
            write_target(context, *out, address, None);
        }
    }
}
