use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{
    LMIRCoercionType, LMIRFloatBinOp, LMIRFloatUnOp, LMIRFunctionSignature, LMIRGlobalType,
    LMIRGlobalValue, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRParameter,
    LMIRParameterABI, LMIRPtrBinOp, LMIRReturnABI, LMIRValue, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::tagged_union_tag_offset;
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBinaryOp, MIRCallKind, MIRCoercion, MIRConstant,
    MIRFloatBinaryOp, MIRFnParam, MIRFnSignature, MIRFunctionMode, MIRFunctionType, MIRInstrKind,
    MIRIntBinaryOp, MIRIntType, MIRPlaceAggregateOp, MIRPointerBinaryOp, MIRPointerOffsetOp,
    MIRRegister, MIRTypeID, MIRTypeKind, MIRUnaryOp, MIRValue, MIRValueAggregateOp,
};
use cx_util::identifier::CXIdent;

use crate::context::{FunctionLoweringContext, PlaceBinding};

use super::memory::{
    address, binding_for_place, binding_type, field_binding, is_address_valued, load_binding,
    load_discriminant, lower_target, lower_value, store_address, store_binding, unreachable_target,
    value_as_binding,
};
use super::output::{
    allocate_temp, emit_kind_to, emit_temp, emit_to, emit_void, int_constant, integer_kind,
    lowered_type, mir_layout, offset_address, place_decl_type, register_decl_type, register_value,
};
use super::typing::{classify_signature, convert_float_type, convert_integer_type};

fn call_signature(
    context: &FunctionLoweringContext<'_>,
    callee: &MIRValue,
) -> LMIRFunctionSignature {
    if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
        let name = context
            .unit()
            .function(*id)
            .unwrap()
            .prototype()
            .signature
            .symbol_name
            .as_str();
        return context.prototypes().get(name).unwrap().signature.clone();
    }
    let ty = value_type(context, callee).expect("indirect callee has no type");
    let signature = callable_type(context, ty).expect("indirect callee is not callable");
    let mir_signature = MIRFnSignature {
        symbol_name: CXIdent::new("<indirect>"),
        debug_name: None,
        params: signature
            .params
            .iter()
            .copied()
            .map(MIRFnParam::new)
            .collect(),
        return_type: signature.return_type,
        variadic: signature.variadic,
        safe: false,
        mode: MIRFunctionMode::Runtime,
        return_staged_params: None,
    };
    classify_signature(&mir_signature, context.types())
}

fn callable_type<'a>(
    context: &'a FunctionLoweringContext<'_>,
    ty: MIRTypeID,
) -> Option<&'a MIRFunctionType> {
    match context.types().kind(ty).unwrap() {
        MIRTypeKind::Function { signature } => Some(signature),
        MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner, .. } => {
            callable_type(context, *inner)
        }
        _ => None,
    }
}

fn value_type(context: &FunctionLoweringContext<'_>, value: &MIRValue) -> Option<MIRTypeID> {
    match value {
        MIRValue::Register(register) => Some(register_decl_type(context, *register)),
        MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
            Some(place_decl_type(context, *place))
        }
        MIRValue::Constant(MIRConstant::Function(id)) => {
            let function = context.unit().function(*id)?;
            let signature = MIRFunctionType {
                params: function
                    .prototype()
                    .signature
                    .params
                    .iter()
                    .map(|param| param.ty)
                    .collect(),
                return_type: function.prototype().signature.return_type,
                variadic: function.prototype().signature.variadic,
            };
            context
                .types()
                .find_kind(&MIRTypeKind::Function { signature })
        }
        _ => None,
    }
}

fn value_is_pointer(context: &FunctionLoweringContext<'_>, value: &MIRValue) -> bool {
    let ty = match value {
        MIRValue::Constant(
            MIRConstant::Null { ty }
            | MIRConstant::Global { ty, .. }
            | MIRConstant::GlobalOffset { ty, .. },
        ) => *ty,
        MIRValue::Constant(_) => return false,
        _ => match value_type(context, value) {
            Some(ty) => ty,
            None => return false,
        },
    };

    matches!(
        context.types().kind(ty),
        Ok(MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. })
    )
}

fn switch_constant(constant: &MIRConstant) -> u64 {
    match constant {
        MIRConstant::Bool(value) => u64::from(*value),
        MIRConstant::Integer { value, .. } => *value as u64,
        _ => panic!("non-integer MIR switch constant"),
    }
}

fn lower_int_binop(op: MIRIntBinaryOp) -> LMIRIntBinOp {
    match op {
        MIRIntBinaryOp::Add => LMIRIntBinOp::ADD,
        MIRIntBinaryOp::Sub => LMIRIntBinOp::SUB,
        MIRIntBinaryOp::Mul => LMIRIntBinOp::MUL,
        MIRIntBinaryOp::SignedMul => LMIRIntBinOp::IMUL,
        MIRIntBinaryOp::Div => LMIRIntBinOp::UDIV,
        MIRIntBinaryOp::SignedDiv => LMIRIntBinOp::IDIV,
        MIRIntBinaryOp::Mod => LMIRIntBinOp::UREM,
        MIRIntBinaryOp::SignedMod => LMIRIntBinOp::IREM,
        MIRIntBinaryOp::Eq => LMIRIntBinOp::EQ,
        MIRIntBinaryOp::Ne => LMIRIntBinOp::NE,
        MIRIntBinaryOp::Lt => LMIRIntBinOp::ULT,
        MIRIntBinaryOp::Le => LMIRIntBinOp::ULE,
        MIRIntBinaryOp::Gt => LMIRIntBinOp::UGT,
        MIRIntBinaryOp::Ge => LMIRIntBinOp::UGE,
        MIRIntBinaryOp::SignedLt => LMIRIntBinOp::ILT,
        MIRIntBinaryOp::SignedLe => LMIRIntBinOp::ILE,
        MIRIntBinaryOp::SignedGt => LMIRIntBinOp::IGT,
        MIRIntBinaryOp::SignedGe => LMIRIntBinOp::IGE,
        MIRIntBinaryOp::LogicalAnd => LMIRIntBinOp::LAND,
        MIRIntBinaryOp::LogicalOr => LMIRIntBinOp::LOR,
        MIRIntBinaryOp::BitAnd => LMIRIntBinOp::BAND,
        MIRIntBinaryOp::BitOr => LMIRIntBinOp::BOR,
        MIRIntBinaryOp::BitXor => LMIRIntBinOp::BXOR,
        MIRIntBinaryOp::ShiftLeft => LMIRIntBinOp::SHL,
        MIRIntBinaryOp::ArithmeticShiftRight => LMIRIntBinOp::ASHR,
        MIRIntBinaryOp::LogicalShiftRight => LMIRIntBinOp::LSHR,
    }
}

fn lower_float_binop(op: MIRFloatBinaryOp) -> LMIRFloatBinOp {
    match op {
        MIRFloatBinaryOp::Add => LMIRFloatBinOp::ADD,
        MIRFloatBinaryOp::Sub => LMIRFloatBinOp::SUB,
        MIRFloatBinaryOp::Mul => LMIRFloatBinOp::FMUL,
        MIRFloatBinaryOp::Div => LMIRFloatBinOp::FDIV,
        MIRFloatBinaryOp::Eq => LMIRFloatBinOp::EQ,
        MIRFloatBinaryOp::Ne => LMIRFloatBinOp::NEQ,
        MIRFloatBinaryOp::Lt => LMIRFloatBinOp::FLT,
        MIRFloatBinaryOp::Le => LMIRFloatBinOp::FLE,
        MIRFloatBinaryOp::Gt => LMIRFloatBinOp::FGT,
        MIRFloatBinaryOp::Ge => LMIRFloatBinOp::FGE,
    }
}

pub(super) fn lower_instruction(
    context: &mut FunctionLoweringContext<'_>,
    instruction: &MIRInstrKind,
) {
    match instruction {
        MIRInstrKind::ScopeEnter { .. }
        | MIRInstrKind::ScopeExit { .. }
        | MIRInstrKind::Initialize { .. }
        | MIRInstrKind::Leak { .. }
        | MIRInstrKind::MakeStaged { .. }
        | MIRInstrKind::ApplyStaged { .. }
        | MIRInstrKind::StagedReturn { .. }
        | MIRInstrKind::StagedMove { .. }
        | MIRInstrKind::StagedUse { .. } => {}
        MIRInstrKind::Create { out, ty } => {
            let lowered = lowered_type(context, *ty);
            let layout = mir_layout(context, *ty);
            let address = allocate_temp(context, &lowered, layout.alignment as u8);
            context.bind_place(
                *out,
                PlaceBinding::Address {
                    value: address,
                    ty: *ty,
                },
            );
        }
        MIRInstrKind::Assign { target, value, ty } => {
            let value = lower_value(context, value);
            match target {
                MIRAssignTarget::Place(place) => {
                    store_binding(context, binding_for_place(context, *place), value, *ty);
                }
                MIRAssignTarget::Register(register) => {
                    emit_to(context, *register, LMIRInstructionKind::Alias { value });
                }
            }
        }
        MIRInstrKind::AddressOf { out, place } => {
            let binding = binding_for_place(context, *place);
            let address = match binding {
                PlaceBinding::Address { value, .. } => value,
                PlaceBinding::Bitfield { .. } => panic!("cannot take address of bitfield"),
            };
            emit_to(context, *out, LMIRInstructionKind::Alias { value: address });
        }
        MIRInstrKind::Dereference {
            out,
            pointer,
            pointee_type,
        } => {
            let value = lower_value(context, pointer);
            context.bind_place(
                *out,
                PlaceBinding::Address {
                    value,
                    ty: *pointee_type,
                },
            );
        }
        MIRInstrKind::AggregateOp(operation) => lower_aggregate(context, operation),
        MIRInstrKind::Call {
            out,
            kind: MIRCallKind::Runtime,
            callee,
            args,
        } => lower_call(context, *out, callee, args),
        MIRInstrKind::Call {
            kind: MIRCallKind::Comptime,
            ..
        } => panic!("unresolved comptime call reached LMIR lowering"),
        MIRInstrKind::VaStart { list, last } => {
            let list = lower_value(context, list);
            let last = lower_value(context, last);
            emit_void(context, LMIRInstructionKind::VaStart { list, last });
        }
        MIRInstrKind::VaEnd { list } => {
            let list = lower_value(context, list);
            emit_void(context, LMIRInstructionKind::VaEnd { list });
        }
        MIRInstrKind::VaArg { out, list, ty } => {
            let list = lower_value(context, list);
            emit_to(
                context,
                *out,
                LMIRInstructionKind::VaArg {
                    list,
                    _type: lowered_type(context, *ty),
                },
            );
        }
        MIRInstrKind::BinOp { out, op, lhs, rhs } => lower_binary(context, *out, op, lhs, rhs),
        MIRInstrKind::UnOp { out, op, operand } => lower_unary(context, *out, op, operand),
        MIRInstrKind::Coerce {
            out,
            operand,
            coercion,
            to_type,
        } => lower_coercion(context, *out, operand, coercion, *to_type),
        MIRInstrKind::Assert { condition, message } => {
            lower_assert(context, condition, message.as_deref())
        }
        MIRInstrKind::Assume { condition } => {
            let condition = lower_value(context, condition);
            emit_void(
                context,
                LMIRInstructionKind::CompilerAssumption { condition },
            );
        }
        MIRInstrKind::Return { value } => lower_return(context, value.as_ref()),
        MIRInstrKind::Jump { target } => {
            let target = lower_target(context, target);
            emit_void(context, LMIRInstructionKind::Jump { target });
        }
        MIRInstrKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            let condition = lower_value(context, cond);
            let true_target = lower_target(context, true_target);
            let false_target = lower_target(context, false_target);
            emit_void(
                context,
                LMIRInstructionKind::Branch {
                    condition,
                    true_target,
                    false_target,
                },
            );
        }
        MIRInstrKind::IntSwitch {
            value,
            cases,
            default,
        } => {
            let value = lower_value(context, value);
            let targets = cases
                .iter()
                .map(|(case, target)| (switch_constant(case), lower_target(context, target)))
                .collect();
            let default = default
                .as_ref()
                .map(|target| lower_target(context, target))
                .unwrap_or_else(|| unreachable_target(context));
            emit_void(
                context,
                LMIRInstructionKind::JumpTable {
                    value,
                    targets,
                    default,
                },
            );
        }
        MIRInstrKind::VariantSwitch {
            subject,
            sum_type,
            cases,
            default,
        } => {
            let binding = value_as_binding(context, subject, *sum_type);
            let tag = load_discriminant(context, binding, *sum_type, None);
            let targets = cases
                .iter()
                .map(|(case, target)| (*case as u64, lower_target(context, target)))
                .collect();
            let default = default
                .as_ref()
                .map(|target| lower_target(context, target))
                .unwrap_or_else(|| unreachable_target(context));
            emit_void(
                context,
                LMIRInstructionKind::JumpTable {
                    value: tag,
                    targets,
                    default,
                },
            );
        }
        MIRInstrKind::Unreachable => emit_void(context, LMIRInstructionKind::Unreachable),
    }
}

fn lower_aggregate(context: &mut FunctionLoweringContext<'_>, operation: &MIRAggregateOp) {
    match operation {
        MIRAggregateOp::Place { out, op } => {
            let binding = match op {
                MIRPlaceAggregateOp::Field {
                    base,
                    field,
                    aggregate_type,
                } => {
                    let target = aggregate_target(context, *aggregate_type);
                    field_binding(
                        context,
                        binding_for_place(context, *base),
                        target.ty,
                        *field,
                    )
                }
                MIRPlaceAggregateOp::Index {
                    base,
                    index,
                    element_type,
                } => {
                    let base = address(context, binding_for_place(context, *base));
                    let index = lower_value(context, index);
                    let element = lowered_type(context, *element_type);
                    let address = emit_temp(
                        context,
                        LMIRInstructionKind::PointerBinOp {
                            op: LMIRPtrBinOp::ADD,
                            ptr_type: element.clone(),
                            type_size: TypeSize::from(mir_layout(context, *element_type).size),
                            left: base,
                            right: index,
                        },
                        LMIRType::default_pointer(context.types().architecture()),
                    );
                    PlaceBinding::Address {
                        value: address,
                        ty: *element_type,
                    }
                }
                MIRPlaceAggregateOp::Variant {
                    base,
                    variant,
                    sum_type,
                } => {
                    let base = address(context, binding_for_place(context, *base));
                    let target = aggregate_target(context, *sum_type);
                    let MIRTypeKind::TaggedUnion { variants } = context
                        .types()
                        .kind(target.ty)
                        .expect("invalid MIR sum type")
                    else {
                        panic!("variant projection on non-sum type");
                    };

                    PlaceBinding::Address {
                        value: base,
                        ty: variants.get(*variant).expect("invalid variant index").ty(),
                    }
                }
            };
            context.bind_place(*out, binding);
        }
        MIRAggregateOp::Value { out, op } => match op {
            MIRValueAggregateOp::Discriminant { value, sum_type } => {
                let target = aggregate_target(context, *sum_type);
                let binding = value_as_binding(context, value, target.ty);
                load_discriminant(context, binding, target.ty, Some(*out));
            }
            MIRValueAggregateOp::Construct { ty, fields } => {
                lower_construct(context, *out, *ty, fields)
            }
            MIRValueAggregateOp::Variant {
                variant,
                value,
                sum_type,
            } => {
                let target = aggregate_target(context, *sum_type);
                lower_variant_construct(context, *out, *variant, value, target.ty)
            }
            MIRValueAggregateOp::ProjectVariant {
                variant,
                value,
                sum_type,
            } => {
                let target = aggregate_target(context, *sum_type);
                lower_variant_project(context, *out, *variant, value, target)
            }
        },
    }
}

#[derive(Clone, Copy)]
struct AggregateTarget {
    ty: MIRTypeID,
    by_value: bool,
}

fn aggregate_target(context: &FunctionLoweringContext<'_>, ty: MIRTypeID) -> AggregateTarget {
    match context
        .types()
        .kind(ty)
        .expect("invalid MIR aggregate type")
    {
        MIRTypeKind::MemoryReference { inner, .. } => AggregateTarget {
            ty: *inner,
            by_value: false,
        },
        _ => AggregateTarget { ty, by_value: true },
    }
}

fn lower_construct(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    ty: MIRTypeID,
    fields: &[(usize, MIRValue)],
) {
    let lowered = lowered_type(context, ty);
    let layout = mir_layout(context, ty);
    emit_kind_to(
        context,
        out,
        LMIRInstructionKind::Allocate {
            _type: lowered.clone(),
            alignment: layout.alignment as u8,
        },
        lowered.clone(),
    );
    let base = register_value(context, out);
    for (index, value) in fields {
        let (address, field_ty) = match context
            .types()
            .kind(ty)
            .expect("invalid MIR aggregate type")
        {
            MIRTypeKind::Structured { .. } | MIRTypeKind::Union { .. } => {
                let binding = field_binding(
                    context,
                    PlaceBinding::Address {
                        value: base.clone(),
                        ty,
                    },
                    ty,
                    *index,
                );

                match binding {
                    PlaceBinding::Address { value, ty } => (value, ty),
                    bitfield @ PlaceBinding::Bitfield { .. } => {
                        let value = lower_value(context, value);
                        store_binding(context, bitfield, value, register_decl_type(context, out));
                        continue;
                    }
                }
            }

            MIRTypeKind::Array { inner, .. } => {
                let field_ty = *inner;
                let element = lowered_type(context, field_ty);
                (
                    offset_address(
                        context,
                        base.clone(),
                        index * mir_layout(context, field_ty).size,
                        &element,
                    ),
                    field_ty,
                )
            }

            _ => unreachable!("construct aggregate has non-structured, non-array type"),
        };
        let value = lower_value(context, value);
        store_address(context, address, value, field_ty);
    }
}

fn lower_variant_construct(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    variant: usize,
    value: &MIRValue,
    sum_type: MIRTypeID,
) {
    let lowered = lowered_type(context, sum_type);
    let layout = mir_layout(context, sum_type);
    emit_kind_to(
        context,
        out,
        LMIRInstructionKind::Allocate {
            _type: lowered.clone(),
            alignment: layout.alignment as u8,
        },
        lowered,
    );
    let base = register_value(context, out);
    let MIRTypeKind::TaggedUnion { variants } = context
        .types()
        .kind(sum_type)
        .expect("invalid MIR sum type")
    else {
        panic!("variant construction on non-sum type");
    };
    let variant_ty = variants.get(variant).expect("invalid variant index").ty();
    let value = lower_value(context, value);
    store_address(context, base.clone(), value, variant_ty);
    let tag_ty = LMIRType::with_implicit_abi(
        context.types().architecture(),
        LMIRTypeKind::Integer(LMIRIntegerType::I8),
    );
    let tag_address = offset_address(
        context,
        base,
        tagged_union_tag_offset(context.types(), sum_type).unwrap(),
        &tag_ty,
    );
    emit_void(
        context,
        LMIRInstructionKind::Store {
            memory: tag_address,
            value: int_constant(context, variant as i128, LMIRIntegerType::I8),
            _type: tag_ty,
        },
    );
}

fn lower_variant_project(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    variant: usize,
    value: &MIRValue,
    target: AggregateTarget,
) {
    let sum_type = target.ty;
    let MIRTypeKind::TaggedUnion { variants } = context
        .types()
        .kind(sum_type)
        .expect("invalid MIR sum type")
    else {
        panic!("variant projection on non-sum type");
    };
    let variant_type = variants.get(variant).expect("invalid variant index").ty();
    let lowered = lowered_type(context, variant_type);
    if lowered.is_void() {
        emit_to(
            context,
            out,
            LMIRInstructionKind::Alias {
                value: LMIRValue::NULL,
            },
        );
        return;
    }
    let base = if target.by_value {
        let aggregate = lower_value(context, value);
        emit_temp(
            context,
            LMIRInstructionKind::StructAccess {
                struct_: aggregate,
                struct_type: lowered_type(context, sum_type),
                field_index: 0,
                field_offset: 0,
            },
            LMIRType::default_pointer(context.types().architecture()),
        )
    } else {
        let binding = value_as_binding(context, value, sum_type);
        address(context, binding)
    };
    let binding = PlaceBinding::Address {
        value: base,
        ty: variant_type,
    };
    let value = if lowered.is_memory_resident() || is_address_valued(context, variant_type) {
        address(context, binding)
    } else {
        load_binding(context, binding, variant_type, None)
    };
    emit_to(context, out, LMIRInstructionKind::Alias { value });
}

fn lower_binary(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    op: &MIRBinaryOp,
    lhs: &MIRValue,
    rhs: &MIRValue,
) {
    let (lhs, rhs) = if matches!(
        op,
        MIRBinaryOp::PointerOffset {
            op: MIRPointerOffsetOp::Add,
            ..
        }
    ) && !value_is_pointer(context, lhs)
        && value_is_pointer(context, rhs)
    {
        (rhs, lhs)
    } else {
        (lhs, rhs)
    };
    let lhs = lower_value(context, lhs);
    let rhs = lower_value(context, rhs);
    let kind = match op {
        MIRBinaryOp::Integer { op, .. } => LMIRInstructionKind::IntegerBinOp {
            op: lower_int_binop(*op),
            left: lhs,
            right: rhs,
        },
        MIRBinaryOp::Float { op, .. } => LMIRInstructionKind::FloatBinOp {
            op: lower_float_binop(*op),
            left: lhs,
            right: rhs,
        },
        MIRBinaryOp::PointerOffset { op, pointee } => LMIRInstructionKind::PointerBinOp {
            op: match op {
                MIRPointerOffsetOp::Add => LMIRPtrBinOp::ADD,
                MIRPointerOffsetOp::Sub => LMIRPtrBinOp::SUB,
            },
            ptr_type: lowered_type(context, *pointee),
            type_size: TypeSize::from(mir_layout(context, *pointee).size),
            left: lhs,
            right: rhs,
        },
        MIRBinaryOp::Pointer(op) => LMIRInstructionKind::PointerBinOp {
            op: match op {
                MIRPointerBinaryOp::Eq => LMIRPtrBinOp::EQ,
                MIRPointerBinaryOp::Ne => LMIRPtrBinOp::NE,
                MIRPointerBinaryOp::Lt => LMIRPtrBinOp::LT,
                MIRPointerBinaryOp::Le => LMIRPtrBinOp::LE,
                MIRPointerBinaryOp::Gt => LMIRPtrBinOp::GT,
                MIRPointerBinaryOp::Ge => LMIRPtrBinOp::GE,
            },
            ptr_type: LMIRType::default_pointer(context.types().architecture()),
            type_size: TypeSize::from(1),
            left: lhs,
            right: rhs,
        },
    };
    emit_to(context, out, kind);
}

fn lower_unary(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    op: &MIRUnaryOp,
    operand: &MIRValue,
) {
    if let MIRUnaryOp::Increment { amount, post } = op {
        let place_id = match operand {
            MIRValue::PlaceRef(place) => *place,
            _ => panic!("increment requires a place operand"),
        };
        let place = binding_for_place(context, place_id);
        let ty = binding_type(context, &place);
        let previous = load_binding(context, place.clone(), ty, None);
        let amount = i128::from(*amount);
        let (result_kind, result_type) = match context.types().kind(ty).unwrap() {
            MIRTypeKind::PointerTo { inner } => (
                LMIRInstructionKind::PointerBinOp {
                    op: if amount < 0 {
                        LMIRPtrBinOp::SUB
                    } else {
                        LMIRPtrBinOp::ADD
                    },
                    ptr_type: lowered_type(context, *inner),
                    type_size: TypeSize::from(mir_layout(context, *inner).size),
                    left: previous.clone(),
                    right: int_constant(context, amount.abs(), integer_kind(context, ty)),
                },
                lowered_type(context, ty),
            ),

            _ => (
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::ADD,
                    left: previous.clone(),
                    right: int_constant(context, amount, integer_kind(context, ty)),
                },
                lowered_type(context, ty),
            ),
        };
        let result = emit_temp(context, result_kind, result_type);
        store_binding(context, place, result.clone(), ty);
        let value = if matches!(
            context
                .types()
                .kind(register_decl_type(context, out))
                .unwrap(),
            MIRTypeKind::MemoryReference { .. }
        ) {
            address(context, binding_for_place(context, place_id))
        } else if *post {
            previous
        } else {
            result
        };
        emit_to(context, out, LMIRInstructionKind::Alias { value });
        return;
    }
    let value = lower_value(context, operand);
    let kind = match op {
        MIRUnaryOp::IntegerNeg { .. } => LMIRInstructionKind::IntegerUnOp {
            op: LMIRIntUnOp::NEG,
            value,
        },
        MIRUnaryOp::FloatNeg(_) => LMIRInstructionKind::FloatUnOp {
            op: LMIRFloatUnOp::NEG,
            value,
        },
        MIRUnaryOp::BitNot(_) => LMIRInstructionKind::IntegerUnOp {
            op: LMIRIntUnOp::BNOT,
            value,
        },
        MIRUnaryOp::LogicalNot => LMIRInstructionKind::IntegerUnOp {
            op: LMIRIntUnOp::LNOT,
            value,
        },
        MIRUnaryOp::Increment { .. } => unreachable!(),
    };
    emit_to(context, out, kind);
}

fn lower_coercion(
    context: &mut FunctionLoweringContext<'_>,
    out: MIRRegister,
    operand: &MIRValue,
    coercion: &MIRCoercion,
    to_type: MIRTypeID,
) {
    let value = lower_value(context, operand);
    let kind = match coercion {
        MIRCoercion::TypeChange => LMIRInstructionKind::Alias { value },
        MIRCoercion::ReinterpretBits => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::BitCast,
        },
        MIRCoercion::Integral {
            sign_extend,
            from,
            to,
        } => {
            if matches!(to, MIRIntType::I1) {
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::NE,
                    left: value,
                    right: int_constant(context, 0, convert_integer_type(*from)),
                }
            } else {
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: if from.bytes() > to.bytes() {
                        LMIRCoercionType::Trunc
                    } else if *sign_extend {
                        LMIRCoercionType::SExtend
                    } else {
                        LMIRCoercionType::ZExtend
                    },
                }
            }
        }
        MIRCoercion::FloatCast { from, .. } => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::FloatCast {
                from: convert_float_type(*from),
            },
        },
        MIRCoercion::IntToFloat { from, signed, .. } => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::IntToFloat {
                from: convert_integer_type(*from),
                sextend: *signed,
            },
        },
        MIRCoercion::FloatToInt { from, signed, .. } => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::FloatToInt {
                from: convert_float_type(*from),
                sextend: *signed,
            },
        },
        MIRCoercion::PointerToInt { .. } => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::PtrToInt,
        },
        MIRCoercion::IntToPointer { from, sign_extend } => LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::IntToPtr {
                from: convert_integer_type(*from),
                sextend: *sign_extend,
            },
        },
        MIRCoercion::FunctionToPointer => match value {
            LMIRValue::FunctionRef(function) => LMIRInstructionKind::GetFunctionAddr {
                func: function.to_string(),
            },
            value => LMIRInstructionKind::Alias { value },
        },
    };
    emit_kind_to(context, out, kind, lowered_type(context, to_type));
}

fn lower_call(
    context: &mut FunctionLoweringContext<'_>,
    out: Option<MIRRegister>,
    callee: &MIRValue,
    args: &[MIRValue],
) {
    let signature = call_signature(context, callee);
    let mut lowered_args = Vec::new();
    for (index, argument) in args.iter().enumerate() {
        if let Some(parameter) = signature.params.get(index) {
            lowered_args.extend(lower_call_argument(context, argument, parameter));
        } else {
            lowered_args.push(lower_value(context, argument));
        }
    }
    let return_type = out
        .map(|register| register_decl_type(context, register))
        .map(|ty| lowered_type(context, ty))
        .unwrap_or_else(LMIRType::unit);
    let return_buffer = if matches!(signature.return_abi, LMIRReturnABI::IndirectSret { .. }) {
        let semantic = out
            .map(|register| register_decl_type(context, register))
            .expect("indirect return without result");
        let buffer = allocate_temp(
            context,
            &return_type,
            mir_layout(context, semantic).alignment as u8,
        );
        lowered_args.insert(0, buffer.clone());
        Some(buffer)
    } else {
        None
    };
    let call_kind = match callee {
        MIRValue::Constant(MIRConstant::Function(id)) => {
            let function = context.unit().function(*id).expect("invalid direct callee");
            LMIRInstructionKind::DirectCall {
                func: function.prototype().signature.symbol_name.clone(),
                args: lowered_args,
                method_sig: signature.clone(),
            }
        }
        _ => LMIRInstructionKind::IndirectCall {
            func_ptr: lower_value(context, callee),
            args: lowered_args,
            method_sig: signature.clone(),
        },
    };
    match (out, return_buffer) {
        (None, _) => emit_void(context, call_kind),
        (Some(out), Some(buffer)) => {
            emit_void(context, call_kind);
            emit_to(context, out, LMIRInstructionKind::Alias { value: buffer });
        }
        (Some(out), None) if return_type.is_memory_resident() => {
            let call = emit_temp(context, call_kind, return_type.clone());
            let semantic = register_decl_type(context, out);
            emit_kind_to(
                context,
                out,
                LMIRInstructionKind::Allocate {
                    _type: return_type.clone(),
                    alignment: mir_layout(context, semantic).alignment as u8,
                },
                return_type.clone(),
            );
            emit_void(
                context,
                LMIRInstructionKind::Store {
                    memory: register_value(context, out),
                    value: call,
                    _type: return_type,
                },
            );
        }
        (Some(out), None) => emit_kind_to(context, out, call_kind, return_type),
    }
}

fn lower_call_argument(
    context: &mut FunctionLoweringContext<'_>,
    argument: &MIRValue,
    parameter: &LMIRParameter,
) -> Vec<LMIRValue> {
    match &parameter.abi {
        LMIRParameterABI::Direct { slots } if parameter._type.is_memory_resident() => {
            let source = lower_value(context, argument);
            slots
                .iter()
                .map(|slot| {
                    let address = offset_address(context, source.clone(), slot.offset, &slot._type);
                    emit_temp(
                        context,
                        LMIRInstructionKind::Load {
                            memory: address,
                            _type: slot._type.clone(),
                        },
                        slot._type.clone(),
                    )
                })
                .collect()
        }
        LMIRParameterABI::Indirect { alignment } | LMIRParameterABI::ByValue { alignment } => {
            let source = lower_value(context, argument);
            if matches!(argument, MIRValue::PlaceRef(_)) {
                let copy = emit_temp(
                    context,
                    LMIRInstructionKind::Allocate {
                        _type: parameter._type.clone(),
                        alignment: *alignment,
                    },
                    parameter._type.clone(),
                );
                let size = int_constant(
                    context,
                    usize::from(parameter._type.size()) as i128,
                    LMIRIntegerType::I64,
                );
                emit_void(
                    context,
                    LMIRInstructionKind::Memcpy {
                        dest: copy.clone(),
                        src: source,
                        size,
                        alignment: *alignment,
                    },
                );
                vec![copy]
            } else {
                vec![source]
            }
        }
        LMIRParameterABI::Direct { .. } => {
            vec![lower_value(context, argument)]
        }
    }
}

fn lower_return(context: &mut FunctionLoweringContext<'_>, value: Option<&MIRValue>) {
    let return_abi = context.prototype().signature.return_abi.clone();
    match (return_abi, value) {
        (LMIRReturnABI::IndirectSret { alignment }, Some(value)) => {
            let semantic = context.function().prototype().signature.return_type;
            let source = lower_value(context, value);
            let size = int_constant(
                context,
                mir_layout(context, semantic).size as i128,
                LMIRIntegerType::I64,
            );
            emit_void(
                context,
                LMIRInstructionKind::Memcpy {
                    dest: LMIRValue::ParameterRef(0),
                    src: source,
                    size,
                    alignment,
                },
            );
            emit_void(context, LMIRInstructionKind::Return { value: None });
        }
        (_, value) => {
            let value = value.map(|value| lower_value(context, value));
            emit_void(context, LMIRInstructionKind::Return { value });
        }
    }
}

fn lower_assert(
    context: &mut FunctionLoweringContext<'_>,
    condition: &MIRValue,
    message: Option<&str>,
) {
    let condition = lower_value(context, condition);
    let message = message.unwrap_or("assertion failed").to_owned();
    let global = context.globals().len() as u32;
    context.globals_mut().push(LMIRGlobalValue {
        name: CXIdent::new(format!("assert_message_{global}")),
        _type: LMIRGlobalType::StringLiteral(message),
        linkage: LinkageType::Static,
    });
    let assertion_name = ASSERTION.symbol_name();
    let signature = context
        .prototypes()
        .get(&assertion_name)
        .expect("assertion prototype was not installed")
        .signature
        .clone();
    emit_void(
        context,
        LMIRInstructionKind::DirectCall {
            func: CXIdent::new(assertion_name),
            args: vec![condition, LMIRValue::Global(global)],
            method_sig: signature,
        },
    );
}
