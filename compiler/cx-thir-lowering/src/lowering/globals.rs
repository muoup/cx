use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRConstant, MIRFloatType, MIRGlobalID, MIRGlobalState, MIRInstrKind, MIRTypeID, MIRTypeKind,
};
use cx_thir::thir::{
    expression::{THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRPtrDiffBinOp},
    global::THIRGlobalVariable,
    r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind},
};
use cx_util::linkage::LinkageMode;

use crate::{
    builder::{MIRBuilder, integer_type},
    lowering::types::{lower_int_type, lower_type},
};

pub(crate) fn lower_global_initializer(
    builder: &mut MIRBuilder<'_>,
    function: cx_mir::MIRFunctionID,
    global: &THIRGlobalVariable,
) -> CXResult<()> {
    let initializer = global
        .initializer
        .as_ref()
        .expect("global initialization request has no initializer");
    builder.start_global_initializer(function, initializer);
    let value = super::lower_expression(builder, initializer)?;
    builder.emit(MIRInstrKind::Return { value: Some(value) });
    builder.finish_function();
    Ok(())
}

pub(crate) fn lower_global(
    builder: &mut MIRBuilder<'_>,
    id: MIRGlobalID,
    global: &THIRGlobalVariable,
) {
    let state = if global.linkage == LinkageMode::Extern {
        MIRGlobalState::External
    } else {
        MIRGlobalState::ZeroInitialized
    };

    builder.set_global_state(id, state);
}

pub(crate) fn evaluate_global_initializer(
    builder: &mut MIRBuilder<'_>,
    id: MIRGlobalID,
    global: &THIRGlobalVariable,
) {
    let initializer = global
        .initializer
        .as_ref()
        .expect("global comptime initializer is missing");
    let ty = lower_type(builder, &global._type);
    let constant = lower_global_constant(builder, initializer, ty);
    builder.set_global_state(id, MIRGlobalState::Initialized(constant));
}

fn lower_global_constant(
    builder: &mut MIRBuilder,
    expression: &THIRExpression,
    target: MIRTypeID,
) -> MIRConstant {
    match &expression.kind {
        THIRExpressionKind::Typechange(source) | THIRExpressionKind::Copy { source } => {
            if let Some(value) = string_literal(source) {
                return lower_string_literal(builder, value, target);
            }
            let source_type = lower_type(builder, &source._type);
            let constant = lower_global_constant(builder, source, source_type);
            retarget_constant(constant, target)
        }

        THIRExpressionKind::BoolLiteral(value) => MIRConstant::Bool(*value),
        THIRExpressionKind::FunctionReference { name, .. } => MIRConstant::Function(
            builder
                .function_symbol(name.as_str())
                .unwrap_or_else(|| panic!("function {name} is not declared")),
        ),
        THIRExpressionKind::GlobalVariable { symbol } => {
            let global = builder
                .global_symbol(symbol.as_str())
                .unwrap_or_else(|| panic!("global {symbol} is not declared"));
            MIRConstant::Global { global, ty: target }
        }
        THIRExpressionKind::IntLiteral(value) => {
            let (ty, signed) =
                integer_target(builder, target).unwrap_or_else(|| integer_type(&expression._type));
            MIRConstant::Integer {
                value: *value as i128,
                ty,
                signed,
            }
        }
        THIRExpressionKind::FloatLiteral(value) => MIRConstant::Float {
            value: *value,
            ty: match expression._type.kind {
                THIRTypeKind::Float {
                    _type: THIRFloatType::F32,
                } => MIRFloatType::F32,
                THIRTypeKind::Float {
                    _type: THIRFloatType::F64,
                } => MIRFloatType::F64,
                _ => MIRFloatType::F64,
            },
        },
        THIRExpressionKind::StringLiteral { value } => lower_string_literal(builder, value, target),
        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => lower_global_conversion(builder, operand, conversion, target),
        THIRExpressionKind::BinaryOperation {
            lhs,
            rhs,
            op: THIRBinOp::PtrDiff { op, ptr_inner },
        } => lower_global_offset(builder, lhs, rhs, op, ptr_inner, target),
        THIRExpressionKind::ArrayInitializer { elements, .. } => {
            let element_type = array_element_type(builder, target);
            MIRConstant::Aggregate {
                ty: target,
                fields: elements
                    .iter()
                    .enumerate()
                    .map(|(index, element)| {
                        (index, lower_global_constant(builder, element, element_type))
                    })
                    .collect(),
            }
        }
        THIRExpressionKind::StructInitializer {
            initializations, ..
        } => {
            let field_types = initializations
                .iter()
                .map(|initialization| {
                    (
                        initialization.field_index,
                        aggregate_field_type(builder, target, initialization.field_index),
                    )
                })
                .collect::<Vec<_>>();
            MIRConstant::Aggregate {
                ty: target,
                fields: initializations
                    .iter()
                    .zip(field_types)
                    .map(|(initialization, (field, field_type))| {
                        (
                            field,
                            lower_global_constant(builder, &initialization.value, field_type),
                        )
                    })
                    .collect(),
            }
        }
        _ => panic!(
            "unsupported global initializer expression: {:?}",
            expression
        ),
    }
}

fn lower_global_conversion(
    builder: &mut MIRBuilder,
    operand: &THIRExpression,
    conversion: &THIRCoercion,
    target: MIRTypeID,
) -> MIRConstant {
    if matches!(conversion, THIRCoercion::IntToPtr { .. })
        && matches!(operand.kind, THIRExpressionKind::IntLiteral(0))
    {
        return MIRConstant::Null { ty: target };
    }
    if let Some(value) = string_literal(operand) {
        return lower_string_literal(builder, value, target);
    }

    let source_type = lower_type(builder, &operand._type);
    let source = lower_global_constant(builder, operand, source_type);
    match conversion {
        THIRCoercion::GetFnPtr
        | THIRCoercion::Typechange
        | THIRCoercion::ReinterpretBits
        | THIRCoercion::Integral { .. }
        | THIRCoercion::FloatCast { .. } => retarget_constant(source, target),
        THIRCoercion::Unreachable => {
            panic!("unreachable coercions cannot initialize globals")
        }
        _ => panic!("unsupported global initializer conversion: {conversion:?}"),
    }
}

fn string_literal(expression: &THIRExpression) -> Option<&str> {
    match &expression.kind {
        THIRExpressionKind::StringLiteral { value } => Some(value),
        THIRExpressionKind::Typechange(source)
        | THIRExpressionKind::Copy { source }
        | THIRExpressionKind::TypeConversion {
            operand: source, ..
        } => string_literal(source),
        _ => None,
    }
}

fn lower_global_offset(
    builder: &mut MIRBuilder,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRPtrDiffBinOp,
    ptr_inner: &THIRType,
    target: MIRTypeID,
) -> MIRConstant {
    let lhs_type = lower_type(builder, &lhs._type);
    let source = lower_global_constant(builder, lhs, lhs_type);
    let rhs_type = lower_type(builder, &rhs._type);
    let index = lower_global_constant(builder, rhs, rhs_type);
    let MIRConstant::Integer { value, .. } = index else {
        panic!("global pointer offset has a non-integer index");
    };
    let pointee = lower_type(builder, ptr_inner);
    let layout = builder
        .types()
        .layout(pointee)
        .expect("global pointer offset has an invalid pointee type")
        .expect("global pointer offset has no pointee layout");
    let offset = i64::try_from(value)
        .and_then(|value| i64::try_from(layout.size).map(|size| value.saturating_mul(size)))
        .expect("global pointer offset overflows i64");
    let offset = match op {
        THIRPtrDiffBinOp::ADD => offset,
        THIRPtrDiffBinOp::SUB => -offset,
    };

    match source {
        MIRConstant::Global { global, .. } => MIRConstant::GlobalOffset {
            global,
            offset,
            ty: target,
        },
        MIRConstant::GlobalOffset {
            global,
            offset: base,
            ..
        } => MIRConstant::GlobalOffset {
            global,
            offset: base
                .checked_add(offset)
                .expect("global pointer offset overflows i64"),
            ty: target,
        },
        _ => panic!("global pointer offset has a non-global base"),
    }
}

fn lower_string_literal(builder: &mut MIRBuilder, value: &str, target: MIRTypeID) -> MIRConstant {
    match builder
        .types()
        .kind(target)
        .expect("invalid string initializer target")
    {
        MIRTypeKind::Array { inner, length } => {
            let (ty, signed) =
                integer_target(builder, *inner).unwrap_or((lower_int_type(THIRIntType::I8), false));
            MIRConstant::Aggregate {
                ty: target,
                fields: value
                    .bytes()
                    .chain(std::iter::once(0))
                    .take(*length)
                    .enumerate()
                    .map(|(index, byte)| {
                        (
                            index,
                            MIRConstant::Integer {
                                value: i128::from(byte),
                                ty,
                                signed,
                            },
                        )
                    })
                    .collect(),
            }
        }
        MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. } => {
            MIRConstant::Global {
                global: builder.add_string_literal(value),
                ty: target,
            }
        }
        _ => panic!("string literal has a non-pointer, non-array initializer target"),
    }
}

fn retarget_constant(constant: MIRConstant, target: MIRTypeID) -> MIRConstant {
    match constant {
        MIRConstant::Integer { value, ty, signed } => MIRConstant::Integer { value, ty, signed },
        MIRConstant::Float { value, ty } => MIRConstant::Float { value, ty },
        MIRConstant::Null { .. } => MIRConstant::Null { ty: target },
        MIRConstant::Global { global, .. } => MIRConstant::Global { global, ty: target },
        MIRConstant::GlobalOffset { global, offset, .. } => MIRConstant::GlobalOffset {
            global,
            offset,
            ty: target,
        },
        constant => constant,
    }
}

fn integer_target(
    builder: &MIRBuilder<'_>,
    target: MIRTypeID,
) -> Option<(cx_mir::MIRIntType, bool)> {
    let MIRTypeKind::Integer { ty, signed } = builder.types().kind(target).ok()? else {
        return None;
    };
    Some((*ty, *signed))
}

fn array_element_type(builder: &MIRBuilder<'_>, target: MIRTypeID) -> MIRTypeID {
    let MIRTypeKind::Array { inner, .. } = builder
        .types()
        .kind(target)
        .expect("array initializer has an invalid target type")
    else {
        panic!("array initializer has a non-array target type");
    };
    *inner
}

fn aggregate_field_type(builder: &MIRBuilder<'_>, target: MIRTypeID, field: usize) -> MIRTypeID {
    let fields = match builder
        .types()
        .kind(target)
        .expect("aggregate initializer has an invalid target type")
    {
        MIRTypeKind::Structured { fields } | MIRTypeKind::Union { variants: fields } => fields,
        _ => panic!("aggregate initializer has a non-aggregate target type"),
    };
    fields
        .get(field)
        .expect("aggregate initializer has an invalid field index")
        .ty()
}
