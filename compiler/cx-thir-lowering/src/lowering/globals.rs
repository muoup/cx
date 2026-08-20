use cx_mir::{MIRConstant, MIRFloatType, MIRGlobalState};
use cx_thir::thir::{
    expression::{THIRExpression, THIRExpressionKind},
    global::{THIRGlobalVariable},
    r#type::{THIRFloatType, THIRTypeKind},
};
use cx_util::linkage::LinkageMode;

use crate::{MIRBuilder, builder::integer_type, lowering::types::lower_type};

pub(crate) fn lower_global(builder: &mut MIRBuilder, global: &THIRGlobalVariable) {
    let id = builder
        .global_id(global.name.as_str())
        .unwrap_or_else(|| panic!("global {} was not declared", global.name));

    let state = match global.initializer.as_ref() {
        Some(initializer) => {
            let constant = lower_global_constant(builder, &initializer);
            MIRGlobalState::Initialized(constant)
        },
        _ if global.linkage == LinkageMode::Extern => MIRGlobalState::External,
        _ => MIRGlobalState::ZeroInitialized,
    };

    builder.set_global_state(id, state);
}

fn lower_global_constant(builder: &mut MIRBuilder, expression: &THIRExpression) -> MIRConstant {
    match &expression.kind {
        THIRExpressionKind::Typechange(source) | THIRExpressionKind::Copy { source } => {
            lower_global_constant(builder, source)
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
            MIRConstant::Global {
                global,
                ty: lower_type(builder, &expression._type),
            }
        }
        THIRExpressionKind::IntLiteral(value) => {
            let (ty, signed) = integer_type(&expression._type);
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
        THIRExpressionKind::ArrayInitializer { elements, .. } => MIRConstant::Aggregate {
            ty: lower_type(builder, &expression._type),
            fields: elements
                .iter()
                .enumerate()
                .map(|(index, element)| (index, lower_global_constant(builder, element)))
                .collect(),
        },
        THIRExpressionKind::StructInitializer {
            initializations, ..
        } => MIRConstant::Aggregate {
            ty: lower_type(builder, &expression._type),
            fields: initializations
                .iter()
                .map(|initialization| {
                    (
                        initialization.field_index,
                        lower_global_constant(builder, &initialization.value),
                    )
                })
                .collect(),
        },
        _ => panic!(
            "unsupported global initializer expression: {:?}",
            expression
        ),
    }
}
