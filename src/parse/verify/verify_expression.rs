use crate::parse::ast::{ControlExpression, Expression, LValueExpression, LiteralExpression, RValueExpression, ValueType};
use crate::parse::verify::bytecode::{BytecodeBuilder, ValueID, VirtualInstruction, VirtualValue};
use crate::parse::verify::context::VerifyContext;

pub(crate) fn verify_expression(context: &mut VerifyContext, builder: &mut BytecodeBuilder,
                                expression: &Expression) -> Option<ValueID> {
    match expression {
        Expression::Literal(literal) =>
            verify_literal(context, builder, literal),

        Expression::Control(control) =>
            verify_control(context, builder, control),

        Expression::LValue(lvalue) =>
            verify_lvalue(context, builder, lvalue),

        Expression::RValue(rvalue) =>
            verify_rvalue(context, builder, rvalue),

        Expression::Unit =>
            builder.add_instruction(
                VirtualInstruction::NOP,
                ValueType::Unit
            )
    }
}

pub(crate) fn verify_literal(context: &mut VerifyContext, builder: &mut BytecodeBuilder, literal: &LiteralExpression) -> Option<ValueID> {
    match literal {
        LiteralExpression::IntLiteral { val, bytes } => {
            let as_bytes = unsafe { std::mem::transmute::<i64, [u8; 8]>(*val) };

            builder.add_instruction(
                VirtualInstruction::Literal { bytes: as_bytes },
                ValueType::Integer { bytes: *bytes, signed: true }
            )
        },

        LiteralExpression::FloatLiteral { val, bytes } => {
            let as_bytes = unsafe { std::mem::transmute::<f64, [u8; 8]>(*val) };

            builder.add_instruction(
                VirtualInstruction::Literal { bytes: as_bytes },
                ValueType::Float { bytes: *bytes }
            )
        },

        LiteralExpression::StringLiteral(val) => {
            let str_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral { str_id },
                ValueType::PointerTo(Box::new(ValueType::Integer { bytes: 8, signed: false }))
            )
        }
    }
}

pub(crate) fn verify_rvalue(context: &mut VerifyContext, builder: &mut BytecodeBuilder, rvalue: &RValueExpression) -> Option<ValueID> {
    match rvalue {
        _ => unimplemented!("{:?}", rvalue)
    }
}

pub(crate) fn verify_lvalue(context: &mut VerifyContext, builder: &mut BytecodeBuilder, lvalue: &LValueExpression) -> Option<ValueID> {
    match lvalue {
        _ => unimplemented!("{:?}", lvalue)
    }
}

pub(crate) fn verify_control(context: &mut VerifyContext, builder: &mut BytecodeBuilder, control: &ControlExpression) -> Option<ValueID> {
    match control {
        _ => unimplemented!("{:?}", control)
    }
}