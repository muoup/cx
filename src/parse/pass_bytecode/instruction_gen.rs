use crate::log_error;
use crate::parse::pass_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, get_type_size};
use crate::parse::pass_molded::CXExpr;
use crate::parse::value_type::ValueType;

pub(crate) fn generate_instruction(
    builder: &mut BytecodeBuilder,
    expr: &CXExpr
) -> Option<ValueID> {
    match expr {
        CXExpr::Assignment { lhs, rhs, op: _ } => {
            let lhs = generate_instruction(builder, lhs.as_ref())?;
            let rhs = generate_instruction(builder, rhs.as_ref())?;
            let assn_type = builder.get_type(lhs)?.clone();

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory: lhs,
                    value: rhs,
                    type_: assn_type
                },
                ValueType::Unit
            )
        },
        CXExpr::VarDeclaration { name, type_, initializer } => {
            let Some(type_size) = get_type_size(&builder.type_map, type_) else {
                log_error!("Invalid type for variable declaration: {type_}");
            };

            let memory = builder.add_instruction(
                VirtualInstruction::Allocate {
                    size: type_size
                },
                type_.clone()
            )?;

            builder.symbol_table.insert(name.clone(), memory);

            if let Some(initializer) = initializer {
                let value = generate_instruction(builder, initializer)?;
                builder.add_instruction(
                    VirtualInstruction::Store {
                        memory,
                        value,
                        type_: type_.clone()
                    },
                    ValueType::Unit
                )?;
            }

            Some(memory)
        },
        CXExpr::BinOp { lhs, rhs, op } => {
            let lhs = generate_instruction(builder, lhs.as_ref())?;
            let rhs = generate_instruction(builder, rhs.as_ref())?;
            let lhs_type = builder.get_type(lhs)?.clone();

            match get_intrinsic_type(&builder.type_map, &lhs_type)? {
                ValueType::Integer { .. } |
                ValueType::PointerTo { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: lhs,
                            right: rhs,
                            op: op.clone()
                        },
                        lhs_type
                    )
                },
                ValueType::Float { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::FloatBinOp {
                            left: lhs,
                            right: rhs,
                            op: op.clone(),
                        },
                        lhs_type
                    )
                },

                _ => panic!("Invalid arguments for binop not caught by type checker: {lhs:?} {rhs:?} {op:?}")
            }
        },
        CXExpr::Block { exprs } => {
            for expr in exprs {
                generate_instruction(builder, expr)?;
            }

            Some(ValueID::NULL)
        },
        CXExpr::ImplicitCast { expr, to_type} => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            match expr.as_ref() {
                CXExpr::VarReference(_) => {
                    builder.add_instruction(
                        VirtualInstruction::Load {
                            value: inner.clone(),
                        },
                        to_type.clone()
                    )
                },

                _ => Some(inner)
            }
        },

        CXExpr::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: *val as i32
                },
                ValueType::Integer {
                    bytes: *bytes,
                    signed: true
                }
            )
        },

        CXExpr::VarReference(val) => {
            if let Some(id) = builder.symbol_table.get(val) {
                Some(id.clone())
            } else {
                log_error!("Variable not found in symbol table: {val}")
            }
        },

        CXExpr::Return { value } => {
            let value = match value {
                Some(value) => generate_instruction(builder, value.as_ref()),
                None => None
            };

            builder.add_instruction(
                VirtualInstruction::Return { value },
                ValueType::Unit
            )
        },

        _ => todo!("generate_instruction for {:?}", expr)
    }
}