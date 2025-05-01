use crate::log_error;
use crate::parse::pass_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use crate::parse::pass_bytecode::typing::{get_intrinsic_type, get_type_size, implicit_casting};
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXUnOp};
use crate::parse::value_type::{is_structure, CXValType};

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
                CXValType::Unit
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
                    CXValType::Unit
                )?;
            }

            Some(memory)
        },

        CXExpr::BinOp { lhs, rhs, op } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let lhs_type = builder.get_type(left_id)?.clone();

            match get_intrinsic_type(&builder.type_map, &lhs_type)? {
                CXValType::Integer { .. } |
                CXValType::PointerTo { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: left_id,
                            right: right_id,
                            op: op.clone()
                        },
                        lhs_type
                    )
                },
                CXValType::Float { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::FloatBinOp {
                            left: left_id,
                            right: right_id,
                            op: op.clone(),
                        },
                        lhs_type
                    )
                },

                _type =>
                    panic!("Invalid arguments with type for binop not caught by type checker: {_type} \'{lhs}\' \'{rhs}\' {op:?} ")
            }
        },
        CXExpr::Block { exprs, value } => {
            for expr in exprs {
                generate_instruction(builder, expr)?;
            }

            if let Some(value) = value {
                return generate_instruction(builder, value.as_ref());
            }

            Some(ValueID::NULL)
        },
        CXExpr::ImplicitCast { expr, from_type, to_type} => {
            let mut inner = generate_instruction(builder, expr.as_ref())?;

            match expr.as_ref() {
                CXExpr::VarReference(_) |
                CXExpr::StructAccess { .. }
                    if !is_structure(&builder.type_map, to_type) => {

                    inner = builder.add_instruction(
                        VirtualInstruction::Load {
                            value: inner.clone(),
                        },
                        to_type.clone()
                    )?;
                },

                _ => ()
            }

            implicit_casting(builder, inner, &from_type, &to_type)
        },

        CXExpr::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: *val as i32
                },
                CXValType::Integer {
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
                CXValType::Unit
            )
        },

        CXExpr::DirectFunctionCall { name, args } => {
            let arg_ids = args.iter()
                .map(|arg| generate_instruction(builder, arg))
                .collect::<Option<Vec<_>>>()?;

            builder.add_instruction(
                VirtualInstruction::DirectCall {
                    function: name.clone(),
                    args: arg_ids
                },
                CXValType::Unit
            )
        },

        CXExpr::StringLiteral { val, .. } => {
            let string_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral {
                    str_id: string_id,
                },
                CXValType::PointerTo(Box::new(CXValType::Identifier("char".to_string())))
            )
        },

        CXExpr::StructAccess { expr, field_type, field_offset, field_index, .. } => {
            let struct_ref = generate_instruction(builder, expr.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::StructAccess {
                    struct_: struct_ref,
                    field_offset: *field_offset,
                    field_index: *field_index,
                },
                field_type.clone()
            )
        },

        CXExpr::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Dereference => {
                    let value = generate_instruction(builder, operand.as_ref())?;

                    builder.add_instruction(
                        VirtualInstruction::Load { value },
                        builder.get_type(value)?.clone()
                    )
                },
                CXUnOp::AddressOf => {
                    let value = generate_instruction(builder, operand.as_ref())?;

                    builder.add_instruction(
                        VirtualInstruction::AddressOf { value },
                        CXValType::PointerTo(Box::new(builder.get_type(value)?.clone()))
                    )
                },

                _ => todo!("generate_instruction for {:?}", operator)
            }
        },

        _ => todo!("generate_instruction for {:?}", expr)
    }
}