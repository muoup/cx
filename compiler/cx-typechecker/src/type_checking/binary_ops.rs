use crate::environment::TCEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::casting::{coerce_value, implicit_cast};
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_parsing_data::data::{FunctionTypeIdent, NaiveFnKind};
use cx_typechecker_data::ast::TCBaseMappings;
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_typechecker_data::mir::expression::{MIRBinOp, MIRCoercion, MIRInstruction, MIRValue};
use cx_typechecker_data::mir::types::{CXFloatType, CXIntegerType, CXType, CXTypeKind};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn typecheck_access(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let mut lhs_val = typecheck_expr(env, base_data, lhs)?;
    let lhs_type = lhs_val.get_type();

    let lhs_inner = match lhs_type.mem_ref_inner() {
        Some(inner) if inner.is_pointer() => {
            lhs_val = coerce_value(env, lhs, lhs_val)?;
            lhs_val.get_type()
        }
        Some(inner) => inner.clone(),

        _ => lhs_type.clone(),
    };

    let lhs_inner = lhs_inner
        .ptr_inner()
        .map(CXType::clone)
        .unwrap_or(lhs_inner);

    let fields = match &lhs_inner.kind {
        CXTypeKind::Structured { fields, .. }
        | CXTypeKind::Union {
            variants: fields, ..
        } => fields,

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                "Member access on {}, expected struct or union type",
                lhs_inner
            );
        }
    };

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(i) = fields
                .iter()
                .position(|(field_name, _)| field_name.as_str() == name.as_str())
            {
                let (_, field_type) = &fields[i];
                let field_type = field_type.clone().mem_ref_to();

                let result = env.builder.new_register();

                match &lhs_inner.kind {
                    CXTypeKind::Structured { .. } => {
                        env.builder.add_instruction(MIRInstruction::StructGet {
                            result: result.clone(),
                            source: lhs_val,
                            field_index: i,
                            struct_type: lhs_inner.clone(),
                        })
                    }

                    CXTypeKind::Union { .. } => {
                        env.builder.add_instruction(MIRInstruction::Alias {
                            result: result.clone(),
                            value: lhs_val,
                        })
                    }

                    _ => unreachable!(),
                }

                return Ok(MIRValue::Register {
                    register: result,
                    _type: field_type.mem_ref_to(),
                });
            }

            let Some(type_name) = lhs_inner.get_name() else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a type name",
                    lhs_inner
                );
            };

            let fn_ident = CXFunctionKind::Member {
                base_type: CXIdent::from(type_name),
                name: name.clone(),
            };

            let Some(prototype) = env.get_realized_func(&fn_ident.into()) else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member access on {} with invalid member name {name}",
                    lhs_inner
                );
            };

            Ok(MIRValue::FunctionReference {
                prototype,
                implicit_variables: vec![lhs_val],
            })
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let Some(type_name) = lhs_inner.get_identifier() else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a base name",
                    lhs_inner
                );
            };

            let ident = NaiveFnKind::MemberFunction {
                function_name: CXIdent::from(name.as_str()),
                _type: FunctionTypeIdent::Standard(type_name.clone()),
            };
            let input = complete_template_args(env, base_data, template_input)?;
            let prototype = env.get_func_templated(base_data, &ident, &input)?;

            Ok(MIRValue::FunctionReference {
                prototype,
                implicit_variables: vec![lhs_val],
            })
        }

        _ => log_typecheck_error!(
            env,
            expr,
            " Invalid rhs for access expression, found {:?}",
            rhs
        ),
    }
}

pub(crate) fn comma_separated<'a>(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    expr: &'a CXExpr,
) -> CXResult<Vec<(&'a CXExpr, MIRValue)>> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    if matches!(expr.kind, CXExprKind::Unit) {
        return Ok(exprs);
    }

    while let CXExprKind::BinOp {
        lhs,
        rhs,
        op: CXBinOp::Comma,
    } = &expr_iter.kind
    {
        exprs.push((rhs, typecheck_expr(env, base_data, rhs)?));
        expr_iter = lhs;
    }

    exprs.push((expr_iter, typecheck_expr(env, base_data, expr_iter)?));
    exprs.reverse();

    Ok(exprs)
}

pub(crate) fn typecheck_method_call(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let lhs_val = typecheck_expr(env, base_data, lhs)?;

    let loaded_lhs = coerce_value(env, lhs, lhs_val.clone())?;
    let loaded_lhs_type = loaded_lhs.get_type();

    let loaded_lhs_type = match loaded_lhs_type.kind {
        CXTypeKind::PointerTo { inner_type, .. } => *inner_type,

        _ => loaded_lhs_type,
    };

    let CXTypeKind::Function { prototype } = &loaded_lhs_type.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Attempted to call non-function type {}",
            loaded_lhs_type
        );
    };

    let tc_args = comma_separated(env, base_data, rhs)?;

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        log_typecheck_error!(
            env,
            expr,
            " Method {} expects {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < prototype.params.len() {
        log_typecheck_error!(
            env,
            expr,
            " Method {} expects at least {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    let canon_params = prototype.params.len();

    // Standard argument coercion
    for ((expr, val), param) in tc_args.iter_mut().zip(prototype.params.iter()) {
        *val = implicit_cast(env, *expr, std::mem::take(val), &param._type)?;
    }

    // Varargs argument coercion
    for (expr, val) in tc_args.iter_mut().skip(canon_params) {
        // All varargs arguments must be lvalues, coerce_value is necessary here
        *val = coerce_value(env, *expr, std::mem::take(&mut val))?;
        let arg_type = val.get_type();

        match &arg_type.kind {
            CXTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            }

            CXTypeKind::Integer { signed, .. } => {
                *val = implicit_cast(
                    env,
                    *expr,
                    std::mem::take(val),
                    &CXTypeKind::Integer {
                        _type: CXIntegerType::I64,
                        signed: *signed,
                    }
                    .into(),
                )?;
            }

            CXTypeKind::Float {
                _type: CXFloatType::F32,
            } => {
                *val = implicit_cast(
                    env,
                    *expr,
                    std::mem::take(val),
                    &CXTypeKind::Float {
                        _type: CXFloatType::F32,
                    }
                    .into(),
                )?;
            }

            CXTypeKind::Float {
                _type: CXFloatType::F64,
            } => {
                // Already the correct type for varargs
            }

            CXTypeKind::Bool => {
                *val = implicit_cast(
                    env,
                    *expr,
                    std::mem::take(val),
                    &CXTypeKind::Integer {
                        _type: CXIntegerType::I64,
                        signed: false,
                    }
                    .into(),
                )?;
            }

            _ => {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Cannot coerce value {} for varargs, expected intrinsic type or pointer!",
                    arg_type
                );
            }
        }
    }

    let result = if prototype.return_type.is_unit() {
        None
    } else {
        Some(env.builder.new_register())
    };

    env.builder.add_instruction(MIRInstruction::CallFunction {
        result: result,
        function: loaded_lhs,
        arguments: tc_args.into_iter().map(|(_, val)| val).collect(),
    });

    match result {
        Some(reg) => Ok(MIRValue::Register {
            register: reg,
            _type: prototype.return_type,
        }),
        None => Ok(MIRValue::NULL),
    }
}

pub(crate) fn typecheck_is<'a>(
    env: &'a mut TCEnvironment<'a>,
    base_data: &'a TCBaseMappings,
    lhs: &'a CXExpr,
    rhs: &'a CXExpr,
    expr: &'a CXExpr,
) -> CXResult<MIRValue> {
    let tc_lhs = typecheck_expr(env, base_data, lhs)?;
    let loaded_lhs_val = coerce_value(env, lhs, tc_lhs)?;
    let loaded_lhs_type = loaded_lhs_val.get_type();

    let CXTypeKind::TaggedUnion {
        name: expected_union_name,
        variants,
        ..
    } = &loaded_lhs_type.kind
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a tagged union on the left-hand side, found {}",
            loaded_lhs_type
        );
    };

    let CXExprKind::TypeConstructor {
        union_name,
        variant_name,
        inner,
    } = &rhs.kind
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a type constructor on the right-hand side, found {:?}",
            rhs
        );
    };

    if expected_union_name != union_name {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name.as_string()
        );
    }

    let CXExprKind::Identifier(inner_var_name) = &inner.kind else {
        return log_typecheck_error!(
            env,
            inner,
            " 'is' operator requires a variant name identifier in the type constructor, found {:?}",
            inner
        );
    };

    if loaded_lhs_type.get_name() != Some(union_name.as_str()) {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side type {} does not match right-hand side tagged union type {}",
            loaded_lhs_type,
            union_name.as_string()
        );
    }

    let Some((tag_value, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == variant_name.as_str())
        .map(|(i, (_, _ty))| (i, _ty))
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator variant name '{}' not found in tagged union {}",
            variant_name,
            union_name
        );
    };

    let comparison = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::TaggedUnionIs {
        result: comparison.clone(),
        source: loaded_lhs_val.clone(),
        tag_id: tag_value,
    });

    let result = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::TaggedUnionGet {
        result: result.clone(),
        source: loaded_lhs_val,
        variant_type: variant_type.clone(),
    });

    env.insert_symbol(
        inner_var_name.as_string(),
        MIRValue::Register {
            register: result,
            _type: variant_type.clone(),
        },
    );

    Ok(MIRValue::Register {
        register: comparison,
        _type: CXTypeKind::Bool.into(),
    })
}

pub(crate) fn typecheck_binop(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    op: CXBinOp,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let lhs = typecheck_expr(env, base_data, lhs).and_then(|e| coerce_value(env, lhs, e))?;
    let lhs_type = lhs.get_type();

    let rhs = typecheck_expr(env, base_data, rhs).and_then(|e| coerce_value(env, rhs, e))?;
    let rhs_type = rhs.get_type();

    match (&lhs_type.kind, &rhs_type.kind) {
        (
            CXTypeKind::PointerTo {
                inner_type: l_inner,
                ..
            },
            CXTypeKind::Integer { .. },
        ) => typecheck_ptr_int_binop(env, op.clone(), l_inner.as_ref(), lhs, rhs),

        (
            CXTypeKind::Integer { .. },
            CXTypeKind::PointerTo {
                inner_type: r_inner,
                ..
            },
        ) => typecheck_int_ptr_binop(env, op.clone(), r_inner.as_ref(), lhs, rhs),

        (CXTypeKind::Integer { .. }, CXTypeKind::Integer { .. }) => {
            typecheck_int_int_binop(env, op, lhs, rhs, expr)
        }

        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => {
            let coerced_bool = env.builder.new_register();
            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_bool.clone(),
                operand: lhs.clone(),
                cast_type: MIRCoercion::Integral {
                    sextend: false,
                    to_type: CXIntegerType::I64,
                },
            });

            let lhs = MIRValue::Register {
                register: coerced_bool,
                _type: rhs_type.clone(),
            };

            typecheck_int_int_binop(env, op, lhs, rhs, expr)
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => {
            let coerced_bool = env.builder.new_register();
            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_bool.clone(),
                operand: rhs.clone(),
                cast_type: MIRCoercion::Integral {
                    sextend: false,
                    to_type: CXIntegerType::I64,
                },
            });

            let rhs = MIRValue::Register {
                register: coerced_bool,
                _type: lhs_type.clone(),
            };

            typecheck_int_int_binop(env, op, lhs, rhs, expr)
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => {
            typecheck_ptr_ptr_binop(env, op, lhs, rhs, expr)
        }

        (CXTypeKind::StrongPointer { .. }, _) | (_, CXTypeKind::StrongPointer { .. }) => {
            return log_typecheck_error!(
                env,
                expr,
                "R-value strong pointers must be assigned to an l-value before being used \
                in binary operations, found '{op}' with types {} and {}",
                lhs_type,
                rhs_type
            );
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid binary operation {op} for types {} and {}",
                lhs_type,
                rhs_type
            );
        }
    }
}

pub(crate) fn typecheck_int_int_binop(
    env: &mut TCEnvironment,
    op: CXBinOp,
    mut lhs: MIRValue,
    mut rhs: MIRValue,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let mut lhs_type = lhs.get_type();
    let mut rhs_type = rhs.get_type();

    let CXTypeKind::Integer {
        _type: lhs_itype,
        signed: lhs_signed,
    } = &lhs_type.kind
    else {
        unreachable!("Expected integer type for lhs in int-int binop");
    };

    let CXTypeKind::Integer {
        _type: rhs_itype,
        signed: rhs_signed,
    } = &rhs_type.kind
    else {
        unreachable!("Expected integer type for rhs in int-int binop");
    };

    // The else here handles both the case where the types are of equal bytes
    // (i.e. default to lhs type for signedness), and if lhs is bigger, which
    // also defaults to lhs type. It's possible this is not ~exactly~ adherent
    // to the language specs, and is worth revisiting later.
    let result_type = if rhs_itype.bytes() > lhs_itype.bytes() {
        rhs_type.clone()
    } else {
        lhs_type.clone()
    };

    let CXTypeKind::Integer {
        signed: result_signed,
        ..
    } = &result_type.kind
    else {
        unreachable!("Expected integer type for result in int-int binop");
    };

    if lhs_itype.bytes() > rhs_itype.bytes() {
        let coerced_rhs = env.builder.new_register();

        env.builder.add_instruction(MIRInstruction::Coercion {
            result: coerced_rhs.clone(),
            operand: rhs.clone(),
            cast_type: MIRCoercion::Integral {
                sextend: *rhs_signed,
                to_type: *lhs_itype,
            },
        });

        rhs_type = lhs_type.clone();
        rhs = MIRValue::Register {
            register: coerced_rhs,
            _type: lhs_type.clone(),
        };
    } else if rhs_itype.bytes() < lhs_itype.bytes() {
        let coerced_lhs = env.builder.new_register();

        env.builder.add_instruction(MIRInstruction::Coercion {
            result: coerced_lhs.clone(),
            operand: lhs.clone(),
            cast_type: MIRCoercion::Integral {
                sextend: *lhs_signed,
                to_type: *rhs_itype,
            },
        });

        lhs_type = rhs_type.clone();
        lhs = MIRValue::Register {
            register: coerced_lhs,
            _type: rhs_type.clone(),
        };
    }

    let (operator, result_type) = match op {
        CXBinOp::Add => (MIRBinOp::ADD, lhs_type.clone()),
        CXBinOp::Subtract => (MIRBinOp::SUB, lhs_type.clone()),
        CXBinOp::Multiply if !result_signed => (MIRBinOp::MUL, lhs_type.clone()),
        CXBinOp::Multiply if *result_signed => (MIRBinOp::IMUL, lhs_type.clone()),
        CXBinOp::Divide if !result_signed => (MIRBinOp::DIV, lhs_type.clone()),
        CXBinOp::Divide if *result_signed => (MIRBinOp::IDIV, lhs_type.clone()),
        CXBinOp::Modulus if !result_signed => (MIRBinOp::MOD, lhs_type.clone()),
        CXBinOp::Modulus if *result_signed => (MIRBinOp::IMOD, lhs_type.clone()),

        CXBinOp::Less => (MIRBinOp::LT, CXTypeKind::Bool.into()),
        CXBinOp::Greater => (MIRBinOp::GT, CXTypeKind::Bool.into()),
        CXBinOp::LessEqual => (MIRBinOp::LE, CXTypeKind::Bool.into()),
        CXBinOp::GreaterEqual => (MIRBinOp::GE, CXTypeKind::Bool.into()),
        CXBinOp::Equal => (MIRBinOp::EQ, CXTypeKind::Bool.into()),
        CXBinOp::NotEqual => (MIRBinOp::NEQ, CXTypeKind::Bool.into()),

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid integer binary operation {op} for types {} and {}",
                lhs_type,
                rhs_type
            );
        }
    };

    let result = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::BinOp {
        op: operator,
        result: result.clone(),
        lhs,
        rhs,
    });

    Ok(MIRValue::Register {
        register: result,
        _type: result_type,
    })
}

pub(crate) fn typecheck_int_ptr_binop(
    env: &mut TCEnvironment,
    op: CXBinOp,
    pointer_inner: &CXType,
    non_pointer: MIRValue,
    pointer: MIRValue,
) -> CXResult<MIRValue> {
    if op == CXBinOp::Subtract {
        return log_typecheck_error!(
            env,
            &CXExpr::default(),
            " Invalid operation [integer] - [pointer] for types {} and {}",
            non_pointer.get_type(),
            pointer.get_type()
        );
    }

    typecheck_ptr_int_binop(env, op, pointer_inner, pointer, non_pointer)
}

pub(crate) fn typecheck_ptr_int_binop(
    env: &mut TCEnvironment,
    op: CXBinOp,
    pointer_inner: &CXType,
    pointer: MIRValue,
    integer: MIRValue,
) -> CXResult<MIRValue> {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::ArrayIndex => {
            let inner_size = pointer_inner.type_size();
            let scaled_integer = env.builder.new_register();
            let rhs_type = integer.get_type();

            let CXTypeKind::Integer { _type, .. } = &rhs_type.kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            env.builder.add_instruction(MIRInstruction::BinOp {
                op: MIRBinOp::MUL,
                result: scaled_integer.clone(),
                lhs: integer.clone(),
                rhs: MIRValue::IntLiteral {
                    value: inner_size as i64,
                    _type: _type.clone(),
                    signed: false,
                },
            });

            let coerced_pointer = env.builder.new_register();

            env.builder.add_instruction(MIRInstruction::BinOp {
                op: match op {
                    CXBinOp::Add | CXBinOp::ArrayIndex => MIRBinOp::ADD,
                    CXBinOp::Subtract => MIRBinOp::SUB,
                    _ => unreachable!(),
                },
                result: coerced_pointer.clone(),
                lhs: pointer,
                rhs: MIRValue::Register {
                    register: scaled_integer,
                    _type: rhs_type,
                },
            });

            Ok(MIRValue::Register {
                register: coerced_pointer,
                _type: pointer_inner.clone().pointer_to(),
            })
        }

        // Requires two pointers
        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => {
            let coerced_val = env.builder.new_register();
            let CXTypeKind::Integer { signed, _type } = integer.get_type().kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_val.clone(),
                operand: integer.clone(),
                cast_type: MIRCoercion::Integral {
                    sextend: signed,
                    to_type: CXIntegerType::I64,
                },
            });

            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_val.clone(),
                operand: integer.clone(),
                cast_type: MIRCoercion::ReinterpretBits,
            });

            Ok(MIRValue::Register {
                register: coerced_val,
                _type: CXTypeKind::Bool.into(),
            })
        }

        _ => panic!("Invalid binary operation {op} for pointer type"),
    }
}

pub(crate) fn typecheck_ptr_ptr_binop(
    env: &mut TCEnvironment,
    op: CXBinOp,
    lhs: MIRValue,
    rhs: MIRValue,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let operator = match op {
        CXBinOp::LessEqual => MIRBinOp::LE,
        CXBinOp::GreaterEqual => MIRBinOp::GE,
        CXBinOp::Less => MIRBinOp::LT,
        CXBinOp::Greater => MIRBinOp::GT,
        CXBinOp::Equal => MIRBinOp::EQ,
        CXBinOp::NotEqual => MIRBinOp::NEQ,

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid binary operation {op} for pointer types",
            );
        }
    };

    let result = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::BinOp {
        op: operator,
        result: result.clone(),
        lhs,
        rhs,
    });

    Ok(MIRValue::Register {
        register: result,
        _type: CXTypeKind::Bool.into(),
    })
}

pub(crate) fn binop_type(
    env: &TCEnvironment,
    op: &CXBinOp,
    lhs: &MIRValue,
    expr: &CXExpr,
) -> CXResult<CXType> {
    match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus => Ok(lhs.get_type()),

        CXBinOp::ArrayIndex => {
            let lhs_type = &lhs.get_type();

            let CXTypeKind::PointerTo {
                inner_type: pointer_inner,
                ..
            } = &lhs_type.kind
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Array index operation requires a pointer type, found {}",
                    lhs_type
                );
            };

            Ok(CXType::from(CXTypeKind::MemoryReference(
                pointer_inner.clone(),
            )))
        }

        CXBinOp::LAnd
        | CXBinOp::LOr
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => Ok(CXTypeKind::Bool.into()),

        _ => log_typecheck_error!(
            env,
            expr,
            "Invalid binary operation {op} for type {:?}",
            lhs
        ),
    }
}
