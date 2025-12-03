use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::casting::{coerce_value, implicit_cast};
use crate::type_checking::contract::contracted_function_call;
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_parsing_data::data::{FunctionTypeIdent, NaiveFnKind};
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_typechecker_data::mir::expression::{
    MIRBinOp, MIRBoolBinOp, MIRCoercion, MIRInstruction, MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp, MIRValue
};
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXFloatType, CXIntegerType, CXType, CXTypeKind};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn typecheck_access(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let mut lhs_val = typecheck_expr(env, base_data, lhs)?;
    let lhs_type = lhs_val.get_type();

    // Here, out aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let lhs_inner = match lhs_type.mem_ref_inner() {
        // If we have a reference to a region containing a pointer, we need to
        // load the pointer and use that as our pointer
        Some(inner) if inner.is_pointer() => {
            lhs_val = coerce_value(env, lhs, lhs_val)?;
            lhs_val.get_type()
        }

        // If we simply have a region reference, that is sufficient as a pointer,
        Some(inner) => inner.clone(),

        // Technically speaking, if we have a owned struct / naked struct type,
        // we can also treat that type as a pointer, as a struct must exist
        // in memory, and its alias is thus a pointer by definition.
        //
        // This may have to change when we introduce structs that fit in registers,
        // but for now, this is acceptable.
        _ => lhs_type.clone(),
    };

    let lhs_inner = lhs_inner.ptr_inner().cloned().unwrap_or(lhs_inner);

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(struct_field) = struct_field(&lhs_inner, name) {
                let result = env.builder.new_register();

                match &lhs_inner.kind {
                    CXTypeKind::Structured { .. } => {
                        env.builder.add_instruction(MIRInstruction::StructGet {
                            result: result.clone(),
                            source: lhs_val,
                            field_index: struct_field.index,
                            field_offset: struct_field.offset,
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
                    _type: struct_field.field_type.mem_ref_to(),
                });
            }

            if let CXTypeKind::Union { variants, .. } = &lhs_inner.kind {
                let Some((_, field_type)) = variants
                    .iter()
                    .find(|(field_name, _)| field_name.as_str() == name.as_str())
                else {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Union type {} has no field named {}",
                        lhs_inner,
                        name
                    );
                };

                let result = env.builder.new_register();

                env.builder.add_instruction(MIRInstruction::Alias {
                    result: result.clone(),
                    value: lhs_val,
                });

                return Ok(MIRValue::Register {
                    register: result,
                    _type: field_type.clone().mem_ref_to(),
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
                base_type: CXIdent::new(type_name),
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
                function_name: CXIdent::new(name.as_str()),
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
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
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
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
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

    let mut tc_args = comma_separated(env, base_data, rhs)?;

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        return log_typecheck_error!(
            env,
            expr,
            " Method {} expects {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < prototype.params.len() {
        return log_typecheck_error!(
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
        *val = coerce_value(env, *expr, std::mem::take(val))?;
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

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();
    contracted_function_call(env, base_data, prototype, lhs_val, &args)
}

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
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
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
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

        (CXTypeKind::Bool, CXTypeKind::Bool) => typecheck_bool_bool_binop(env, op, lhs, rhs, expr),

        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => {
            let coerced_bool = env.builder.new_register();

            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_bool.clone(),
                operand: rhs.clone(),
                cast_type: MIRCoercion::IntToBool,
            });

            let rhs = MIRValue::Register {
                register: coerced_bool,
                _type: CXTypeKind::Bool.into(),
            };

            typecheck_bool_bool_binop(env, op, lhs, rhs, expr)
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => {
            let coerced_bool = env.builder.new_register();

            env.builder.add_instruction(MIRInstruction::Coercion {
                result: coerced_bool.clone(),
                operand: lhs.clone(),
                cast_type: MIRCoercion::IntToBool,
            });

            let lhs = MIRValue::Register {
                register: coerced_bool,
                _type: lhs_type.clone(),
            };

            typecheck_bool_bool_binop(env, op, lhs, rhs, expr)
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

pub(crate) fn typecheck_bool_bool_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRValue,
    rhs: MIRValue,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let operator = match op {
        CXBinOp::LAnd => MIRBoolBinOp::LAND,
        CXBinOp::LOr => MIRBoolBinOp::LOR,
        
        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid boolean binary operation {op}",
            );
        }
    };

    let result = env.builder.new_register();

    env.builder.add_instruction(MIRInstruction::BinOp {
        op: MIRBinOp::Bool { op: operator },
        result: result.clone(),
        lhs,
        rhs,
    });

    Ok(MIRValue::Register {
        register: result,
        _type: CXTypeKind::Bool.into()
    })
}

pub(crate) fn typecheck_int_int_binop(
    env: &mut TypeEnvironment,
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

    let itype = if lhs_itype.bytes() > rhs_itype.bytes() {
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

        *lhs_itype
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

        *rhs_itype
    } else {
        *lhs_itype
    };

    let operator = MIRBinOp::Integer {
        itype,
        op: match op {
            CXBinOp::Add => MIRIntegerBinOp::ADD,
            CXBinOp::Subtract => MIRIntegerBinOp::SUB,
            CXBinOp::Multiply => MIRIntegerBinOp::MUL,
            CXBinOp::Divide => MIRIntegerBinOp::DIV,
            CXBinOp::Modulus => MIRIntegerBinOp::MOD,
            CXBinOp::Less if !*result_signed => MIRIntegerBinOp::LT,
            CXBinOp::Less if *result_signed => MIRIntegerBinOp::ILT,
            CXBinOp::Greater if !*result_signed => MIRIntegerBinOp::GT,
            CXBinOp::Greater if *result_signed => MIRIntegerBinOp::IGT,
            CXBinOp::LessEqual if !*result_signed => MIRIntegerBinOp::LE,
            CXBinOp::LessEqual if *result_signed => MIRIntegerBinOp::ILE,
            CXBinOp::GreaterEqual if !*result_signed => MIRIntegerBinOp::GE,
            CXBinOp::GreaterEqual if *result_signed => MIRIntegerBinOp::IGE,
            CXBinOp::Equal => MIRIntegerBinOp::EQ,
            CXBinOp::NotEqual => MIRIntegerBinOp::NE,
            
            _ => {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Invalid integer binary operation {op} for types {} and {}",
                    lhs_type,
                    rhs_type
                );
            }
        },
    };

    let result_type = match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus => result_type.clone(),

        CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => CXTypeKind::Bool.into(),

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
    env: &mut TypeEnvironment,
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
    env: &mut TypeEnvironment,
    op: CXBinOp,
    pointer_inner: &CXType,
    pointer: MIRValue,
    integer: MIRValue,
) -> CXResult<MIRValue> {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::ArrayIndex => {
            let rhs_type = integer.get_type();
            let int_sized_pointer = CXTypeKind::Integer {
                _type: CXIntegerType::I64,
                signed: true,
            }
            .into();

            let coerced_integer =
                implicit_cast(env, &CXExpr::default(), integer, &int_sized_pointer)?;

            let CXTypeKind::Integer { _type, .. } = &rhs_type.kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            let result = env.builder.new_register();
            let (operation, return_type) = match op {
                CXBinOp::Add => (
                    MIRBinOp::PtrDiff {
                        op: MIRPtrDiffBinOp::ADD,
                        ptr_inner: Box::new(pointer_inner.clone()),
                    },
                    pointer_inner.clone().pointer_to(),
                ),

                CXBinOp::ArrayIndex => (
                    MIRBinOp::PtrDiff {
                        op: MIRPtrDiffBinOp::ADD,
                        ptr_inner: Box::new(pointer_inner.clone()),
                    },
                    pointer_inner.clone().mem_ref_to(),
                ),

                CXBinOp::Subtract => (
                    MIRBinOp::PtrDiff {
                        op: MIRPtrDiffBinOp::SUB,
                        ptr_inner: Box::new(pointer_inner.clone()),
                    },
                    pointer_inner.clone().pointer_to(),
                ),
                
                _ => unreachable!(),
            };

            env.builder.add_instruction(MIRInstruction::BinOp {
                op: operation,
                result: result.clone(),
                lhs: pointer,
                rhs: coerced_integer,
            });

            Ok(MIRValue::Register {
                register: result,
                _type: return_type,
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
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRValue,
    rhs: MIRValue,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let operator = match op {
        CXBinOp::LessEqual => MIRPtrBinOp::LE,
        CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
        CXBinOp::Less => MIRPtrBinOp::LT,
        CXBinOp::Greater => MIRPtrBinOp::GT,
        CXBinOp::Equal => MIRPtrBinOp::EQ,
        CXBinOp::NotEqual => MIRPtrBinOp::NE,

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
        op: MIRBinOp::Pointer { op: operator },
        result: result.clone(),
        lhs,
        rhs,
    });

    Ok(MIRValue::Register {
        register: result,
        _type: CXTypeKind::Bool.into(),
    })
}

struct StructField {
    index: usize,
    offset: usize,
    field_type: CXType,
}

fn struct_field<'a>(struct_type: &CXType, field_name: &CXIdent) -> Option<StructField> {
    let mut field_index = 0;
    let mut field_offset = 0;

    let CXTypeKind::Structured { fields, .. } = &struct_type.kind else {
        unreachable!()
    };

    for (field_name_i, field_type) in fields.iter() {
        if field_offset % field_type.type_alignment() != 0 {
            field_offset +=
                field_type.type_alignment() - (field_offset % field_type.type_alignment());
        }

        if field_name_i.as_str() == field_name.as_str() {
            return Some(StructField {
                index: field_index,
                offset: field_offset,
                field_type: field_type.clone(),
            });
        }

        field_offset += field_type.type_size();
        field_index += 1;
    }

    None
}
