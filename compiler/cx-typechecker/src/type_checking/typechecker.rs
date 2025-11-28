use crate::environment::TCEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::binary_ops::{
    typecheck_access, typecheck_binop, typecheck_is, typecheck_method_call,
};
use crate::type_checking::casting::{coerce_condition, coerce_value, explicit_cast, implicit_cast};
use crate::type_checking::move_semantics::acknowledge_declared_type;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalVariable, CXUnOp};
use cx_parsing_data::data::{CX_CONST, CXLinkageMode, NaiveFnIdent, NaiveFnKind};
use cx_typechecker_data::ast::{TCBaseMappings, TCGlobalVarKind, TCGlobalVariable};
use cx_typechecker_data::mir::expression::{MIRBinOp, MIRInstruction, MIRUnOp, MIRValue};
use cx_typechecker_data::mir::types::{CXIntegerType, CXTypeKind};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

fn anonymous_name_gen() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__anon_{id}")
}

pub fn typecheck_expr(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    Ok(match &expr.kind {
        CXExprKind::Block { exprs } => {
            exprs
                .iter()
                .map(|e| typecheck_expr(env, base_data, e))
                .collect::<CXResult<Vec<_>>>()?;

            MIRValue::NULL
        }

        CXExprKind::IntLiteral { val, bytes } => MIRValue::IntLiteral {
            value: *val,
            _type: CXIntegerType::from_bytes(*bytes).unwrap(),
            signed: true,
        },

        CXExprKind::FloatLiteral { val, bytes } => MIRValue::FloatLiteral {
            value: *val,
            _type: cx_typechecker_data::mir::types::CXFloatType::from_bytes(*bytes).unwrap(),
        },

        CXExprKind::StringLiteral { val } => {
            let anonymous_name = anonymous_name_gen();
            let name_ident = CXIdent::from(anonymous_name.clone());

            env.realized_globals.insert(
                anonymous_name.clone(),
                TCGlobalVariable {
                    kind: TCGlobalVarKind::StringLiteral {
                        name: name_ident.clone(),
                        value: val.clone(),
                    },
                    is_mutable: false,
                    linkage: CXLinkageMode::Static,
                },
            );

            MIRValue::GlobalValue {
                name: CXIdent::from(anonymous_name),
                _type: env
                    .get_realized_type("char")
                    .unwrap()
                    .clone()
                    .pointer_to()
                    .add_specifier(CX_CONST),
            }
        }

        CXExprKind::VarDeclaration { type_, name } => {
            let type_ = env.complete_type(base_data, type_)?;
            let result = env.builder.new_register();

            env.builder
                .add_instruction(MIRInstruction::CreateStackRegion {
                    result: result.clone(),
                    _type: type_.clone(),
                });

            env.insert_symbol(
                name.as_string(),
                MIRValue::Register {
                    register: result.clone(),
                    _type: type_.clone(),
                },
            );
            acknowledge_declared_type(env, base_data, &type_);

            MIRValue::Register {
                register: result,
                _type: type_.mem_ref_to(),
            }
        }

        CXExprKind::Identifier(name) => {
            if let Some(symbol_val) = env.symbol_value(name.as_str()) {
                symbol_val.clone()
            } else if let Some(function_type) = env
                .get_func(base_data, &NaiveFnIdent::Standard(name.clone()))
                .ok()
            {
                MIRValue::FunctionReference {
                    prototype: function_type.clone(),
                    implicit_variables: vec![],
                }
            } else if let Some(global) = global_expr(env, base_data, name.as_str()).ok() {
                global
            } else {
                return log_typecheck_error!(env, expr, "Identifier '{}' not found", name);
            }
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            // These [for now], are only for functions, as templated type identifiers can only appear
            // in CXNaiveType contexts.

            let input = complete_template_args(env, base_data, template_input)?;
            let ident = NaiveFnKind::Standard(name.clone());

            let function = env.get_func_templated(base_data, &ident, &input)?;

            MIRValue::FunctionReference {
                prototype: function.clone(),
                implicit_variables: vec![],
            }
        }

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            // Scope for condition
            env.push_scope(None, None);

            let then_block = env.builder.new_block_id();
            let (else_block, merge_block) = if else_branch.is_some() {
                (env.builder.new_block_id(), env.builder.new_block_id())
            } else {
                let merge_block = env.builder.new_block_id();
                (merge_block.clone(), merge_block.clone())
            };

            let condition_value = typecheck_expr(env, base_data, condition)
                .and_then(|c| coerce_condition(env, expr, c))?;

            env.builder.add_instruction(MIRInstruction::Branch {
                condition: condition_value,
                true_block: then_block.clone(),
                false_block: else_block.clone(),
            });

            env.builder.add_block(then_block);
            env.push_scope(Some(merge_block.clone()), None);
            typecheck_expr(env, base_data, then_branch)?;
            env.pop_scope();

            env.builder.add_jump(merge_block.clone());

            if let Some(else_branch) = else_branch {
                env.builder.set_block(else_block);
                env.push_scope(Some(merge_block.clone()), None);
                typecheck_expr(env, base_data, else_branch)?;
                env.pop_scope();
                env.builder.add_jump(merge_block.clone());
            }

            env.builder.set_block(merge_block);
            env.pop_scope();

            MIRValue::NULL
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            let condition_block = env.builder.new_block_id();
            let body_block = env.builder.new_block_id();
            let merge_block = env.builder.new_block_id();

            env.builder.add_jump(condition_block.clone());
            env.push_scope(None, None);

            env.builder.add_and_set_block(condition_block.clone());
            let condition_tc = typecheck_expr(env, base_data, condition)
                .and_then(|c| coerce_condition(env, expr, c))?;
            env.builder.add_instruction(MIRInstruction::Loop {
                condition: condition_tc,
                condition_precheck: *pre_eval,
                body: body_block.clone(),
                merge: merge_block.clone(),
            });

            env.builder.add_and_set_block(body_block);
            env.push_scope(Some(merge_block.clone()), Some(condition_block.clone()));
            typecheck_expr(env, base_data, body)?;
            env.pop_scope();
            env.builder.add_instruction(MIRInstruction::Jump {
                target: condition_block.clone(),
            });

            env.pop_scope();
            env.builder.add_and_set_block(merge_block);

            MIRValue::NULL
        }

        CXExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            let condition_block = env.builder.new_block_id();
            let body_block = env.builder.new_block_id();
            let increment_block = env.builder.new_block_id();
            let merge_block = env.builder.new_block_id();

            env.push_scope(None, None);
            typecheck_expr(env, base_data, init)?;
            env.builder.add_jump(condition_block.clone());

            env.builder.add_and_set_block(condition_block.clone());
            let condition = typecheck_expr(env, base_data, condition)
                .and_then(|c| coerce_condition(env, expr, c))?;
            env.builder.add_instruction(MIRInstruction::Loop {
                condition,
                condition_precheck: true,
                body: body_block.clone(),
                merge: merge_block.clone(),
            });

            env.builder.add_and_set_block(body_block);
            env.push_scope(Some(merge_block.clone()), Some(condition_block.clone()));
            typecheck_expr(env, base_data, body)?;
            env.pop_scope();
            env.builder.add_jump(increment_block.clone());

            env.builder.add_and_set_block(increment_block);
            env.push_scope(None, None);

            typecheck_expr(env, base_data, increment)?;

            env.pop_scope();
            env.builder.add_jump(condition_block.clone());

            env.pop_scope();
            env.builder.add_and_set_block(merge_block);

            MIRValue::NULL
        }

        CXExprKind::Break => {
            // Q: What's the easiest way to convert from Option<&Option<T>> to Option<&T>?
            // A: Using and_then twice.

            let Some(break_to) = env
                .scope_stack
                .last()
                .map(|inner| inner.break_to.as_ref())
                .flatten()
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'break' used outside of a loop or switch context"
                );
            };

            // TODO: Handle cleanup of deferred expressions here
            env.builder.add_jump(break_to.clone());
            MIRValue::NULL
        }

        CXExprKind::Continue => {
            let Some(continue_to) = env
                .scope_stack
                .last()
                .map(|inner| inner.continue_to.as_ref())
                .flatten()
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'continue' used outside of a loop context"
                );
            };

            env.builder.add_jump(continue_to.clone());
            MIRValue::NULL
        }

        CXExprKind::Return { value } => {
            let mut value_tc = if let Some(value) = value {
                let val = typecheck_expr(env, base_data, value)
                    .and_then(|v| coerce_value(env, expr, v))?;

                Some(val)
            } else {
                None
            };

            let return_type = &env.current_function().return_type.clone();

            match (&mut value_tc, return_type) {
                (Some(some_value), return_type) if !return_type.is_unit() => {
                    *some_value =
                        implicit_cast(env, expr, std::mem::take(some_value), return_type)?;
                }

                (None, _) if return_type.is_unit() => {}

                (Some(_), _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Cannot return from function {} with a void return type",
                        env.current_function()
                    );
                }

                (None, _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Function {} expects a return value, but none was provided",
                        env.current_function()
                    );
                }
            }

            env.builder
                .add_instruction(MIRInstruction::Return { value: value_tc });

            MIRValue::NULL
        }

        CXExprKind::Defer { expr } => {
            env.in_defer(|e| typecheck_expr(e, base_data, expr));

            MIRValue::NULL
        }

        CXExprKind::UnOp { operator, operand } => {
            let operand_val = typecheck_expr(env, base_data, operand)?;
            let operand_type = operand_val.get_type();

            match operator {
                CXUnOp::PreIncrement(increment_amount)
                | CXUnOp::PostIncrement(increment_amount) => {
                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot apply pre-increment to a non-reference {}",
                            operand_type
                        );
                    };

                    let load = env.builder.new_register();
                    let result = env.builder.new_register();
                    env.builder.add_instruction(MIRInstruction::MemoryRead {
                        result: load.clone(),
                        source: operand_val.clone(),
                        _type: inner.clone(),
                    });

                    match &inner.kind {
                        CXTypeKind::Integer { _type, signed, .. } => {
                            env.builder.add_instruction(MIRInstruction::BinOp {
                                result: result.clone(),
                                op: MIRBinOp::ADD,
                                lhs: MIRValue::Register {
                                    register: load.clone(),
                                    _type: inner.clone(),
                                },
                                rhs: MIRValue::IntLiteral {
                                    value: *increment_amount as i64,
                                    _type: _type.clone(),
                                    signed: signed.clone(),
                                },
                            });

                            env.builder.add_instruction(MIRInstruction::MemoryWrite {
                                target: operand_val,
                                value: MIRValue::Register {
                                    register: result.clone(),
                                    _type: inner.clone(),
                                },
                            });

                            match operator {
                                CXUnOp::PreIncrement(_) => MIRValue::Register {
                                    register: result.clone(),
                                    _type: inner.clone(),
                                },

                                CXUnOp::PostIncrement(_) => MIRValue::Register {
                                    register: load,
                                    _type: inner.clone(),
                                },

                                _ => unreachable!(),
                            }
                        }

                        CXTypeKind::PointerTo { inner_type, .. } => {
                            let element_stride = inner_type.type_size();

                            env.builder.add_instruction(MIRInstruction::BinOp {
                                result: result.clone(),
                                op: MIRBinOp::ADD,
                                lhs: MIRValue::Register {
                                    register: load.clone(),
                                    _type: inner.clone(),
                                },
                                rhs: MIRValue::IntLiteral {
                                    value: (*increment_amount as i64) * (element_stride as i64),
                                    _type: CXIntegerType::from_bytes(8).unwrap(),
                                    signed: true,
                                },
                            });

                            env.builder.add_instruction(MIRInstruction::MemoryWrite {
                                target: operand_val,
                                value: MIRValue::Register {
                                    register: result.clone(),
                                    _type: inner.clone(),
                                },
                            });

                            match operator {
                                CXUnOp::PreIncrement(_) => MIRValue::Register {
                                    register: result.clone(),
                                    _type: inner.clone(),
                                },

                                CXUnOp::PostIncrement(_) => MIRValue::Register {
                                    register: load,
                                    _type: inner.clone(),
                                },

                                _ => unreachable!(),
                            }
                        }

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand,
                                " Pre-increment operator requires an integer or pointer type, found {}",
                                inner
                            );
                        }
                    }
                }

                CXUnOp::LNot => {
                    if !operand_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Logical NOT operator requires an integer type, found {}",
                            operand_type
                        );
                    }

                    let result = env.builder.new_register();
                    env.builder.add_instruction(MIRInstruction::UnOp {
                        result: result.clone(),
                        op: MIRUnOp::LNOT,
                        operand: operand_val,
                    });

                    MIRValue::Register {
                        register: result,
                        _type: CXTypeKind::Bool.into(),
                    }
                }

                CXUnOp::BNot => {
                    let loaded_op_val = coerce_value(env, expr, operand_val)?;
                    let loaded_op_type = loaded_op_val.get_type();

                    if !loaded_op_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Bitwise NOT operator requires an integer type, found {}",
                            loaded_op_type
                        );
                    }

                    let result = env.builder.new_register();
                    env.builder.add_instruction(MIRInstruction::UnOp {
                        result: result.clone(),
                        op: MIRUnOp::BNOT,
                        operand: loaded_op_val,
                    });

                    MIRValue::Register {
                        register: result,
                        _type: loaded_op_type,
                    }
                }

                CXUnOp::Negative => {
                    let loaded_op_val = coerce_value(env, expr, operand_val)?;
                    let loaded_op_type = loaded_op_val.get_type();

                    let operator = match &loaded_op_type.kind {
                        CXTypeKind::Integer { .. } => MIRUnOp::NEG,
                        CXTypeKind::Float { .. } => MIRUnOp::FNEG,

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand,
                                " Negation operator requires an integer or float type, found {}",
                                loaded_op_type
                            );
                        }
                    };

                    let result = env.builder.new_register();
                    env.builder.add_instruction(MIRInstruction::UnOp {
                        result: result.clone(),
                        op: operator,
                        operand: loaded_op_val,
                    });

                    MIRValue::Register {
                        register: result,
                        _type: loaded_op_type,
                    }
                }

                CXUnOp::AddressOf => {
                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot take address of a non-reference type"
                        );
                    };

                    let register = env.builder.new_register();
                    env.builder.add_instruction(MIRInstruction::Alias {
                        result: register.clone(),
                        value: operand_val,
                    });

                    MIRValue::Register {
                        register,
                        _type: inner.clone().pointer_to(),
                    }
                }

                CXUnOp::Dereference => {
                    // If the operand is a memory reference of a pointer type, we need to load the value first
                    let loaded_operand = coerce_value(env, expr, operand_val)?;
                    let loaded_operand_type = loaded_operand.get_type();

                    let Some(inner) = loaded_operand_type.ptr_inner().cloned() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot dereference a non-pointer type {}",
                            loaded_operand_type
                        );
                    };

                    // coerce_value(&mut operand_val)?;

                    let result = env.builder.new_register();

                    // There are a few things we don't want to actually load, like function pointers
                    // i.e. calling (*func)() is semantically equivalent to just calling func()
                    match &loaded_operand_type.kind {
                        CXTypeKind::Function { .. } => {
                            // Here, we simply create an alias rather than loading the value
                            env.builder.add_instruction(MIRInstruction::Alias {
                                result: result.clone(),
                                value: loaded_operand,
                            });
                        }

                        _ => {
                            env.builder.add_instruction(MIRInstruction::MemoryRead {
                                result: result.clone(),
                                source: loaded_operand,
                                _type: inner.clone(),
                            });
                        }
                    }

                    MIRValue::Register {
                        register: result,
                        _type: inner,
                    }
                }

                CXUnOp::ExplicitCast(to_type) => {
                    let to_type = env.complete_type(base_data, to_type)?;
                    let tc_expr = typecheck_expr(env, base_data, operand)?;

                    let loaded_value = coerce_value(env, expr, tc_expr)?;

                    explicit_cast(env, expr, loaded_value, &to_type)?
                }
            }
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            if let Some(_) = op {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Compound assignment operators (e.g. +=, -=) are not yet supported"
                );
            }

            let lhs_val = typecheck_expr(env, base_data, lhs)?;
            let lhs_type = lhs_val.get_type();

            let rhs_val =
                typecheck_expr(env, base_data, rhs).and_then(|v| coerce_value(env, rhs, v))?;

            let Some(inner) = lhs_type.mem_ref_inner() else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Cannot assign to non-reference type {}",
                    lhs_type
                );
            };

            // TODO: This is probably better to do at the VarDeclaration-level, where its type
            // returned in this function is non-const, but the variable-table reference type
            // is, but for now this will do until we refactor that part.
            if inner.get_specifier(CX_CONST)
                && !matches!(lhs.kind, CXExprKind::VarDeclaration { .. })
            {
                return log_typecheck_error!(env, expr, " Cannot assign to a const type");
            }

            let rhs_val = implicit_cast(env, expr, rhs_val, inner)?;

            env.builder.add_instruction(MIRInstruction::MemoryWrite {
                target: lhs_val,
                value: rhs_val.clone(),
            });

            rhs_val
        }

        CXExprKind::BinOp {
            op: CXBinOp::Is,
            lhs,
            rhs,
        } => typecheck_is(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs,
            rhs,
        } => typecheck_access(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => typecheck_method_call(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp { op, lhs, rhs } => {
            typecheck_binop(env, base_data, op.clone(), lhs, rhs, expr)?
        }

        CXExprKind::Move { .. } => {
            todo!(
                "Intrinsic strong pointers are no longer supported, move semantics for structs are not yet implemented"
            );
        }

        CXExprKind::New { _type } => {
            todo!(
                "Intrinsic strong pointers are no longer supported, new semantics for 'new' tbd."
            );
        }

        CXExprKind::InitializerList { .. } => {
            todo!("Initializer lists to be implemented");
        }

        CXExprKind::TypeConstructor {
            union_name: type_name,
            variant_name: name,
            inner,
        } => {
            let union_type = env.get_type(base_data, type_name.as_str())?;
            let CXTypeKind::TaggedUnion { variants, .. } = &union_type.kind else {
                return log_typecheck_error!(env, expr, " Unknown type: {}", type_name);
            };

            let Some((i, variant_type)) = variants
                .iter()
                .enumerate()
                .find(|(_, (variant_name, _))| variant_name == name.as_str())
                .map(|(i, (_, variant_type))| (i, variant_type.clone()))
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Variant '{}' not found in tagged union type {}",
                    name,
                    type_name
                );
            };

            let inner = typecheck_expr(env, base_data, inner)
                .and_then(|v| implicit_cast(env, expr, v, &variant_type))?;

            let result = env.builder.new_register();
            env.builder
                .add_instruction(MIRInstruction::ConstructSumType {
                    result: result.clone(),
                    value: inner,
                    variant_index: i,
                    sum_type: union_type.clone(),
                });

            MIRValue::Register {
                register: result,
                _type: variant_type,
            }
        }

        CXExprKind::Unit => MIRValue::NULL,

        CXExprKind::SizeOf { expr } => {
            let tc_expr = typecheck_expr(env, base_data, expr)?;
            let tc_type = tc_expr.get_type();

            MIRValue::IntLiteral {
                value: tc_type.type_size() as i64,
                _type: CXIntegerType::I64,
                signed: false,
            }
        }

        CXExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => {
            env.push_scope(None, None);

            let condition_value = typecheck_expr(env, base_data, condition)?;

            env.pop_scope();

            let (default_block, merge_block) = if default_case.is_some() {
                (
                    env.builder.new_named_block_id("default_block"),
                    env.builder.new_named_block_id("merge_block"),
                )
            } else {
                let merge_block = env.builder.new_named_block_id("merge_block");
                (merge_block.clone(), merge_block)
            };

            env.push_scope(None, Some(merge_block.clone()));

            let case_blocks = cases
                .iter()
                .map(|_| env.builder.new_named_block_id("case_block"))
                .collect::<Vec<_>>();

            let mut sorted_cases = cases.clone();
            sorted_cases.sort_by(|a, b| a.1.cmp(&b.1));

            env.builder.add_instruction(MIRInstruction::JumpTable {
                condition: condition_value,
                targets: sorted_cases
                    .iter()
                    .enumerate()
                    .map(|(i, (case, _))| (*case, case_blocks[i].clone()))
                    .collect(),
                default: default_block.clone(),
            });

            let mut case_iter = sorted_cases.iter().map(|(_, i)| *i);
            let mut case_block_iter = case_blocks.iter();
            let mut next_index = case_iter.next();

            for (i, expr) in block.iter().enumerate() {
                while next_index == Some(i) {
                    let case_block = case_block_iter.next().unwrap();
                    next_index = case_iter.next();
                    env.builder.add_and_set_block(case_block.clone());
                }

                if *default_case == Some(i) {
                    env.builder.add_jump(default_block.clone());
                    env.builder.add_and_set_block(default_block.clone());
                }

                typecheck_expr(env, base_data, expr)?;
            }

            env.builder.add_jump(merge_block);
            env.pop_scope();

            MIRValue::NULL
        }

        CXExprKind::Match { .. } => {
            todo!()
        }

        CXExprKind::Taken => {
            unreachable!("Taken expressions should not be present in the typechecker")
        }
    })
}

pub(crate) fn global_expr(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ident: &str,
) -> CXResult<MIRValue> {
    if let Some(global) = env.realized_globals.get(ident) {
        return tcglobal_expr(global);
    }

    let Some(module_res) = base_data.global_variables.get(ident) else {
        return CXError::create_result(format!("Global variable '{}' not found", ident));
    };

    let module_res = match env.in_external_templated_function {
        true => module_res.clone().transfer(""),
        false => module_res.clone(),
    };

    match &module_res.resource {
        CXGlobalVariable::EnumConstant(val) => Ok(MIRValue::IntLiteral {
            value: *val as i64,
            _type: CXIntegerType::from_bytes(8).unwrap(),
            signed: true,
        }),

        CXGlobalVariable::Standard {
            type_,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, type_)?;
            let initializer = match initializer.as_ref() {
                Some(init_expr) => {
                    let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                        return log_typecheck_error!(
                            env,
                            init_expr,
                            " CX currently only supports integer initializers for global variable initialization"
                        );
                    };

                    Some(*val)
                }

                None => None,
            };

            env.realized_globals.insert(
                ident.to_string(),
                TCGlobalVariable {
                    kind: TCGlobalVarKind::Variable {
                        name: CXIdent::from(ident.to_string()),
                        _type,
                        initializer,
                    },
                    is_mutable: *is_mutable,
                    linkage: module_res.linkage,
                },
            );

            tcglobal_expr(env.realized_globals.get(ident).unwrap())
        }
    }
}

fn tcglobal_expr(global: &TCGlobalVariable) -> CXResult<MIRValue> {
    match &global.kind {
        TCGlobalVarKind::Variable { name, _type, .. } => Ok(MIRValue::GlobalValue {
            name: name.clone(),
            _type: _type.clone(),
        }),

        TCGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}
