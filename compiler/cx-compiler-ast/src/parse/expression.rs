use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use crate::parse::global_scope::{parse_body};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::type_mapping::{contextualize_template_args, contextualize_type};
use cx_data_ast::parse::value_type::CXTypeKind;
use crate::parse::operators::{binop_prec, parse_binop, parse_post_unop, parse_pre_unop, unop_prec, PrecOperator};
use cx_util::{log_error, point_log_error};
use crate::parse::structured_initialization::parse_structured_initialization;
use crate::parse::typing::is_type_decl;
use crate::preparse::preparser::{parse_intrinsic, parse_std_ident};
use crate::preparse::typing::{parse_base_mods, parse_initializer, parse_specifier, parse_template_args, parse_type_base};

pub(crate) fn requires_semicolon(expr: &CXExpr) -> bool {
    match expr.kind {
        CXExprKind::Defer { .. }    |
        CXExprKind::If { .. }       |
        CXExprKind::While { .. }    |
        CXExprKind::For { .. }      |
        CXExprKind::Switch { .. }   => false,

        _ => true
    }
}

pub(crate) fn parse_expr(data: &mut ParserData) -> Option<CXExpr> {
    if is_type_decl(data) {
        return parse_declaration(data);
    }

    if try_next!(data.tokens, TokenKind::Keyword(_)) {
        data.tokens.back();
        
        if let Some(expr) = parse_keyword_expr(data) {
            return Some(expr);
        }
    }

    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    let Some(_) = parse_expr_val(data, &mut expr_stack, &mut op_stack) else {
        return Some(CXExprKind::Unit.into_expr(0, 0));
    };

    while let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack) {}

    compress_stack(&mut expr_stack, &mut op_stack, 100)?;

    let Some(expr) = expr_stack.pop() else {
        log_error!("Failed to parse expression value after operator: {:#?}", data.tokens.peek());
    };

    if !expr_stack.is_empty() {
        log_error!("PARSER ERROR: Expression stack is not empty after parsing expression: {:#?} {:#?}", expr_stack, op_stack);
    }

    Some(expr)
}

pub(crate) fn parse_declaration(data: &mut ParserData) -> Option<CXExpr> {
    let start_index = data.tokens.index;
    
    let specifiers = parse_specifier(&mut data.tokens);
    let base_type = parse_type_base(&mut data.tokens)?.add_specifier(specifiers);

    let mut decls = Vec::new();
    data.change_comma_mode(false);

    loop {
        let (Some(name), mut _type) = parse_base_mods(&mut data.tokens, base_type.clone())? else {
            log_error!("PARSER ERROR: Failed to parse type declaration");
        };
        _type.specifiers = specifiers;

        let cx_type = contextualize_type(&data.ast.type_map, &_type)
            .expect("PARSER ERROR: Failed to contextualize type in declaration");

        if try_next!(data.tokens, TokenKind::Assignment(None)) {
            let lhs_end_index = data.tokens.index;
            let expr = parse_expr(data)?;
            decls.push(
                CXExprKind::BinOp {
                    lhs: Box::new(CXExprKind::VarDeclaration { type_: cx_type, name }.into_expr(start_index, lhs_end_index)),
                    rhs: Box::new(expr),
                    op: CXBinOp::Assign(None)
                }.into_expr(start_index, data.tokens.index)
            )
        } else {
            decls.push(
                CXExprKind::VarDeclaration { 
                    type_: cx_type,
                    name: name.clone() 
                }.into_expr(start_index, data.tokens.index)
            );
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    data.pop_comma_mode();

    if decls.len() == 1 {
        Some(decls.pop().unwrap())
    } else {
        Some(
            CXExprKind::Block {
                exprs: decls,
                value: None
            }.into_expr(start_index, data.tokens.index)
        )
    }
}

pub(crate) fn parse_expr_op_concat(data: &mut ParserData, expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<()> {
    let Some(op) = parse_binop(data) else {
        return None;
    };

    let op_prec = binop_prec(op.clone());
    compress_stack(expr_stack, op_stack, op_prec)?;

    op_stack.push(PrecOperator::BinOp(op));

    let Some(_) = parse_expr_val(data, expr_stack, op_stack) else {
        log_error!("PARSER ERROR: Failed to parse expression value after operator: {:#?}", data.tokens.peek());
    };

    Some(())
}

fn compress_one_expr(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<CXExpr> {
    let op = op_stack.pop().unwrap();

    match op {
        PrecOperator::UnOp(un_op) => {
            let rhs = expr_stack.pop().unwrap();

            let start_index = rhs.start_index;
            let end_index = rhs.end_index;
            
            let acc = CXExprKind::UnOp {
                operator: un_op,
                operand: Box::new(rhs)
            };

            Some(acc.into_expr(start_index, end_index))
        },
        PrecOperator::BinOp(bin_op) => {
            let rhs = expr_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let start_index = lhs.start_index;
            let end_index = rhs.end_index;

            let acc = CXExprKind::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin_op
            };

            Some(acc.into_expr(start_index, end_index))
        }
    }
}

pub(crate) fn compress_stack(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>, rprec: u8) -> Option<()> {
    if op_stack.is_empty() {
        return Some(());
    }

    while let Some(op2) = op_stack.last() {
        if op2.get_precedence() > rprec {
            break;
        }

        let expr = compress_one_expr(expr_stack, op_stack)?;
        expr_stack.push(expr);
    }

    Some(())
}

pub(crate) fn parse_expr_val(data: &mut ParserData, expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<()> {
    let start_index = data.tokens.index;
    
    while let Some(op) = parse_pre_unop(data) {
        op_stack.push(PrecOperator::UnOp(op));
    }

    let acc = match &data.tokens.next()?.kind {
        TokenKind::IntLiteral(value) =>
            CXExprKind::IntLiteral { bytes: 4, val: *value },
        TokenKind::FloatLiteral(value) =>
            CXExprKind::FloatLiteral { bytes: 4, val: *value },
        TokenKind::StringLiteral(value) =>
            CXExprKind::StringLiteral { val: value.clone() },

        TokenKind::Intrinsic(_) =>
            CXExprKind::Identifier(parse_intrinsic(&mut data.back().tokens)?),
        TokenKind::Identifier(_) => {
            data.back();
            parse_expr_identifier(data)?
        },
        
        TokenKind::Operator(OperatorType::Move) => {
            let expr = parse_expr(data)?;
            
            CXExprKind::Move { expr: Box::new(expr) }
        },

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            if try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen)) {
                expr_stack.push(CXExprKind::Unit.into_expr(0, 0));
                return Some(());
            }

            data.change_comma_mode(true);

            let expr = parse_expr(data)?;

            data.pop_comma_mode();

            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));

            expr.kind
        },
        
        TokenKind::Punctuator(PunctuatorType::OpenBrace) => {
            data.tokens.back();
            
            parse_structured_initialization(data)?.kind
        }
        
        TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
            if try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBracket)) {
                expr_stack.push(CXExprKind::Unit.into_expr(start_index, data.tokens.index));
                return Some(());
            }

            let index = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBracket));

            index.kind
        },
        
        TokenKind::Keyword(KeywordType::Sizeof) => {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
            
            let return_type = if is_type_decl(data) {
                let Some((None, _type)) = parse_initializer(&mut data.tokens) else {
                    log_error!("PARSER ERROR: Failed to parse type declaration for sizeof");
                };
                let cx_type = contextualize_type(&data.ast.type_map, &_type)
                    .expect("PARSER ERROR: Failed to contextualize type in sizeof declaration");

                CXExprKind::SizeOf {
                    expr: Box::new(
                        CXExprKind::VarDeclaration {
                            name: CXIdent::from("__internal_sizeof_dummy_decl"),
                            type_: cx_type
                        }.into_expr(start_index, data.tokens.index)
                    )
                }
            } else {
                let expr = parse_expr(data)?;

                CXExprKind::SizeOf {
                    expr: Box::new(expr)
                }
            };
            
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            
            return_type
        },
        
        TokenKind::Keyword(KeywordType::New) => {
            let Some((None, _type)) = parse_initializer(&mut data.tokens) else {
                log_error!("PARSER ERROR: Failed to parse type declaration for new");
            };
            let cx_type = contextualize_type(&data.ast.type_map, &_type)
                .expect("PARSER ERROR: Failed to contextualize type in new declaration");
            
            match cx_type.kind {
                CXTypeKind::Array { size, inner_type } => {
                    let length = CXExprKind::IntLiteral {
                        bytes: 8,
                        val: size as i64
                    };
                    
                    CXExprKind::New {
                        _type: *inner_type,
                        array_length: Some(Box::new(length.into_expr(start_index, data.tokens.index)))
                    }
                }
                CXTypeKind::VariableLengthArray { size, _type: inner_type } => {
                    CXExprKind::New {
                        _type: *inner_type,
                        array_length: Some(size)
                    }
                },
                _ => CXExprKind::New { _type: cx_type, array_length: None },
            }
        },

        _ => {
            data.back();
            return None
        }
    }.into_expr(start_index, data.tokens.index);

    expr_stack.push(acc);

    while let Some(op) = parse_post_unop(data) {
        let prec = unop_prec(op.clone());

        compress_stack(expr_stack, op_stack, prec);
        op_stack.push(PrecOperator::UnOp(op));
    }

    Some(())
}

pub(crate) fn parse_expr_identifier(data: &mut ParserData) -> Option<CXExprKind> {
    let ident = parse_std_ident(&mut data.tokens)?;
    
    if data.ast.function_map.has_template(ident.as_str()) {
        let Some(args) = parse_template_args(&mut data.tokens) else {
            point_log_error!(data.tokens, "PARSER ERROR: Failed to parse template arguments for function identifier: {:#?}", ident);
        };
        let Some(contextualized) = contextualize_template_args(&data.ast.type_map, &args) else {
            point_log_error!(data.tokens, "PARSER ERROR: Failed to contextualize template arguments for function identifier: {:#?}", ident);
        };

        return Some(
            CXExprKind::TemplatedFnIdent {
                fn_name: ident,
                template_input: contextualized
            }
        );
    }
    
    Some(CXExprKind::Identifier(ident))
}

pub(crate) fn parse_keyword_expr(data: &mut ParserData) -> Option<CXExpr> {
    let start_index = data.tokens.index;
    
    let TokenKind::Keyword(keyword_type) = data.tokens.next()?.kind else {
        unreachable!("PANIC: parse_keyword_expr called with non-keyword token: {:#?}", data.tokens.peek());
    };
    
    match keyword_type {
        KeywordType::Return => {
            let val = parse_expr(data)?;

            if matches!(val.kind, CXExprKind::Unit) {
                return Some(CXExprKind::Return { value: None }.into_expr(start_index, data.tokens.index));
            }

            Some(
                CXExprKind::Return {
                    value: Some(Box::new(val))
                }
            )
        },
        KeywordType::If => {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let then_body = parse_body(data)?;
            let else_body =
                if try_next!(data.tokens, TokenKind::Keyword(KeywordType::Else)) {
                    parse_body(data)
                } else {
                    None
                };

            Some(
                CXExprKind::If {
                    condition: Box::new(expr),
                    then_branch: Box::new(then_body),
                    else_branch: else_body.map(Box::new)
                }
            )
        },
        KeywordType::Switch => {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenBrace));
            
            let mut block = Vec::new();
            let mut cases = Vec::new();
            let mut default_case = None;
            let mut index = 0;
            
            while !try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
                if try_next!(data.tokens, TokenKind::Keyword(KeywordType::Case)) {
                    assert_token_matches!(data.tokens, TokenKind::IntLiteral(case_value));
                    cases.push((*case_value as u64, index));
                    
                    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));
                    continue;
                } else if try_next!(data.tokens, TokenKind::Keyword(KeywordType::Default)) {
                    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));
                    if default_case.is_some() {
                        log_error!("PARSER ERROR: Multiple default cases in switch statement");
                    }
                    default_case = Some(index);
                    continue;
                }
                
                let expr = parse_expr(data)?;
                index += 1;
                if requires_semicolon(&expr) {
                    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));
                }
                block.push(expr);
            }
            
            Some(
                CXExprKind::Switch {
                    condition: Box::new(expr),
                    block,
                    cases,
                    default_case
                }
            )
        },
        KeywordType::Do => {
            let body = parse_body(data)?;
            assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::While));
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));

            Some(
                CXExprKind::While {
                    condition: Box::new(expr),
                    body: Box::new(body),
                    pre_eval: false,
                }
            )
        },
        KeywordType::While => {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let body = parse_body(data)?;

            Some(
                CXExprKind::While {
                    condition: Box::new(expr),
                    body: Box::new(body),
                    pre_eval: true
                }
            )
        },
        KeywordType::Break => Some(CXExprKind::Break),
        KeywordType::Continue => Some(CXExprKind::Continue),
        KeywordType::For => {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));

            let init = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));

            let condition = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));

            let increment = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));

            let body = parse_body(data)?;

            Some(
                CXExprKind::For {
                    init: Box::new(init),
                    condition: Box::new(condition),
                    increment: Box::new(increment),
                    body: Box::new(body)
                }
            )
        },
        
        KeywordType::Defer => {
            let body = parse_body(data)?;
            Some(CXExprKind::Defer { expr: Box::new(body) })
        },

        _ => {
            data.tokens.back();
            return None
        }
    }.map(|e| e.into_expr(start_index, data.tokens.index))
}