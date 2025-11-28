use cx_lexer_data::{identifier, keyword, operator, punctuator};
use cx_parsing_data::{
    assert_token_matches,
    data::{
        CXFunctionContract, CXNaiveParameter, CXNaivePrototype, CXNaiveType, CXNaiveTypeKind,
        CXTemplatePrototype, FunctionTypeIdent, NaiveFnKind, PredeclarationType,
    },
    peek_next_kind, try_next,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::parse::{
    expressions::parse_expr,
    parse_std_ident,
    parser::ParserData,
    templates::{convert_template_proto_to_args, try_parse_template},
    types::parse_initializer,
};

pub struct FunctionDeclaration {
    pub prototype: CXNaivePrototype,
    pub template_prototype: Option<CXTemplatePrototype>,
}

fn destructor_prototype(_type: CXNaiveType) -> CXNaivePrototype {
    CXNaivePrototype {
        name: NaiveFnKind::Destructor(FunctionTypeIdent::from_type(&_type).unwrap()),

        return_type: CXNaiveTypeKind::Identifier {
            name: CXIdent::from("void"),
            predeclaration: PredeclarationType::None,
        }
        .to_type(),
        params: vec![],
        var_args: false,
        this_param: true,

        contract: CXFunctionContract::default()
    }
}

pub fn parse_destructor_prototype(data: &mut ParserData) -> CXResult<FunctionDeclaration> {
    assert_token_matches!(data.tokens, operator!(Tilda));

    let name = parse_std_ident(&mut data.tokens)?;
    let template_prototype = try_parse_template(&mut data.tokens)?;

    assert_token_matches!(data.tokens, punctuator!(OpenParen));
    assert_token_matches!(data.tokens, identifier!(this));
    if this.as_str() != "this" {
        return log_preparse_error!(data.tokens, "Destructor can only have 'this' as parameter.");
    }
    assert_token_matches!(data.tokens, punctuator!(CloseParen));

    let _type = match &template_prototype {
        Some(prototype) => CXNaiveTypeKind::TemplatedIdentifier {
            name: name.clone(),
            input: convert_template_proto_to_args(prototype.clone()),
        },
        None => CXNaiveTypeKind::Identifier {
            name: name.clone(),
            predeclaration: PredeclarationType::None,
        },
    };

    let prototype = destructor_prototype(_type.to_type());

    Ok(FunctionDeclaration {
        prototype,
        template_prototype,
    })
}

pub fn try_function_parse(
    data: &mut ParserData,
    return_type: CXNaiveType,
    name: CXIdent,
) -> CXResult<Option<FunctionDeclaration>> {
    let template_prototype = try_parse_template(&mut data.tokens)?;

    match peek_next_kind!(data.tokens)? {
        // e.g:
        // int main()
        //         ^
        // void template_func<int>()
        //                        ^
        punctuator!(OpenParen) => {
            let args = parse_params(data)?;
            let prototype = CXNaivePrototype {
                return_type,
                name: NaiveFnKind::Standard(name.clone()),
                params: args.params,
                var_args: args.var_args,
                this_param: args.contains_this,
                contract: args.contract,
            };

            data.add_function(prototype.clone(), template_prototype.clone());
            Ok(Some(FunctionDeclaration {
                prototype,
                template_prototype,
            }))
        }

        // e.g:
        // void renderer::draw()
        //              ^
        operator!(ScopeRes) => {
            data.tokens.next();

            let _type = match template_prototype {
                // e.g:
                // void vector<int>::push()
                //                 ^
                // We have parsed the `<int>` part as a template prototype rather than
                // a template argument list, so we need to convert it here.
                Some(prototype) => CXNaiveTypeKind::TemplatedIdentifier {
                    name,
                    input: convert_template_proto_to_args(prototype),
                },

                None => CXNaiveTypeKind::Identifier {
                    name,
                    predeclaration: PredeclarationType::None,
                },
            }
            .to_type();

            assert_token_matches!(data.tokens, identifier!(name));
            let name = name.clone();
            let template_prototype = try_parse_template(&mut data.tokens)?;

            let name = NaiveFnKind::MemberFunction {
                _type: FunctionTypeIdent::from_type(&_type).unwrap(),
                function_name: CXIdent::from(name.as_str()),
            };

            let Ok(params) = parse_params(data) else {
                return log_parse_error!(
                    data,
                    "Failed to parse parameters in member function declaration!"
                );
            };

            let prototype = CXNaivePrototype {
                return_type,
                name,
                params: params.params,
                var_args: params.var_args,
                this_param: params.contains_this,
                contract: params.contract,
            };

            data.add_function(prototype.clone(), template_prototype.clone());

            Ok(Some(FunctionDeclaration {
                prototype,
                template_prototype,
            }))
        }

        _ => Ok(None),
    }
}

pub(crate) fn parse_function_contract(
    data: &mut ParserData,
) -> CXResult<CXFunctionContract> {
    if !try_next!(data.tokens, keyword!(Where)) {
        return Ok(CXFunctionContract::default());
    }

    let mut contract = CXFunctionContract {
        precondition: None,
        postcondition: None,
    };

    while let Some(next) = peek_next_kind!(data.tokens).ok() {
        match next {
            keyword!(Precondition) => {
                if contract.precondition.is_some() {
                    return log_parse_error!(data, "Precondition already defined in function contract.");
                }

                data.tokens.next();
                assert_token_matches!(data.tokens, punctuator!(Colon));
                assert_token_matches!(data.tokens, punctuator!(OpenParen));
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen));

                contract.precondition = Some(expr);
            }
            keyword!(Postcondition) => {
                if contract.postcondition.is_some() {
                    return log_parse_error!(data, "Postcondition already defined in function contract.");
                }

                data.tokens.next();
                
                let return_val_name = if try_next!(data.tokens, punctuator!(OpenParen)) {
                    assert_token_matches!(data.tokens, identifier!(ret));
                    let name = CXIdent::from(ret.as_str());
                    
                    assert_token_matches!(data.tokens, punctuator!(CloseParen));
                    Some(name)
                } else {
                    None
                };
                
                assert_token_matches!(data.tokens, punctuator!(Colon));
                assert_token_matches!(data.tokens, punctuator!(OpenParen));
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen));

                contract.postcondition = Some((return_val_name, expr));
            }
            _ => break,
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    return Ok(contract);
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<CXNaiveParameter>,
    pub(crate) var_args: bool,
    pub(crate) contains_this: bool,
    pub(crate) contract: CXFunctionContract,
}

pub(crate) fn parse_params(data: &mut ParserData) -> CXResult<ParseParamsResult> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen));

    let mut params = Vec::new();
    let mut contains_this = false;

    if matches!(peek_next_kind!(data.tokens), Ok(identifier!(this)) if this.as_str() == "this") {
        data.tokens.next();
        contains_this = true;

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen));
            let contract = parse_function_contract(data)?;

            return Ok(ParseParamsResult {
                params,
                var_args: false,
                contains_this,
                contract,
            });
        }
    };

    while !try_next!(data.tokens, punctuator!(CloseParen)) {
        if try_next!(data.tokens, punctuator!(Ellipsis)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen));
            let contract = parse_function_contract(data)?;

            return Ok(ParseParamsResult {
                params,
                var_args: true,
                contains_this,
                contract,
            });
        }

        if let Ok((name, _type)) = parse_initializer(data) {
            let name = name;

            params.push(CXNaiveParameter { name, _type });
        } else {
            return log_parse_error!(data, "Failed to parse parameter in function call");
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen));
            break;
        }
    }

    let contract = parse_function_contract(data)?;

    Ok(ParseParamsResult {
        params,
        var_args: false,
        contains_this,
        contract,
    })
}
