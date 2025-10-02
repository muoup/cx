use cx_data_ast::{
    assert_token_matches, peek_next, peek_next_kind,
    preparse::{
        naive_types::{
            CXNaiveParameter, CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType,
        },
        templates::CXTemplatePrototype,
        CXNaiveFnIdent,
    },
    try_next,
};
use cx_data_lexer::{identifier, operator, punctuator, TokenIter};
use cx_util::{identifier::CXIdent, CXResult};

use crate::declarations::{
    data_parsing::{parse_template_args, parse_template_prototype, try_parse_template},
    type_parsing::parse_initializer,
    FunctionDeclaration,
};

pub fn try_function_parse(
    tokens: &mut TokenIter,
    return_type: CXNaiveType,
    name: CXIdent,
) -> CXResult<Option<FunctionDeclaration>> {
    match tokens.peek()?.kind {
        // e.g:
        // int main()
        //         ^
        punctuator!(OpenParen) => {
            let Some(args) = parse_params(tokens) else {
                log_preparse_error!(
                    tokens,
                    "Failed to parse parameters in function declaration!"
                );
            };

            Some(Some(FunctionDeclaration {
                prototype: CXNaivePrototype {
                    return_type,
                    name: CXNaiveFnIdent::Standard(name),
                    params: args.params,
                    var_args: args.var_args,
                    this_param: args.contains_this,
                },
                template_prototype: None,
            }))
        }

        // e.g: void templated_function<T>()
        //                             ^
        operator!(Less) => {
            let Some(template_prototype) = parse_template_prototype(tokens) else {
                return Some(None);
            };

            let Some(params) = parse_params(tokens) else {
                log_preparse_error!(
                    tokens,
                    "Failed to parse parameters in function declaration!"
                );
            };

            Some(Some(FunctionDeclaration {
                prototype: CXNaivePrototype {
                    return_type,
                    name: CXNaiveFnIdent::Standard(name),
                    params: params.params,
                    var_args: params.var_args,
                    this_param: params.contains_this,
                },
                template_prototype: Some(template_prototype),
            }))
        }

        // e.g:
        // void vec<int>::push()
        //         ^
        operator!(Less) => {
            let Some(params) = parse_template_args(tokens) else {
                return Some(None);
            };

            let _type = name;

            assert_token_matches!(tokens, operator!(ScopeRes));
            assert_token_matches!(tokens, identifier!(name));
            let fn_name = CXIdent::from(name.as_str());

            let template_prototype = try_parse_template(tokens);
            assert_token_matches!(tokens, punctuator!(OpenParen));

            let Some(args) = parse_params(tokens) else {
                log_preparse_error!(
                    tokens,
                    "Failed to parse parameters in function declaration!"
                );
            };

            let name = CXNaiveFnIdent::MemberFunction {
                _type: CXNaiveTypeKind::TemplatedIdentifier {
                    name: CXIdent::from(_type.as_str()),
                    input: params,
                }
                .to_type(),
                function_name: fn_name,
            };

            Some(Some(FunctionDeclaration {
                prototype: CXNaivePrototype {
                    return_type,
                    name,
                    params: args.params,
                    var_args: args.var_args,
                    this_param: args.contains_this,
                },
                template_prototype,
            }))
        }

        // e.g:
        // void renderer::draw()
        //              ^
        operator!(ScopeRes) => {
            tokens.next();
            let _type = name.as_string();
            assert_token_matches!(tokens, identifier!(name));
            let name = CXNaiveFnIdent::MemberFunction {
                _type: CXNaiveTypeKind::Identifier {
                    name: CXIdent::from(_type.as_str()),
                    predeclaration: PredeclarationType::None,
                }
                .to_type(),
                function_name: CXIdent::from(name.as_str()),
            };

            let template_prototype = try_parse_template(tokens);
            let Some(params) = parse_params(tokens) else {
                log_preparse_error!(
                    tokens,
                    "Failed to parse parameters in member function declaration!"
                );
            };

            Some(Some(FunctionDeclaration {
                prototype: CXNaivePrototype {
                    return_type,
                    name,
                    params: params.params,
                    var_args: params.var_args,
                    this_param: params.contains_this,
                },
                template_prototype,
            }))
        }

        _ => Some(None),
    }
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<CXNaiveParameter>,
    pub(crate) var_args: bool,
    pub(crate) contains_this: bool,
}

pub(crate) fn parse_params(tokens: &mut TokenIter) -> CXResult<ParseParamsResult> {
    assert_token_matches!(tokens, punctuator!(OpenParen));

    let mut params = Vec::new();
    let mut contains_this = false;

    if matches!(peek_next_kind!(tokens), Some(identifier!(this)) if this.as_str() == "this") {
        tokens.next();
        contains_this = true;

        if !try_next!(tokens, operator!(Comma)) {
            assert_token_matches!(tokens, punctuator!(CloseParen));

            return Some(ParseParamsResult {
                params,
                var_args: false,
                contains_this,
            });
        }
    };

    while !try_next!(tokens, punctuator!(CloseParen)) {
        if try_next!(tokens, punctuator!(Ellipsis)) {
            assert_token_matches!(tokens, punctuator!(CloseParen));
            return Some(ParseParamsResult {
                params,
                var_args: true,
                contains_this,
            });
        }

        if let Some((name, type_)) = parse_initializer(tokens) {
            let name = name;

            params.push(CXNaiveParameter { name, _type: type_ });
        } else {
            log_preparse_error!(tokens, "Failed to parse parameter in function call");
        }

        if !try_next!(tokens, operator!(Comma)) {
            assert_token_matches!(tokens, punctuator!(CloseParen));
            break;
        }
    }

    Some(ParseParamsResult {
        params,
        var_args: false,
        contains_this,
    })
}
