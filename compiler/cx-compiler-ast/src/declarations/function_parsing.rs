use cx_data_ast::{
    assert_token_matches, peek_next_kind,
    preparse::{
        naive_types::{
            CXNaiveParameter, CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType,
        },
        CXNaiveFnIdent,
    },
    try_next,
};
use cx_data_lexer::{identifier, operator, punctuator, TokenIter};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{declarations::{
    data_parsing::{convert_template_proto_to_args, parse_std_ident, try_parse_template},
    type_parsing::parse_initializer,
    FunctionDeclaration,
}, definitions::global_scope::destructor_prototype};

pub fn parse_destructor_prototype(tokens: &mut TokenIter) -> CXResult<FunctionDeclaration> {
    assert_token_matches!(tokens, operator!(Tilda));

    let Some(name) = parse_std_ident(tokens) else {
        log_preparse_error!(tokens, "Expected type name.");
    };
    
    let template_prototype = try_parse_template(tokens);
    
    assert_token_matches!(tokens, punctuator!(OpenParen));
    assert_token_matches!(tokens, identifier!(this));
    if this.as_str() != "this" {
        log_preparse_error!(tokens, "Destructor can only have 'this' as parameter.");
    }
    assert_token_matches!(tokens, punctuator!(CloseParen));
    
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
    
    Some(
        FunctionDeclaration {
            prototype,
            template_prototype,
        }
    )
}

pub fn try_function_parse(
    tokens: &mut TokenIter,
    return_type: CXNaiveType,
    name: CXIdent,
) -> CXResult<Option<FunctionDeclaration>> {
    let template_prototype = try_parse_template(tokens);

    match tokens.peek()?.kind {
        // e.g:
        // int main()
        //         ^
        // void template_func<int>()
        //                        ^
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
                template_prototype,
            }))
        }

        // e.g:
        // void renderer::draw()
        //              ^
        operator!(ScopeRes) => {
            tokens.next();

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
            }.to_type();

            assert_token_matches!(tokens, identifier!(name));
            let name = name.clone();
            let template_prototype = try_parse_template(tokens);

            let name = CXNaiveFnIdent::MemberFunction {
                _type,
                function_name: CXIdent::from(name.as_str()),
            };

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
