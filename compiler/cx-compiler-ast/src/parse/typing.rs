use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::parse::expression::parse_name;
use cx_data_ast::parse::ast::{TypeMap, CXAST};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::{CXTypeSpecifier, CXTypeUnion, CXValType, CX_CONST, CX_VOLATILE};
use cx_util::log_error;
use crate::parse::global_scope::parse_params;
use crate::parse::parsing_tools::{goto_block_end, goto_statement_end};

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXValType,
}

pub fn is_type_decl(data: &mut ParserData) -> bool {
    let tok = data.toks.peek();

    if tok.is_none() {
        return false;
    }

    match tok.unwrap() {
        Token::Intrinsic(_) => true,
        Token::Specifier(_) => true,

        Token::Keyword(KeywordType::Struct) |
        Token::Keyword(KeywordType::Union) |
        Token::Keyword(KeywordType::Enum) => true,

        Token::Identifier(name) => data.type_symbols.contains(name),

        _ => false
    }
}

pub fn parse_types(data: &mut ParserData) -> Option<TypeMap> {
    let mut type_map = TypeMap::new();

    while let Some(token) = data.toks.peek() {
        let type_record = match token {
            Token::Keyword(KeywordType::Typedef) =>
                parse_typedef(data)?,

            Token::Keyword(KeywordType::Struct) |
            Token::Keyword(KeywordType::Enum) |
            Token::Keyword(KeywordType::Union) =>
                parse_plain_typedef(data)?,

            _ => {
                goto_statement_end(data);
                continue;
            }
        };

        if let Some(name) = type_record.name {
            type_map.insert(name.clone(), type_record.type_);
            data.type_symbols.insert(name);
        }
    }

    Some(type_map)
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Typedef));
    
    let (name, type_) = parse_initializer(data)?;

    if name.is_none() {
        log_error!("PARSER ERROR: Invalid typedef declaration with no name!");
    }

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(
        TypeRecord {
            name: Some(name?.to_string()),
            type_
        }
    )
}

pub(crate) fn parse_plain_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    match data.toks.peek()? {
        Token::Keyword(KeywordType::Struct) => {
            let type_ = parse_struct(data)?;
            let CXTypeUnion::Structured { name, .. } = &type_ else {
                log_error!("PARSER ERROR: Expected struct type, found: {:#?}", type_);
            };
            let name = match name {
                Some(name) => Some(name.to_string()),
                None => {
                    println!("PARSER Warning: Nameless global struct type is effective no-op");
                    None
                }
            };
            try_next!(data, Token::Punctuator(PunctuatorType::Semicolon));

            Some(
                TypeRecord {
                    name,
                    type_: CXValType::new(0, type_)
                }
            )
        },

        tok => todo!("parse_plain_typedef: {tok:?}")
    }
}

pub(crate) fn parse_struct(data: &mut ParserData) -> Option<CXTypeUnion> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Struct));

    let name = parse_std_ident(data);
    let mut fields = Vec::new();

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    while data.toks.peek() != Some(&Token::Punctuator(PunctuatorType::CloseBrace)) {
        let (name, _type) = parse_initializer(data)?;
        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

        fields.push(
            (name?.to_string(), _type)
        )
    }

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        CXTypeUnion::Structured {
            name,
            fields,
        }
    )
}

fn parse_specifier(data: &mut ParserData) -> CXTypeSpecifier {
    let mut spec_acc = 0;

    while let Some(Token::Specifier(spec)) = data.toks.next() {
        match spec {
            SpecifierType::Const => spec_acc |= CX_CONST,
            SpecifierType::Volatile => spec_acc |= CX_VOLATILE,
            SpecifierType::Restrict => spec_acc |= CX_VOLATILE,

            _ => break
        }
    }

    data.back();
    spec_acc
}

fn parse_typemod_name(data: &mut ParserData, acc_type: CXValType) -> Option<(Option<CXIdent>, CXValType)> {
    let Some(next_tok) = data.toks.peek() else {
        return Some((None, acc_type));
    };

    match next_tok {
        Token::Operator(OperatorType::Asterisk) => {
            data.toks.next();
            let specs = parse_specifier(data);
            let acc_type = acc_type.pointer_to().add_specifier(specs);

            parse_typemod_name(data, acc_type)
        },

        Token::Punctuator(PunctuatorType::OpenParen) => {
            data.toks.next();
            assert_token_matches!(data, Token::Operator(OperatorType::Asterisk));
            let name = parse_std_ident(data);
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            let params = parse_params(data)?
                .into_iter()
                .map(|params| {
                    if params.name.is_some() {
                        log_error!("Function pointer parameters cannot have names");
                    }

                    Some(params.type_)
                })
                .collect::<Option<Vec<_>>>()?;

            let acc_type = CXValType::new(
                0,
                CXTypeUnion::PointerTo(
                    Box::new(CXValType::new(
                        0,
                        CXTypeUnion::Function {
                            return_type: Box::new(acc_type),
                            args: params
                        }
                    ))
                )
            );

            Some((name, acc_type))
        },

        Token::Identifier(_) => Some((Some(parse_std_ident(data)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

fn parse_suffix_typemod(data: &mut ParserData, acc_type: CXValType) -> Option<CXValType> {
    let Some(next_tok) = data.toks.peek() else {
        return Some(acc_type);
    };

    match next_tok {
        Token::Punctuator(PunctuatorType::OpenBracket) => {
            data.toks.next();

            if try_next!(data, Token::Punctuator(PunctuatorType::CloseBracket)) {
                return Some(acc_type.pointer_to());
            }

            let Token::IntLiteral(size) = data.toks.next().cloned()? else {
                println!("Error parsing type, acc_type = {acc_type}");
                log_error!("PARSER ERROR: Expected integer literal for array size, found: {:#?}", data.back().toks.peek());
            };
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBracket));

            Some(
                CXValType::new(
                    0,
                    CXTypeUnion::Array {
                        size: size.clone() as usize,
                        _type: Box::new(acc_type)
                    }
                )
            )
        },

        _ => Some(acc_type),
    }
}

fn parse_type_base(data: &mut ParserData) -> Option<CXValType> {
    match data.toks.peek()? {
        Token::Identifier(_) => Some(
            CXValType::new(
                parse_specifier(data),
                CXTypeUnion::Identifier(parse_std_ident(data)?)
            )
        ),
        Token::Intrinsic(_) => Some(
            CXValType::new(
                parse_specifier(data),
                CXTypeUnion::Identifier(parse_intrinsic(data)?)
            )
        ),

        Token::Keyword(KeywordType::Struct) => Some(
            CXValType::new(
                parse_specifier(data),
                parse_struct(data)?
            )
        ),

        _ => None
    }
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<CXIdent>, CXValType)> {
    let prefix_specs = parse_specifier(data);
    let type_base = parse_type_base(data)?;
    let (name, modified_type)
        = parse_typemod_name(data, type_base.add_specifier(prefix_specs))?;

    Some((name, parse_suffix_typemod(data, modified_type)?))
}