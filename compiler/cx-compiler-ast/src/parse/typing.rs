use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::parse::expression::parse_name;
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXTypeMap, CXAST};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::macros::error_pointer;
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::{CXTypeSpecifier, CXTypeKind, CXType, CX_CONST, CX_VOLATILE};
use cx_util::{log_error, point_log_error};
use crate::parse::global_scope::{parse_params, ParseParamsResult};
use crate::parse::parsing_tools::{goto_block_end, goto_statement_end};

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXType,
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

pub fn parse_types(data: &mut ParserData) -> Option<CXTypeMap> {
    let mut type_map = CXTypeMap::new();

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
            
            // parse_struct returned some "struct [identifier]", which is a placeholder
            // type that need to be processed by the type parser.
            // alternatively parse_struct returned a nameless struct declaration, which
            // is an effective no-op.
            let CXTypeKind::Structured { name: Some(name), .. } = &type_ else {
                goto_statement_end(data);
                
                // this is janky but returning a None here indicates an error,
                // not that no type needs to be parsed.
                return Some(TypeRecord { name: None, type_: CXType::unit() });
            };
            
            try_next!(data, Token::Punctuator(PunctuatorType::Semicolon));

            Some(
                TypeRecord {
                    name: Some(name.to_string()),
                    type_: CXType::new(0, type_)
                }
            )
        },

        tok => todo!("parse_plain_typedef: {tok:?}")
    }
}

pub(crate) fn parse_struct(data: &mut ParserData) -> Option<CXTypeKind> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Struct));

    let name = parse_std_ident(data);
    
    if !try_next!(data, Token::Punctuator(PunctuatorType::OpenBrace)) {
        return Some(CXTypeKind::Identifier(name?));
    }
    
    let mut fields = Vec::new();

    while data.toks.peek() != Some(&Token::Punctuator(PunctuatorType::CloseBrace)) {
        let specifiers = parse_specifier(data);
        let base_type = parse_type_base(data)?.add_specifier(specifiers);

        loop {
            let (Some(name), _type) = parse_base_mods(data, base_type.clone())? else {
                log_error!("UNSUPPORTED: Nameless struct members");
            };

            fields.push((name.data, _type));

            if try_next!(data, Token::Operator(OperatorType::Comma)) {
                continue;
            }

            break;
        };

        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
    }

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        CXTypeKind::Structured {
            name,
            fields,
        }
    )
}

pub(crate) fn parse_specifier(data: &mut ParserData) -> CXTypeSpecifier {
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

pub(crate) fn parse_typemods(data: &mut ParserData, acc_type: CXType) -> Option<(Option<CXIdent>, CXType)> {
    let Some(next_tok) = data.toks.peek() else {
        return Some((None, acc_type));
    };

    match next_tok {
        Token::Operator(OperatorType::Asterisk) => {
            data.toks.next();
            let specs = parse_specifier(data);
            let acc_type = acc_type.pointer_to().add_specifier(specs);

            parse_typemods(data, acc_type)
        },

        Token::Punctuator(PunctuatorType::OpenParen) => {
            data.toks.next();
            assert_token_matches!(data, Token::Operator(OperatorType::Asterisk));
            let name = parse_std_ident(data);
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            let ParseParamsResult { params, var_args } = parse_params(data)?;

            let prototype = CXFunctionPrototype {
                name: CXIdent::from("INTERNAL_FUNCTION_PTR_TYPE"),
                return_type: acc_type,
                params: params,
                var_args
            };

            Some((
                name,
                CXTypeKind::Function { prototype: Box::new(prototype) }.to_val_type().pointer_to()
            ))
        },

        Token::Identifier(_) => Some((Some(parse_std_ident(data)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

pub(crate) fn parse_suffix_typemod(data: &mut ParserData, acc_type: CXType) -> Option<CXType> {
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
                CXType::new(
                    0,
                    CXTypeKind::Array {
                        size: size.clone() as usize,
                        _type: Box::new(acc_type)
                    }
                )
            )
        },

        _ => Some(acc_type),
    }
}

pub(crate) fn parse_type_base(data: &mut ParserData) -> Option<CXType> {
    match data.toks.peek()? {
        Token::Identifier(_) => Some(
            CXType::new(
                parse_specifier(data),
                CXTypeKind::Identifier(parse_std_ident(data)?)
            )
        ),
        Token::Intrinsic(_) => Some(
            CXType::new(
                parse_specifier(data),
                CXTypeKind::Identifier(parse_intrinsic(data)?)
            )
        ),

        Token::Keyword(KeywordType::Struct) => Some(
            CXType::new(
                parse_specifier(data),
                parse_struct(data)?
            )
        ),

        _ => None
    }
}

pub(crate) fn parse_base_mods(data: &mut ParserData, acc_type: CXType) -> Option<(Option<CXIdent>, CXType)> {
    let (name, modified_type) = parse_typemods(data, acc_type)?;

    Some((name, parse_suffix_typemod(data, modified_type)?))
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<CXIdent>, CXType)> {
    let prefix_specs = parse_specifier(data);
    let type_base = parse_type_base(data)?;

    parse_base_mods(data, type_base.add_specifier(prefix_specs))
}