use cx_data_ast::{assert_token_matches, next_kind, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXTypeMap};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use cx_data_ast::parse::value_type::{CXTypeSpecifier, CXTypeKind, CXType, CX_CONST, CX_VOLATILE, PredeclarationType};
use cx_util::{log_error, point_log_error};
use crate::parse::expression::parse_expr;
use crate::parse::global_scope::{parse_import, parse_params, ParseParamsResult};
use crate::parse::parsing_tools::goto_statement_end;

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXType,
}

pub fn is_type_decl(data: &mut ParserData) -> bool {
    let tok = data.toks.peek();

    if tok.is_none() {
        return false;
    }

    match &tok.unwrap().kind {
        TokenKind::Intrinsic(_) => true,
        TokenKind::Specifier(_) => true,

        TokenKind::Keyword(KeywordType::Struct) |
        TokenKind::Keyword(KeywordType::Union) |
        TokenKind::Keyword(KeywordType::Enum) => true,

        TokenKind::Identifier(name) => data.type_symbols.contains(name),

        _ => false
    }
}

pub fn parse_types(data: &mut ParserData) -> Option<(CXTypeMap, Vec<String>, Vec<String>)> {
    let mut type_map = CXTypeMap::new();
    let mut public_types = Vec::new();
    let mut imports = Vec::new();

    while let Some(token) = data.toks.peek() {
        let type_record = match &token.kind {
            TokenKind::Keyword(KeywordType::Typedef) =>
                parse_typedef(data)?,

            TokenKind::Keyword(KeywordType::Struct) |
            TokenKind::Keyword(KeywordType::Enum) |
            TokenKind::Keyword(KeywordType::Union) =>
                parse_plain_typedef(data)?,
            
            TokenKind::Keyword(KeywordType::Import) => {
                imports.push(
                    parse_import(data)?
                );
                continue;
            },

            TokenKind::Specifier(SpecifierType::Public) => {
                data.visibility = VisibilityMode::Public;
                data.toks.next();
                try_next!(data, TokenKind::Punctuator(PunctuatorType::Colon));
                continue;
            },

            TokenKind::Specifier(SpecifierType::Private) => {
                data.visibility = VisibilityMode::Private;
                data.toks.next();
                try_next!(data, TokenKind::Punctuator(PunctuatorType::Colon));
                continue
            },

            _ => {
                goto_statement_end(data);
                continue;
            }
        };

        if let Some(name) = type_record.name {
            type_map.insert(name.clone(), type_record.type_);
            if data.visibility == VisibilityMode::Public {
                public_types.push(name.clone());
            }
            data.type_symbols.insert(name);
        }
    }

    Some((type_map, public_types, imports))
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    assert_token_matches!(data, TokenKind::Keyword(KeywordType::Typedef));
    
    let (name, type_) = parse_initializer(data)?;

    if name.is_none() {
        log_error!("PARSER ERROR: Invalid typedef declaration with no name!");
    }

    assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));

    Some(
        TypeRecord {
            name: Some(name?.to_string()),
            type_
        }
    )
}

pub(crate) fn parse_plain_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    match &data.toks.peek()?.kind {
        TokenKind::Keyword(KeywordType::Struct) => {
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
            
            try_next!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));

            Some(
                TypeRecord {
                    name: Some(name.to_string()),
                    type_: CXType::new(0, type_)
                }
            )
        },
        
        TokenKind::Keyword(KeywordType::Union) => {
            let type_ = parse_union(data)?;
            
            // parse_union returned some "union [identifier]", which is a placeholder
            // type that need to be processed by the type parser.
            // alternatively parse_union returned a nameless union declaration, which
            // is an effective no-op.
            let CXTypeKind::Union { name: Some(name), .. } = &type_ else {
                goto_statement_end(data);
                
                // this is janky but returning a None here indicates an error,
                // not that no type needs to be parsed.
                return Some(TypeRecord { name: None, type_: CXType::unit() });
            };
            
            try_next!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));

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
    assert_token_matches!(data, TokenKind::Keyword(KeywordType::Struct));

    let mut has_destructor = false;
    let name = parse_std_ident(data);
    
    if !try_next!(data, TokenKind::Punctuator(PunctuatorType::OpenBrace)) {
        return Some(name?.as_str().into());
    }
    
    let mut fields = Vec::new();

    while !try_next!(data, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
        if try_next!(data, TokenKind::Operator(OperatorType::Plus)) {
            assert_token_matches!(data, TokenKind::Keyword(KeywordType::Destructor));
            assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));
            has_destructor = true;
            continue;
        }
        
        let Some((name, _type)) = parse_initializer(data) else {
            point_log_error!(data, "PARSER ERROR: Failed to parse struct member type");
        };

        let Some(name) = name else {
            point_log_error!(data, "UNSUPPORTED: Nameless struct member of type {}", _type);
        };

        fields.push((name.data, _type));
        assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));
    }

    Some(CXTypeKind::Structured { name, fields, has_destructor })
}

pub(crate) fn parse_union(data: &mut ParserData) -> Option<CXTypeKind> {
    assert_token_matches!(data, TokenKind::Keyword(KeywordType::Union));

    let name = parse_std_ident(data);
    
    if !try_next!(data, TokenKind::Punctuator(PunctuatorType::OpenBrace)) {
        return Some(name?.as_str().into());
    }
    
    let mut fields = Vec::new();

    while !try_next!(data, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
        let (name, _type) = parse_initializer(data)?;

        let Some(name) = name else {
            point_log_error!(data, "UNSUPPORTED: Nameless union member of type {}", _type);
        };

        fields.push((name.data, _type));
        assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));
    }

    Some(
        CXTypeKind::Union {
            name,
            fields,
        }
    )
}

pub(crate) fn parse_specifier(data: &mut ParserData) -> CXTypeSpecifier {
    let mut spec_acc = 0;

    while let Some(TokenKind::Specifier(spec)) = next_kind!(data) {
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

    match &next_tok.kind {
        TokenKind::Keyword(KeywordType::Strong) => {
            data.toks.next();
            
            let is_array = match data.toks.next()?.kind {
                TokenKind::Operator(OperatorType::Asterisk) => false,
                TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
                    assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseBracket));
                    true
                },
                
                _ => log_error!("PARSER ERROR: Expected '*' or '[]' after 'strong' keyword")
            };
            
            let specs = parse_specifier(data);
            let acc_type = CXType::new(
                specs,
                CXTypeKind::StrongPointer { 
                    inner: Box::new(acc_type),
                    is_array
                }
            );

            parse_typemods(data, acc_type)
        },
        
        TokenKind::Keyword(KeywordType::Weak) => {
            data.toks.next();
            assert_token_matches!(data, TokenKind::Operator(OperatorType::Asterisk));
            
            let specs = parse_specifier(data);
            let acc_type = CXType::new(
                specs,
                CXTypeKind::PointerTo { 
                    inner: Box::new(acc_type),
                    explicitly_weak: true
                }
            );

            parse_typemods(data, acc_type)
        },
        
        TokenKind::Operator(OperatorType::Asterisk) => {
            data.toks.next();
            let specs = parse_specifier(data);
            let acc_type = acc_type.pointer_to().add_specifier(specs);

            parse_typemods(data, acc_type)
        },

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            data.toks.next();
            assert_token_matches!(data, TokenKind::Operator(OperatorType::Asterisk));
            let name = parse_std_ident(data);
            assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let ParseParamsResult { params, var_args } = parse_params(data)?;

            let prototype = CXFunctionPrototype {
                name: CXIdent::from("INTERNAL_FUNCTION_PTR_TYPE"),
                return_type: acc_type,
                params,
                var_args
            };

            Some((
                name,
                CXTypeKind::Function { prototype: Box::new(prototype) }.to_val_type().pointer_to()
            ))
        },

        TokenKind::Identifier(_) => Some((Some(parse_std_ident(data)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

pub(crate) fn parse_suffix_typemod(data: &mut ParserData, acc_type: CXType) -> Option<CXType> {
    let Some(next_tok) = data.toks.peek() else {
        return Some(acc_type);
    };

    match &next_tok.kind {
        TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
            data.toks.next();

            let _type = match data.toks.peek()?.kind {
                TokenKind::Punctuator(PunctuatorType::CloseBracket) => {
                    acc_type.pointer_to()
                },
                TokenKind::IntLiteral(size) => {
                    data.toks.next();
                    CXTypeKind::Array { _type: Box::new(acc_type), size: size as usize }.to_val_type()
                },
                _ => {
                    let size = parse_expr(data)?;
                    
                    CXTypeKind::VariableLengthArray { 
                        _type: Box::new(acc_type),
                        size: Box::new(size)
                    }.to_val_type()
                }
            };
            
            assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseBracket));

            Some(_type)
        },

        _ => Some(acc_type),
    }
}

pub(crate) fn parse_type_base(data: &mut ParserData) -> Option<CXType> {
    let _type = match data.toks.peek()?.kind {
        TokenKind::Identifier(_) => Some(
            CXTypeKind::Identifier {
                name: parse_std_ident(data)?,
                predeclaration: PredeclarationType::None
            }.to_val_type()
        ),
        TokenKind::Intrinsic(_) => Some(
            CXTypeKind::Identifier {
                name: parse_intrinsic(data)?,
                predeclaration: PredeclarationType::None
            }.to_val_type()
        ),

        TokenKind::Keyword(KeywordType::Struct) => Some(
            parse_struct(data)?.to_val_type()
        ),
        
        TokenKind::Keyword(KeywordType::Union) => Some(
            parse_union(data)?.to_val_type()
        ),

        _ => return None
    };

    let specifiers = parse_specifier(data);

    Some(_type?.add_specifier(specifiers))
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