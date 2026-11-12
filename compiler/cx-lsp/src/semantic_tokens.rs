use crate::index::{FileIndex, SymbolClass};
use crate::position::SourceText;
use crate::project::ProjectSettings;
use cx_lexer::lex_with_context;
use cx_tokens::token::TokenKind;
use std::collections::BTreeMap;
use std::path::Path;
use tower_lsp::lsp_types::{Position, Range, SemanticToken, SemanticTokenType};

pub const LEGEND: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::PARAMETER,
];

const KEYWORD: u32 = 0;
const STRING: u32 = 1;
const NUMBER: u32 = 2;
const TYPE: u32 = 3;
const VARIABLE: u32 = 4;
const FUNCTION: u32 = 5;
const PARAMETER: u32 = 6;

fn class_token(class: SymbolClass) -> u32 {
    match class {
        SymbolClass::Function => FUNCTION,
        SymbolClass::Type => TYPE,
        SymbolClass::Global | SymbolClass::Local => VARIABLE,
        SymbolClass::Parameter => PARAMETER,
    }
}

pub fn semantic_tokens(
    text: &str,
    path: &Path,
    project: &ProjectSettings,
    index: Option<&FileIndex>,
) -> Vec<SemanticToken> {
    let references = index.map_or(&[][..], |index| &index.references);
    let lexed = lex_with_context(text, path, &project.include_dirs, &project.predefined_macros);

    let tokens = match lexed {
        Ok(lexed) => {
            let source = SourceText::new(text.to_string());
            let classes = references
                .iter()
                .map(|reference| (reference.range.start, class_token(reference.class)))
                .collect::<BTreeMap<Position, u32>>();

            lexed
                .iter()
                .filter(|token| *token.file_origin == *path)
                .filter_map(|token| {
                    let range = source.range(token.byte_start_index, token.byte_end_index);
                    let token_type = match token.kind {
                        TokenKind::Keyword(_)
                        | TokenKind::Intrinsic(_)
                        | TokenKind::Attribute(_)
                        | TokenKind::CompilerIdentifier(_) => KEYWORD,
                        TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) => NUMBER,
                        TokenKind::StringLiteral(_) => STRING,
                        TokenKind::Identifier(_) => *classes.get(&range.start)?,
                        _ => return None,
                    };
                    Some((range, token_type))
                })
                .collect::<Vec<_>>()
        }
        Err(_) => references
            .iter()
            .map(|reference| (reference.range, class_token(reference.class)))
            .collect(),
    };

    encode(tokens)
}

fn encode(mut tokens: Vec<(Range, u32)>) -> Vec<SemanticToken> {
    tokens.sort_by_key(|(range, _)| range.start);

    let mut encoded = Vec::with_capacity(tokens.len());
    let mut previous = Position::default();
    let mut covered = Position::default();

    for (range, token_type) in tokens {
        if range.start.line != range.end.line || range.start < covered {
            continue;
        }

        let delta_line = range.start.line - previous.line;
        let delta_start = match delta_line {
            0 => range.start.character - previous.character,
            _ => range.start.character,
        };
        encoded.push(SemanticToken {
            delta_line,
            delta_start,
            length: (range.end.character - range.start.character).max(1),
            token_type,
            token_modifiers_bitset: 0,
        });

        previous = range.start;
        covered = range.end.max(Position {
            line: range.start.line,
            character: range.start.character + 1,
        });
    }

    encoded
}
