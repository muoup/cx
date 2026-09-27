const cxKeywordWords = [
    "as",
    "break",
    "case",
    "comptime",
    "const",
    "continue",
    "default",
    "defer",
    "do",
    "else",
    "enum",
    "expr",
    "extern",
    "for",
    "goto",
    "if",
    "import",
    "is",
    "match",
    "move",
    "public",
    "return",
    "safe",
    "sizeof",
    "static",
    "struct",
    "switch",
    "then",
    "typedef",
    "union",
    "unsafe",
    "where",
    "while",
    "yield",
];

const cxKeywords = new Set(cxKeywordWords);

const cxTypeWords = [
    "bool",
    "int",
    "char",
    "f32",
    "f64",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "isize",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "unreachable",
    "usize",
    "void",
];

const cxConstantWords = ["false", "true", "NULL"];

function regexAlternatives(values) {
    return values
        .map((value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
        .join("|");
}

const identifier = String.raw`[A-Za-z_][A-Za-z0-9_]*`;
const keywordPattern = String.raw`\b(?:${regexAlternatives(cxKeywordWords)})\b`;

const cxTokenPattern = new RegExp(
    [
        String.raw`//[^\r\n]*`,
        String.raw`/\*[\s\S]*?\*/`,
        String.raw`"(?:\\.|[^"\\])*"`,
        String.raw`'(?:\\.|[^'\\])*'`,
        String.raw`@${identifier}`,
        String.raw`\|>|<\|`,
        keywordPattern,
        String.raw`\b(?:${regexAlternatives(cxTypeWords)})\b`,
        String.raw`\b(?:${regexAlternatives(cxConstantWords)})\b`,
        String.raw`\b(?:0[xX][0-9a-fA-F]+|\d+(?:\.\d+)?)\b`,
        String.raw`\b[A-Z][A-Z0-9_]+\b`,
        String.raw`\b[A-Z][A-Za-z0-9_]*\b`,
        // The name after struct/enum/union, or one followed by a declared name: `tcp_stream client`, `span<u8> buf`, `char* data`.
        String.raw`(?<=\b(?:struct|enum|union)\s+)${identifier}`,
        String.raw`\b${identifier}(?=(?:<[^;(){}=\r\n]*>)?[*&]*[ \t]+(?!${keywordPattern})[A-Za-z_])`,
    ].join("|"),
    "g",
);

function cxTokenKind(token) {
    if (token.startsWith("//") || token.startsWith("/*")) {
        return "comment";
    }

    if (token.startsWith('"') || token.startsWith("'")) {
        return "string";
    }

    if (/^(?:0[xX][0-9a-fA-F]+|\d+(?:\.\d+)?)$/.test(token)) {
        return "number";
    }

    if (token === "|>" || token === "<|") {
        return "operator";
    }

    if (token.startsWith("@") || cxKeywords.has(token)) {
        return "keyword";
    }

    if (cxConstantWords.includes(token) || /^[A-Z][A-Z0-9_]+$/.test(token)) {
        return "constant";
    }

    return "type";
}

export function tokenizeCx(source) {
    const text = String(source ?? "");
    const tokens = [];
    let cursor = 0;

    for (const match of text.matchAll(cxTokenPattern)) {
        const token = match[0];
        const index = match.index ?? 0;

        if (index > cursor) {
            tokens.push({text: text.slice(cursor, index), kind: null});
        }

        tokens.push({text: token, kind: cxTokenKind(token)});
        cursor = index + token.length;
    }

    if (cursor < text.length) {
        tokens.push({text: text.slice(cursor), kind: null});
    }

    return tokens;
}
