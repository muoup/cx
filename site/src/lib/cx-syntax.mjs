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
    "if",
    "import",
    "match",
    "move",
    "public",
    "return",
    "safe",
    "static",
    "struct",
    "switch",
    "union",
    "unsafe",
    "while",
];

const cxKeywords = new Set(cxKeywordWords);

const cxTypeWords = [
    "bool",
    "int",
    "char",
    "f32",
    "f64",
    "fd",
    "file",
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

const cxTypes = new Set(cxTypeWords);

const cxConstants = new Set(["AF_INET", "SOCK_STREAM"]);

function regexAlternatives(values) {
    return values
        .map((value) => value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
        .join("|");
}

const cxTokenPattern = new RegExp(
    [
        String.raw`//[^\r\n]*`,
        String.raw`/\*[\s\S]*?\*/`,
        String.raw`"(?:\\.|[^"\\])*"`,
        String.raw`'(?:\\.|[^'\\])*'`,
        String.raw`@[A-Za-z_][A-Za-z0-9_]*`,
        String.raw`\b(?:${regexAlternatives(cxKeywordWords)})\b`,
        String.raw`\b(?:${regexAlternatives(cxTypeWords)})\b`,
        String.raw`\b(?:std::[A-Za-z_][A-Za-z0-9_:]*|AF_INET|SOCK_STREAM)\b`,
        String.raw`\b(?:0[xX][0-9a-fA-F]+|\d+(?:\.\d+)?)\b`,
        String.raw`\b[A-Z][A-Za-z0-9_]*\b`,
        String.raw`[A-Za-z_][A-Za-z0-9_]*(?=\s*(?:<[^>\r\n]*>)?\s*\()`,
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

    if (token.startsWith("@") || cxKeywords.has(token)) {
        return "keyword";
    }

    if (cxTypes.has(token) || /^[A-Z][A-Za-z0-9_]*$/.test(token)) {
        return "type";
    }

    if (token.startsWith("std::") || cxConstants.has(token)) {
        return "constant";
    }

    return "function";
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
