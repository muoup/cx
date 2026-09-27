import fs from "node:fs";
import path from "node:path";
import {fileURLToPath} from "node:url";

import {tokenizeCx} from "../src/lib/cx-syntax.mjs";

const siteDirectory = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const dataDirectory = path.join(siteDirectory, "stdlib");
const outputDirectory = path.join(siteDirectory, "docs", "stdlib");

function readModuleRecords(directory) {
    const records = [];

    for (const entry of fs.readdirSync(directory, {withFileTypes: true})) {
        const entryPath = path.join(directory, entry.name);

        if (entry.isDirectory()) {
            records.push(...readModuleRecords(entryPath));
            continue;
        }

        if (!entry.isFile() || !entry.name.endsWith(".json")) {
            continue;
        }

        const record = JSON.parse(fs.readFileSync(entryPath, "utf8"));
        if (record.module && record.source) {
            records.push({...record, dataPath: entryPath});
        }
    }

    return records;
}

function moduleSlug(moduleName) {
    return moduleName.replace(/^std::/, "").replaceAll("::", "/");
}

function sourcePath(record) {
    return record.source ?? "lib/std/<module>.cx";
}

function sourceLink(record) {
    return `[${inlineCode(sourcePath(record))}](https://github.com/muoup/cx/blob/main/${sourcePath(record)})`;
}

function yamlString(value) {
    return JSON.stringify(String(value ?? ""));
}

function inlineCode(value) {
    return `\`${String(value ?? "").replaceAll("`", "\\`")}\``;
}

function htmlText(value) {
    return String(value ?? "")
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll('"', "&quot;")
        .replaceAll("'", "&#39;");
}

// Text inside MDX markup is still parsed as markdown and expressions, so those characters become references too.
function mdxText(value) {
    return htmlText(value).replace(/[{}*_`[\]\\]/g, (character) => `&#${character.charCodeAt(0)};`);
}

// Highlights code[start, end), tokenizing all of code so the slice keeps its context.
function tokens(code, start, end) {
    let offset = 0;
    return tokenizeCx(code)
        .map(({text, kind}) => {
            const part = text.slice(Math.max(start - offset, 0), Math.max(end - offset, 0));
            offset += text.length;
            return part && kind ? `<span className="cx-token-${kind}">${mdxText(part)}</span>` : mdxText(part);
        })
        .join("");
}

// A documented type links to its docs.
function reference(qualified, {slug, id}, from) {
    const parts = qualified.split("::");
    const name = parts.pop();
    const target = `${path.posix.relative(path.posix.dirname(from), slug)}.md#${id}`;
    const href = target.startsWith(".") ? target : `./${target}`;
    return `[<span className="cx-stdlib-ref">${parts.map(mdxText).join("::")}::<span className="cx-token-type">${mdxText(name)}</span></span>](${href})`;
}

function highlighted(code, start, end, from) {
    const pieces = [];
    let cursor = start;
    for (const match of code.slice(start, end).matchAll(/\bstd(?:::\w+)+/g)) {
        const target = typeIndex.get(match[0]);
        if (target) {
            const at = start + match.index;
            pieces.push(tokens(code, cursor, at), reference(match[0], target, from));
            cursor = at + match[0].length;
        }
    }
    return pieces.join("") + tokens(code, cursor, end);
}

function span(className, content) {
    return `<span className="${className}">${content}</span>`;
}

// A qualified name may break after its ::, never inside a word; lines can wrap between inline blocks.
function qualifiedName(name) {
    return name.split(/(?<=::)/).map((part) => `<span className="cx-stdlib-segment">${mdxText(part)}</span>`).join("");
}

function fieldDeclaration({name, type}) {
    const array = type.match(/^(.*?)(\[\d*\])$/);
    return array ? `${array[1]} ${name}${array[2]}` : `${type} ${name}`;
}

// The name of a named type, which is also its heading id.
function typeId(type) {
    const words = type.name.replace(/<.*$/, "").trim().split(/\s+/);
    return words.length > 1 ? words.at(-1) : undefined;
}

// A type's heading is its declaration, with each member's description as a trailing comment.
function renderType(type, from) {
    const id = typeId(type);
    const attributes = type.attributes?.length ? ` : ${type.attributes.join(", ")}` : "";
    const variants = type.variants ?? [];
    const members = [
        ...(type.fields ?? []).map((field) => [`${fieldDeclaration(field)};`, field.description]),
        ...variants.map((variant, index) => [
            `${variant.name}${variant.payloadType ? ` :: ${variant.payloadType}` : ""}${index < variants.length - 1 ? "," : ""}`,
            variant.description,
        ]),
    ];
    const width = Math.max(0, ...members.map(([code]) => code.length)) + 2;
    const body = members.map(([code, description]) =>
        span("cx-stdlib-member", [
            `<span className="cx-stdlib-member-code" style={{width: "${width}ch"}}>${highlighted(code, 0, code.length, from)}</span>`,
            description ? span("cx-token-comment", `// ${mdxText(description)}`) : "",
        ].join("")),
    );
    const heading = renderSignature(`${type.name}${attributes} {`, id ?? type.name, from, false) + span("cx-stdlib-signature", `${body.join("")}};`);

    return renderItem(`### <code>${heading}</code>${id ? ` \\{#${id}}` : ""}`, [type.description]);
}

// An item's description and examples sit indented below its heading.
function renderItem(heading, body) {
    return [heading, "", `<div className="cx-stdlib-item">`, "", ...body.flatMap((block) => [block, ""]), "</div>"].join("\n");
}

// Longer signatures put one parameter per line, as rustdoc does.
const signatureWidth = 64;

// The [start, end) ranges of the parameters between the parentheses at open and close, split at top-level commas.
function parameterRanges(signature, open, close) {
    const ranges = [];
    let depth = 0;
    let start = open + 1;
    for (let index = start; index <= close; index++) {
        const character = signature[index];
        if ("(<".includes(character)) {
            depth++;
        } else if (")>".includes(character) && index < close) {
            depth--;
        } else if ((character === "," && depth === 0) || index === close) {
            const text = signature.slice(start, index);
            const leading = text.length - text.trimStart().length;
            if (text.trim()) {
                ranges.push([start + leading, index]);
            }
            start = index + 1;
        }
    }
    return ranges;
}

// The signature is the heading, with the name picked out; the contents list shows only the name.
function renderSignature(signature, name, from, split = true) {
    const at = signature.indexOf(name);
    if (at < 0) {
        return span("cx-stdlib-name", qualifiedName(name));
    }

    const end = at + name.length;
    const open = signature.indexOf("(", end);
    const close = signature.lastIndexOf(")");
    const parameters = !split || open < 0 || signature.length <= signatureWidth ? [] : parameterRanges(signature, open, close);
    const rest = parameters.length === 0
        ? highlighted(signature, end, signature.length, from)
        : [
            highlighted(signature, end, open + 1, from),
            ...parameters.map(([start, stop], index) =>
                span("cx-stdlib-parameter", highlighted(signature, start, stop, from) + (index < parameters.length - 1 ? "," : "")),
            ),
            highlighted(signature, close, signature.length, from),
        ].join("");

    return [
        span("cx-stdlib-signature", highlighted(signature, 0, at, from)),
        span("cx-stdlib-name", qualifiedName(name)),
        span("cx-stdlib-signature", rest),
    ].join("");
}

function renderFunction(functionRecord, from) {
    const name = `${functionRecord.owner ? `${functionRecord.owner}::` : ""}${functionRecord.name}`;
    const heading = renderSignature(functionRecord.signature, name, from);
    const examples = (functionRecord.examples ?? []).map((example, index) =>
        [`#### ${example.title ?? `Example ${index + 1}`}`, "", `~~~${example.language ?? "cx"}`, example.code, "~~~"].join("\n"),
    );

    return renderItem(`### <code>${heading}</code> \\{#${name.replaceAll("::", "-").toLowerCase()}}`, [functionRecord.description, ...examples]);
}

function renderModule(record) {
    const from = moduleSlug(record.module);
    const types = (record.types ?? []).map((type) => renderType(type, from)).join("\n\n");
    const functions = (record.functions ?? []).map((functionRecord) => renderFunction(functionRecord, from)).join("\n\n");
    const sections = [];

    if (types) {
        sections.push(`## Types\n\n${types}`);
    }

    if (functions) {
        sections.push(`## Functions\n\n${functions}`);
    }

    return [
        "---",
        `title: ${yamlString(record.module)}`,
        `sidebar_position: ${record.order ?? 100}`,
        `description: ${yamlString(record.summary ?? "Standard-library module.")}`,
        "---",
        "",
        `<div className="cx-stdlib-page">`,
        "",
        `# ${record.module}`,
        "",
        `<div className="cx-stdlib-meta">`,
        "",
        `Source: ${sourceLink(record)}`,
        "",
        "</div>",
        "",
        sections.join("\n\n"),
        "",
        "</div>",
        "",
    ].join("\n");
}

function renderIndex(records) {
    const modules = records
        .map((record) => {
            const slug = moduleSlug(record.module);
            return `- [${record.module}](./${slug}.md) — ${record.summary ?? "Standard-library module."}`;
        })
        .join("\n");

    return [
        "---",
        "title: Standard Library",
        "sidebar_position: 1",
        "description: Structured reference for the CX standard library.",
        "---",
        "",
        "<div className=\"cx-stdlib-page\">",
        "",
        "# Standard Library",
        "",
        "This reference is generated from the structured records in `site/stdlib`. Descriptions are intentionally concise placeholders while the API documentation is being filled in.",
        "",
        "## Modules",
        "",
        modules,
        "",
        "</div>",
        "",
    ].join("\n");
}

function writeRecord(record) {
    const slug = moduleSlug(record.module);
    const outputPath = path.join(outputDirectory, `${slug}.md`);
    fs.mkdirSync(path.dirname(outputPath), {recursive: true});
    fs.writeFileSync(outputPath, renderModule(record));

    const group = path.dirname(slug);
    if (group !== ".") {
        const label = `std::${group.replaceAll("/", "::")}`;
        fs.writeFileSync(path.join(outputDirectory, group, "_category_.json"), JSON.stringify({label}));
    }
}

const records = readModuleRecords(dataDirectory).sort((left, right) => {
    const leftOrder = left.order ?? 100;
    const rightOrder = right.order ?? 100;
    return leftOrder - rightOrder || left.module.localeCompare(right.module);
});

if (records.length === 0) {
    throw new Error(`No standard-library records found in ${dataDirectory}`);
}

// Documented types by fully qualified name, so signatures can link to them.
const typeIndex = new Map(
    records.flatMap((record) =>
        (record.types ?? []).flatMap((type) => {
            const id = typeId(type);
            return id ? [[`${record.module}::${id}`, {slug: moduleSlug(record.module), id}]] : [];
        }),
    ),
);

fs.rmSync(outputDirectory, {recursive: true, force: true});
fs.mkdirSync(outputDirectory, {recursive: true});
fs.writeFileSync(path.join(outputDirectory, "index.md"), renderIndex(records));

for (const record of records) {
    writeRecord(record);
}

console.log(`Generated ${records.length} standard-library module pages.`);
