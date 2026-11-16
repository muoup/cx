import {execFileSync} from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import {fileURLToPath} from "node:url";

import {tokenizeChl} from "../src/lib/chl-syntax.mjs";

const siteDirectory = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const repositoryDirectory = path.resolve(siteDirectory, "..");
const libraryDirectory = path.join(repositoryDirectory, "lib", "std");
const outputDirectory = path.join(siteDirectory, "docs", "stdlib");
const sourceBase = "https://github.com/muoup/cx/blob/main/lib/std";
const undocumented = "*No documentation provided.*";

// CHL_BIN names a built compiler; otherwise cargo builds one from this checkout.
function readDocumentation() {
    const options = {cwd: repositoryDirectory, encoding: "utf8", maxBuffer: 64 * 1024 * 1024, stdio: ["ignore", "pipe", "inherit"]};
    const output = process.env.CHL_BIN
        ? execFileSync(process.env.CHL_BIN, ["doc", libraryDirectory], options)
        : execFileSync("cargo", ["run", "--quiet", "--release", "-p", "chl", "--", "doc", libraryDirectory], options);
    return JSON.parse(output);
}

function moduleSlug(moduleName) {
    return moduleName.replace(/^std::/, "").replaceAll("::", "/");
}

function sourceLink(module, line) {
    const label = inlineCode(`lib/std/${module.source}`);
    return `[${label}](${sourceBase}/${module.source}${line ? `#L${line}` : ""})`;
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

// Doc comments are markdown, but MDX would read their braces and angle brackets as code; code keeps them as written.
function mdxMarkdown(markdown) {
    return markdown
        .split(/(^(?:```|~~~)[^\n]*\n[\s\S]*?^(?:```|~~~)[ \t]*$)/m)
        .map((part, index) =>
            index % 2 === 1
                ? part
                : part
                    .split(/(`[^`\n]*`)/)
                    .map((text, inner) => (inner % 2 === 1 ? text : text.replace(/[{}]/g, "\\$&").replaceAll("<", "&lt;")))
                    .join(""),
        )
        .join("");
}

// Highlights code[start, end), tokenizing all of code so the slice keeps its context.
function tokens(code, start, end) {
    let offset = 0;
    return tokenizeChl(code)
        .map(({text, kind}) => {
            const part = text.slice(Math.max(start - offset, 0), Math.max(end - offset, 0));
            offset += text.length;
            return part && kind ? `<span className="chl-token-${kind}">${mdxText(part)}</span>` : mdxText(part);
        })
        .join("");
}

// A documented type links to its docs.
function reference(qualified, {slug, id}, from) {
    const parts = qualified.split("::");
    const name = parts.pop();
    const target = `${path.posix.relative(path.posix.dirname(from), slug)}.md#${id}`;
    const href = target.startsWith(".") ? target : `./${target}`;
    return `[<span className="chl-stdlib-ref">${parts.map(mdxText).join("::")}::<span className="chl-token-type">${mdxText(name)}</span></span>](${href})`;
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
    return name.split(/(?<=::)/).map((part) => `<span className="chl-stdlib-segment">${mdxText(part)}</span>`).join("");
}

function itemId(item) {
    return item.name.replaceAll("::", "-").toLowerCase();
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
function renderSignature(signature, name, from) {
    const at = signature.indexOf(name);
    if (at < 0) {
        return span("chl-stdlib-name", qualifiedName(name));
    }

    const end = at + name.length;
    const open = signature.indexOf("(", end);
    const close = signature.lastIndexOf(")");
    const parameters = open < 0 || close < open || signature.length <= signatureWidth ? [] : parameterRanges(signature, open, close);
    const rest = parameters.length === 0
        ? highlighted(signature, end, signature.length, from)
        : [
            highlighted(signature, end, open + 1, from),
            ...parameters.map(([start, stop], index) =>
                span("chl-stdlib-parameter", highlighted(signature, start, stop, from) + (index < parameters.length - 1 ? "," : "")),
            ),
            highlighted(signature, close, signature.length, from),
        ].join("");

    return [
        span("chl-stdlib-signature", highlighted(signature, 0, at, from)),
        span("chl-stdlib-name", qualifiedName(name)),
        span("chl-stdlib-signature", rest),
    ].join("");
}

function renderDoc(doc) {
    if (!doc) {
        return [undocumented];
    }
    const blocks = [doc.summary, doc.body].filter(Boolean).map(mdxMarkdown);
    if (doc.parameters.length > 0) {
        const rows = doc.parameters.map(({name, description}) => `| ${inlineCode(name)} | ${mdxMarkdown(description).replaceAll("|", "\\|")} |`);
        blocks.push(["#### Parameters", "", "| Name | Description |", "| --- | --- |", ...rows].join("\n"));
    }
    if (doc.returns) {
        blocks.push(`#### Returns\n\n${mdxMarkdown(doc.returns)}`);
    }
    if (doc.examples) {
        blocks.push(`#### Examples\n\n${mdxMarkdown(doc.examples)}`);
    }
    return blocks.length > 0 ? blocks : [undocumented];
}

// An item's declaration heads it; its docs sit indented below.
function renderItem(item, from) {
    const heading = item.kind === "type"
        ? `### <code>${span("chl-stdlib-name", qualifiedName(item.name))}</code> \\{#${itemId(item)}}`
        : `### <code>${renderSignature(item.declaration, item.name, from)}</code> \\{#${itemId(item)}}`;
    const declaration = item.kind === "type" ? [["~~~chl", item.declaration, "~~~"].join("\n")] : [];
    return [heading, "", `<div className="chl-stdlib-item">`, "", ...[...declaration, ...renderDoc(item.doc)].flatMap((block) => [block, ""]), "</div>"].join("\n");
}

const sections = [
    ["type", "Types"],
    ["function", "Functions"],
    ["global", "Globals"],
];

function renderModule(module, position) {
    const from = moduleSlug(module.module);
    const body = sections.flatMap(([kind, title]) => {
        const items = module.items.filter((item) => item.kind === kind);
        return items.length > 0 ? [`## ${title}\n\n${items.map((item) => renderItem(item, from)).join("\n\n")}`] : [];
    });

    return [
        "---",
        `title: ${yamlString(module.module)}`,
        `sidebar_position: ${position}`,
        `description: ${yamlString(module.doc?.summary || "Standard-library module.")}`,
        "---",
        "",
        `<div className="chl-stdlib-page">`,
        "",
        `# ${module.module}`,
        "",
        `<div className="chl-stdlib-meta">`,
        "",
        `Source: ${sourceLink(module)}`,
        "",
        "</div>",
        "",
        ...renderDoc(module.doc).flatMap((block) => [block, ""]),
        body.join("\n\n"),
        "",
        "</div>",
        "",
    ].join("\n");
}

function renderIndex(modules) {
    const list = modules
        .map((module) => `- [${module.module}](./${moduleSlug(module.module)}.md) — ${module.doc ? mdxMarkdown(module.doc.summary) : undocumented}`)
        .join("\n");

    return [
        "---",
        "title: Standard Library",
        "sidebar_position: 1",
        "description: Reference for the Chloro standard library, generated from its doc comments.",
        "---",
        "",
        "<div className=\"chl-stdlib-page\">",
        "",
        "# Standard Library",
        "",
        "This reference is generated by `chl doc` from the public declarations and doc comments in `lib/std`.",
        "",
        "## Modules",
        "",
        list,
        "",
        "</div>",
        "",
    ].join("\n");
}

function writeModule(module, position) {
    const slug = moduleSlug(module.module);
    const outputPath = path.join(outputDirectory, `${slug}.md`);
    fs.mkdirSync(path.dirname(outputPath), {recursive: true});
    fs.writeFileSync(outputPath, renderModule(module, position));

    const group = path.dirname(slug);
    if (group !== ".") {
        const label = `std::${group.replaceAll("/", "::")}`;
        fs.writeFileSync(path.join(outputDirectory, group, "_category_.json"), JSON.stringify({label}));
    }
}

const {modules} = readDocumentation();

if (modules.length === 0) {
    throw new Error(`chl doc found no modules in ${libraryDirectory}`);
}

// Documented types by fully qualified name, so signatures can link to them.
const typeIndex = new Map(
    modules.flatMap((module) =>
        module.items
            .filter((item) => item.kind === "type")
            .map((item) => [`${module.module}::${item.name}`, {slug: moduleSlug(module.module), id: itemId(item)}]),
    ),
);

fs.rmSync(outputDirectory, {recursive: true, force: true});
fs.mkdirSync(outputDirectory, {recursive: true});
fs.writeFileSync(path.join(outputDirectory, "index.md"), renderIndex(modules));

modules.forEach((module, index) => writeModule(module, index + 2));

console.log(`Generated ${modules.length} standard-library module pages.`);
