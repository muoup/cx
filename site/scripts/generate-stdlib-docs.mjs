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
        : execFileSync("cargo", ["run", "--quiet", "--release", "-p", "chloro", "--", "doc", libraryDirectory], options);
    return JSON.parse(output);
}

function moduleSlug(moduleName) {
    return moduleName.replace(/^std::/, "").replaceAll("::", "/");
}

function pageFile(entry) {
    if (entry.children.length > 0) {
        return path.posix.join(entry.slug, "index.md");
    }
    const {base, dir} = path.posix.parse(entry.slug);
    const landingNames = ["index", "readme", path.posix.basename(dir || "stdlib").toLowerCase()];
    return landingNames.includes(base.toLowerCase()) ? `${entry.slug}-module.md` : `${entry.slug}.md`;
}

function pageLink(from, to) {
    const relative = path.posix.relative(path.posix.dirname(from), to);
    return relative.startsWith(".") ? relative : `./${relative}`;
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
function reference(qualified, {file, id}, from) {
    const parts = qualified.split("::");
    const name = parts.pop();
    const href = `${pageLink(from, file)}#${id}`;
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
        return [];
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
    return blocks;
}

// An item's declaration heads it; its docs sit indented below.
function renderItem(item, from) {
    const heading = item.kind === "type"
        ? `### <code>${span("chl-stdlib-name", qualifiedName(item.name))}</code> \\{#${itemId(item)}}`
        : `### <code>${renderSignature(item.declaration, item.name, from)}</code> \\{#${itemId(item)}}`;
    const body = renderDoc(item.doc);
    if (item.kind === "type") {
        body.unshift(['<div className="chl-stdlib-declaration">', "", "~~~chl", item.declaration, "~~~", "", "</div>"].join("\n"));
    }
    return body.length > 0 ? [heading, '<div className="chl-stdlib-item">', ...body, "</div>"].join("\n\n") : heading;
}

const sections = [
    ["type", "Types"],
    ["function", "Functions"],
    ["global", "Globals"],
];

function renderDeclarations(module, from) {
    return sections.flatMap(([kind, title]) => {
        const items = module.items.filter((item) => item.kind === kind);
        return items.length > 0 ? [`## ${title}\n\n${items.map((item) => renderItem(item, from)).join("\n\n")}`] : [];
    });
}

function renderChildren(entry, from) {
    if (entry.children.length === 0) {
        return [];
    }
    const list = entry.children.map((child) => {
        const link = `[${inlineCode(child.name)}](${pageLink(from, pageFile(child))})`;
        const summary = child.module?.doc?.summary;
        const description = summary ? mdxMarkdown(summary) : entry.slug === "" ? undocumented : "";
        return description ? `- ${link} — ${description}` : `- ${link}`;
    });
    return ["## Modules", '<div className="chl-stdlib-modules">', list.join("\n"), "</div>"];
}

function renderPage(entry, position, file) {
    const {module} = entry;
    const frontMatter = [
        "---",
        `title: ${yamlString(entry.name)}`,
        `sidebar_position: ${position}`,
        "pagination_prev: null",
        "pagination_next: null",
    ];
    const intro = [];

    if (entry.children.length === 0 && file !== `${entry.slug}.md`) {
        frontMatter.push(`slug: ${yamlString(`/stdlib/${entry.slug}`)}`);
    }

    if (entry.slug === "") {
        frontMatter.push(
            "sidebar_custom_props: {stdlibSearch: true}",
            "description: Reference for the Chloro standard library, generated from its doc comments.",
        );
        intro.push("This reference is generated by `chl doc` from the public declarations and doc comments in `lib/std`.");
    } else if (module) {
        frontMatter.push(
            `sidebar_custom_props: ${JSON.stringify({sourcePath: `lib/std/${module.source}`})}`,
            `description: ${yamlString(module.doc?.summary || "Standard-library module.")}`,
        );
        intro.push('<div className="chl-stdlib-meta">', `Source: ${sourceLink(module)}`, "</div>", ...renderDoc(module.doc));
    }
    frontMatter.push("---");

    return [
        frontMatter.join("\n"),
        '<div className="chl-stdlib-page">',
        `# ${entry.name}`,
        ...intro,
        ...renderChildren(entry, file),
        ...(module ? renderDeclarations(module, file) : []),
        "</div>",
        "",
    ].join("\n\n");
}

function buildEntries(modules) {
    const root = {slug: "", name: "Standard Library", children: []};
    const entries = new Map([["", root]]);
    for (const module of modules) {
        let entry = root;
        for (const segment of moduleSlug(module.module).split("/")) {
            const slug = path.posix.join(entry.slug, segment);
            let child = entries.get(slug);
            if (!child) {
                child = {slug, name: `std::${slug.replaceAll("/", "::")}`, children: []};
                entries.set(slug, child);
                entry.children.push(child);
            }
            entry = child;
        }
        entry.name = module.module;
        entry.module = module;
    }
    return entries;
}

const {modules} = readDocumentation();

if (modules.length === 0) {
    throw new Error(`chl doc found no modules in ${libraryDirectory}`);
}

const entries = buildEntries(modules);
const typeIndex = new Map(
    [...entries.values()].flatMap((entry) =>
        (entry.module?.items ?? [])
            .filter((item) => item.kind === "type")
            .map((item) => [`${entry.name}::${item.name}`, {file: pageFile(entry), id: itemId(item)}]),
    ),
);

fs.rmSync(outputDirectory, {recursive: true, force: true});
[...entries.values()].forEach((entry, index) => {
    const file = pageFile(entry);
    const outputPath = path.join(outputDirectory, file);
    fs.mkdirSync(path.dirname(outputPath), {recursive: true});
    fs.writeFileSync(outputPath, renderPage(entry, index + 1, file));
});

console.log(`Generated ${entries.size} standard-library pages.`);
