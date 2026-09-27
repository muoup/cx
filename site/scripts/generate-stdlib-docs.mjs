import fs from "node:fs";
import path from "node:path";
import {fileURLToPath} from "node:url";

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

// A qualified name may break after its ::, never inside a word; lines can wrap between inline blocks.
function qualifiedName(name) {
    return name.split(/(?<=::)/).map((part) => `<span className="cx-stdlib-segment">${mdxText(part)}</span>`).join("");
}

function fieldDeclaration({name, type}) {
    const array = type.match(/^(.*?)(\[\d*\])$/);
    return array ? `${array[1]} ${name}${array[2]}` : `${type} ${name}`;
}

// Types are shown as their CX declaration, with each member's description as a trailing comment.
function renderDeclaration(type) {
    const attributes = type.attributes?.length ? ` : ${type.attributes.join(", ")}` : "";
    const variants = type.variants ?? [];
    const members = [
        ...(type.fields ?? []).map((field) => [`${fieldDeclaration(field)};`, field.description]),
        ...variants.map((variant, index) => [
            `${variant.name}${variant.payloadType ? ` :: ${variant.payloadType}` : ""}${index < variants.length - 1 ? "," : ""}`,
            variant.description,
        ]),
    ];
    const width = Math.max(0, ...members.map(([code]) => code.length));
    const body = members.map(([code, description]) =>
        description ? `    ${code.padEnd(width)}  // ${description}` : `    ${code}`,
    );

    return ["~~~cx", `${type.name}${attributes} {`, ...body, "};", "~~~"].join("\n");
}

function renderType(type) {
    return [
        `### <span className="cx-stdlib-type-name"><code>${htmlText(type.name)}</code></span>`,
        "",
        type.description,
        "",
        renderDeclaration(type),
    ].join("\n");
}

// The signature is the heading, with the qualified name picked out; the contents list shows only the name.
function renderFunction(functionRecord) {
    const name = `${functionRecord.owner ? `${functionRecord.owner}::` : ""}${functionRecord.name}`;
    const {signature} = functionRecord;
    const at = signature.indexOf(name);
    const heading = at < 0
        ? `<span className="cx-stdlib-function-name">${qualifiedName(name)}</span>`
        : [
            `<span className="cx-stdlib-signature">${mdxText(signature.slice(0, at))}</span>`,
            `<span className="cx-stdlib-function-name">${qualifiedName(name)}</span>`,
            `<span className="cx-stdlib-signature">${mdxText(signature.slice(at + name.length))}</span>`,
        ].join("");
    const examples = (functionRecord.examples ?? []).map((example, index) =>
        [`#### ${example.title ?? `Example ${index + 1}`}`, "", `~~~${example.language ?? "cx"}`, example.code, "~~~"].join("\n"),
    );

    return [`### <code>${heading}</code> \\{#${name.replaceAll("::", "-").toLowerCase()}}`, "", functionRecord.description, ...examples.flatMap((example) => ["", example])].join("\n");
}

function renderModule(record) {
    const types = (record.types ?? []).map(renderType).join("\n\n");
    const functions = (record.functions ?? []).map(renderFunction).join("\n\n");
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

fs.rmSync(outputDirectory, {recursive: true, force: true});
fs.mkdirSync(outputDirectory, {recursive: true});
fs.writeFileSync(path.join(outputDirectory, "index.md"), renderIndex(records));

for (const record of records) {
    writeRecord(record);
}

console.log(`Generated ${records.length} standard-library module pages.`);
