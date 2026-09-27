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

function tableCell(value) {
    return String(value ?? "").replaceAll("|", "\\|").replaceAll("\n", " ");
}

function renderTable(headers, rows) {
    return [
        `| ${headers.join(" | ")} |`,
        `| ${headers.map(() => "---").join(" | ")} |`,
        ...rows.map((row) => `| ${row.map(tableCell).join(" | ")} |`),
    ].join("\n");
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

function renderParameters(parameters = []) {
    if (parameters.length === 0) {
        return "No parameters.";
    }

    return renderTable(
        ["Name", "Type", "Description"],
        parameters.map((parameter) => [inlineCode(parameter.name), inlineCode(parameter.type), parameter.description]),
    );
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

function renderFunction(functionRecord) {
    const metadata = functionRecord.stage && functionRecord.stage !== "runtime"
        ? `Stage: ${inlineCode(functionRecord.stage)}`
        : "";
    const owner = functionRecord.owner ? `${functionRecord.owner}::` : "";
    const parameters = renderParameters(functionRecord.parameters);
    const returnType = functionRecord.returnType ?? "void";
    const returns = returnType === "void"
        ? []
        : [
            "#### Returns",
            "",
            `${inlineCode(returnType)} — ${functionRecord.returnDescription ?? "The documented return value."}`,
        ];
    const metadataBlock = metadata ? [`<div className="cx-stdlib-meta">`, "", metadata, "", "</div>", ""] : [];
    const examples = (functionRecord.examples ?? [])
        .map((example, index) => {
            const heading = example.title ?? `Example ${index + 1}`;
            const language = example.language ?? "cx";
            return [`#### ${heading}`, "", `~~~${language}`, example.code, "~~~"].join("\n");
        })
        .join("\n\n");

    return [
        `### <span className="cx-stdlib-function-name"><code>${htmlText(`${owner}${functionRecord.name}`)}</code></span>`,
        "",
        functionRecord.description,
        "",
        `<div className="cx-stdlib-signature">`,
        "",
        `~~~cx\n${functionRecord.signature}\n~~~`,
        "",
        "</div>",
        "",
        ...metadataBlock,
        "#### Parameters",
        "",
        parameters,
        "",
        ...returns,
        examples ? `\n${examples}` : "",
    ].join("\n");
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
