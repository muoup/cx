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

function yamlString(value) {
    return JSON.stringify(String(value ?? ""));
}

function inlineCode(value) {
    return `\`${String(value ?? "").replaceAll("`", "\\`")}\``;
}

function renderParameters(parameters = []) {
    if (parameters.length === 0) {
        return "No parameters.";
    }

    return parameters
        .map((parameter) => {
            const mode = parameter.mode ? ` (${parameter.mode})` : "";
            return `- ${inlineCode(parameter.name)}: ${inlineCode(parameter.type)}${mode} — ${parameter.description}`;
        })
        .join("\n");
}

function renderFields(fields = []) {
    if (fields.length === 0) {
        return "No public fields are listed.";
    }

    return fields
        .map((field) => `- ${inlineCode(field.name)}: ${inlineCode(field.type)} — ${field.description}`)
        .join("\n");
}

function renderVariants(variants = []) {
    if (variants.length === 0) {
        return "No variants are listed.";
    }

    return variants
        .map((variant) => {
            const payload = variant.payloadType ? `: ${inlineCode(variant.payloadType)}` : "";
            return `- ${inlineCode(variant.name)}${payload} — ${variant.description}`;
        })
        .join("\n");
}

function renderType(type) {
    const attributes = type.attributes?.length
        ? `\n\nAttributes: ${type.attributes.map(inlineCode).join(", ")}.`
        : "";
    const fields = type.fields?.length
        ? `\n\n#### Fields\n\n${renderFields(type.fields)}`
        : "";
    const variants = type.variants?.length
        ? `\n\n#### Variants\n\n${renderVariants(type.variants)}`
        : "";

    return [
        `### ${inlineCode(type.name)}`,
        "",
        `${type.kind ? `${type.kind}. ` : ""}${type.description}${attributes}${fields}${variants}`,
    ].join("\n");
}

function renderFunction(functionRecord) {
    const metadata = [
        functionRecord.stage && functionRecord.stage !== "runtime"
            ? `Stage: ${inlineCode(functionRecord.stage)}`
            : null,
        functionRecord.safety ? `Safety: ${inlineCode(functionRecord.safety)}` : null,
    ].filter(Boolean).join(" · ");
    const owner = functionRecord.owner ? `${functionRecord.owner}::` : "";
    const parameters = renderParameters(functionRecord.parameters);
    const returnDescription = functionRecord.returnDescription ?? "The documented return value.";
    const metadataBlock = metadata ? [`> ${metadata}`, ""] : [];
    const examples = (functionRecord.examples ?? [])
        .map((example, index) => {
            const heading = example.title ?? `Example ${index + 1}`;
            const language = example.language ?? "cx";
            return [`#### ${heading}`, "", `~~~${language}`, example.code, "~~~"].join("\n");
        })
        .join("\n\n");

    return [
        `### ${inlineCode(`${owner}${functionRecord.name}`)}`,
        "",
        functionRecord.description,
        "",
        `~~~cx\n${functionRecord.signature}\n~~~`,
        "",
        ...metadataBlock,
        "#### Parameters",
        "",
        parameters,
        "",
        "#### Returns",
        "",
        `- ${inlineCode(functionRecord.returnType ?? "void")} — ${returnDescription}`,
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
        `title: ${yamlString(record.title ?? record.module)}`,
        `sidebar_position: ${record.order ?? 100}`,
        `description: ${yamlString(record.summary ?? "Standard-library module.")}`,
        "---",
        "",
        `# ${record.title ?? record.module}`,
        "",
        `${record.summary ?? "Standard-library module."}`,
        "",
        `Module: ${inlineCode(record.module)}  \\`,
        `Source: ${inlineCode(sourcePath(record))}  \\`,
        `Status: ${inlineCode(record.status ?? "experimental")}`,
        "",
        sections.join("\n\n"),
        "",
    ].join("\n");
}

function renderIndex(records) {
    const modules = records
        .map((record) => {
            const slug = moduleSlug(record.module);
            return `- [${record.title ?? record.module}](./${slug}.md) — ${record.summary ?? "Standard-library module."}`;
        })
        .join("\n");

    return [
        "---",
        "title: Standard Library",
        "sidebar_position: 1",
        "description: Structured reference for the CX standard library.",
        "---",
        "",
        "# Standard Library",
        "",
        "This reference is generated from the structured records in `site/stdlib`. Descriptions are intentionally concise placeholders while the API documentation is being filled in.",
        "",
        "## Modules",
        "",
        modules,
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
