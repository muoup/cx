import fs from "node:fs";
import path from "node:path";

const root = process.env.CX_REPORT_DIR || "publisher-input";
const event = readJson(process.env.GITHUB_EVENT_PATH) || {};
const workflowRun = event.workflow_run || {};
const pullRequest = readJson(path.join(root, "pull-request.json")) || {};
const metadata = readJson(path.join(root, "pr", "metadata.json"));
const pullRequestNumber = Number(pullRequest.number || 0);
const outputPath = process.env.CX_REPORT_OUTPUT || path.join(root, "comment.md");

if (!pullRequestNumber) {
  fs.writeFileSync(outputPath, "");
  process.exit(0);
}

const report = readJson(path.join(root, "pr", "report.json"));
const baselineReport = readJson(path.join(root, "baseline", "report.json"));
const baselineMetadata = readJson(path.join(root, "baseline", "metadata.json"));
const jobsDocument = readJson(path.join(root, "jobs.json"));
const jobs = Array.isArray(jobsDocument)
  ? jobsDocument
  : Array.isArray(jobsDocument?.jobs)
    ? jobsDocument.jobs
    : [];
const repository = process.env.GITHUB_REPOSITORY || "";
const server = process.env.GITHUB_SERVER_URL || "https://github.com";
const repositoryUrl = `${server}/${repository}`;
const runUrl = workflowRun.html_url || `${repositoryUrl}/actions/runs/${workflowRun.id}`;
const commit = report?.commit || metadata?.source_sha || workflowRun.head_sha || "";
const artifactIssue = validateMetadata(metadata, workflowRun, pullRequestNumber, pullRequest);
const usableReport = artifactIssue ? null : report;
const baselineCases = indexCases(baselineReport);

const lines = [
  "<!-- cx-ci-report -->",
  `## CX CI · ${status(workflowRun.conclusion)}`,
  "",
  `Commit: ${link(commit.slice(0, 8), commit ? `${repositoryUrl}/commit/${commit}` : "")} · ${link("View workflow run", runUrl)}`,
  `Pull request: ${link(`#${pullRequestNumber}`, pullRequest.html_url || `${repositoryUrl}/pull/${pullRequestNumber}`)}`,
  "",
  "| Check | Result | Details |",
  "| --- | --- | --- |",
];

for (const job of orderedJobs(jobs)) {
  lines.push(
    `| ${cell(job.name || "Check")} | ${cell(status(job.conclusion))} | ${link("open job", job.html_url || "")} |`,
  );
}

if (jobs.length === 0) {
  lines.push(`| Workflow | ${cell(status(workflowRun.conclusion))} | ${link("open run", runUrl)} |`);
}

lines.push("", "### Benchmarks", "");

if (artifactIssue) {
  lines.push(`Benchmark artifact was ignored: ${artifactIssue}`);
} else if (!usableReport?.cases || !Array.isArray(usableReport.cases)) {
  lines.push("Benchmark artifact was unavailable; see the benchmark job logs for details.");
} else {
  const reference = usableReport.reference;
  const referenceCases = new Map(
    usableReport.cases.filter((result) => result.reference).map((result) => [subjectKey(result), result]),
  );
  const table = (subject, timings, results) => {
    const columns = timings.flatMap((timing) => {
      const title = timing[0].toUpperCase() + timing.slice(1);
      return reference ? [title, "Δ", `vs ${reference.command}`] : [title, "Δ"];
    });
    const rows = [
      `| ${subject} | Backend | ${columns.map(cell).join(" | ")} |`,
      `| --- | --- |${" ---: |".repeat(columns.length)}`,
    ];

    for (const result of results) {
      const baseline = baselineCases.get(caseKey(result));
      const referenceResult = result.reference ? null : referenceCases.get(subjectKey(result));
      const cells = timings.flatMap((timing) => {
        const current = result[timing];
        const timingCells = [formatTiming(current), formatDelta(medianOf(current), medianOf(baseline?.[timing]))];
        if (reference) {
          timingCells.push(formatRatio(medianOf(current), medianOf(referenceResult?.[timing])));
        }
        return timingCells;
      });
      rows.push(`| ${cell(result.workload ?? result.case)} | ${cell(result.backend)} | ${cells.map(cell).join(" | ")} |`);
    }
    return rows;
  };

  lines.push(
    ...table(
      "Case",
      ["compile", "execute"],
      usableReport.cases.filter((result) => result.workload == null),
    ),
  );

  if (reference) {
    lines.push(
      "",
      `\`vs ${cell(reference.command)}\` compares each cx time with the \`${cell(reference.command)}\` time (${cell(reference.version)}).`,
    );
  }

  const workloads = new Map();
  for (const result of usableReport.cases.filter((result) => result.workload != null)) {
    workloads.set(result.case, [...(workloads.get(result.case) || []), result]);
  }
  for (const [name, results] of workloads) {
    lines.push(
      "",
      "<details>",
      `<summary>${escapeHtml(name)} workloads</summary>`,
      "",
      ...table("Workload", ["execute"], results),
      "",
      "</details>",
    );
  }

  if (baselineReport?.cases && baselineMetadata) {
    const baselineBranch = baselineMetadata.base_branch || pullRequest.base?.ref || "base";
    const baselineCommit = baselineReport.commit || baselineMetadata.source_sha || "";
    const baselineRunId = process.env.CX_BASELINE_RUN_ID || "";
    const baselineRunUrl = baselineRunId ? `${repositoryUrl}/actions/runs/${baselineRunId}` : "";
    lines.push(
      "",
      `Δ compares against ${link(`${baselineBranch} baseline`, baselineRunUrl)} at ${link(baselineCommit.slice(0, 8), baselineCommit ? `${repositoryUrl}/commit/${baselineCommit}` : "the recorded baseline commit")}. Lower is faster.`,
    );
  } else {
    lines.push("", "No successful baseline benchmark artifact was available, so deltas are omitted.");
  }
}

lines.push("", "<sub>Generated by the trusted CI report publisher.</sub>", "");
fs.writeFileSync(outputPath, lines.join("\n"));

function readJson(filePath) {
  try {
    return JSON.parse(fs.readFileSync(filePath, "utf8"));
  } catch {
    return null;
  }
}

function validateMetadata(reportMetadata, run, number, resolvedPullRequest) {
  if (!reportMetadata) {
    return null;
  }
  if (reportMetadata.pr_number && String(reportMetadata.pr_number) !== String(number)) {
    return "it did not belong to this pull request";
  }
  if (reportMetadata.run_id && String(reportMetadata.run_id) !== String(run.id)) {
    return "it did not belong to this workflow run";
  }
  const acceptedShas = new Set([run.head_sha, resolvedPullRequest.head?.sha].filter(Boolean));
  if (reportMetadata.source_sha && acceptedShas.size > 0 && !acceptedShas.has(reportMetadata.source_sha)) {
    return "its source commit did not match the workflow run";
  }
  return null;
}

function orderedJobs(allJobs) {
  const order = new Map([
    ["integration", 0],
    ["benchmarks", 1],
  ]);
  return [...allJobs].sort((left, right) => {
    const leftOrder = order.get(left.name) ?? 2;
    const rightOrder = order.get(right.name) ?? 2;
    return leftOrder - rightOrder || String(left.name).localeCompare(String(right.name));
  });
}

function indexCases(reportDocument) {
  const cases = Array.isArray(reportDocument?.cases) ? reportDocument.cases : [];
  return new Map(cases.map((result) => [caseKey(result), result]));
}

function caseKey(result) {
  return `${subjectKey(result)}\u0000${result?.backend || ""}`;
}

function subjectKey(result) {
  return `${result?.case || ""}\u0000${result?.workload || ""}`;
}

function escapeHtml(value) {
  return String(value ?? "").replace(/[&<>]/g, (character) => `&#${character.charCodeAt(0)};`);
}

function formatRatio(current, reference) {
  if (!Number.isFinite(current) || !Number.isFinite(reference) || current <= 0 || reference <= 0) {
    return "—";
  }
  const ratio = current / reference;
  const factor = Math.max(ratio, 1 / ratio).toFixed(2);
  if (factor === "1.00") {
    return "same";
  }
  return `${factor}× ${ratio > 1 ? "slower" : "faster"}`;
}

function formatTiming(timing) {
  if (!timing) {
    return "—";
  }
  const median = medianOf(timing);
  if (!Number.isFinite(median)) {
    return "n/a";
  }

  const useSeconds = median >= 1000;
  const divisor = useSeconds ? 1000 : 1;
  const unit = useSeconds ? "s" : "ms";
  const spread = spreadOf(timing);
  const formattedSpread = Number.isFinite(spread) ? ` ± ${(spread / divisor).toFixed(2)}` : "";

  return `${(median / divisor).toFixed(2)}${formattedSpread} ${unit}`;
}

function formatDelta(current, baseline) {
  if (!Number.isFinite(current) || !Number.isFinite(baseline) || baseline === 0) {
    return "—";
  }
  const delta = ((current - baseline) / baseline) * 100;
  return `${delta >= 0 ? "+" : ""}${delta.toFixed(1)}%`;
}

// Reports from before the median ± MAD summary carry no `mad_ms`, so it is recomputed from their samples
function medianOf(timing) {
  if (Number.isFinite(timing?.median_ms)) {
    return timing.median_ms;
  }
  return Array.isArray(timing?.samples_ms) ? median(timing.samples_ms) : null;
}

function spreadOf(timing) {
  const samples = Array.isArray(timing?.samples_ms) ? timing.samples_ms : [];
  if (samples.length < 2) {
    return null;
  }
  if (Number.isFinite(timing.mad_ms)) {
    return timing.mad_ms;
  }
  const center = median(samples);
  return median(samples.map((sample) => Math.abs(sample - center)));
}

function median(samples) {
  if (samples.length === 0) {
    return null;
  }
  const sorted = [...samples].sort((left, right) => left - right);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0 ? (sorted[middle - 1] + sorted[middle]) / 2 : sorted[middle];
}

function status(conclusion) {
  const values = {
    success: "✅ passed",
    failure: "❌ failed",
    cancelled: "⏹ cancelled",
    skipped: "⏭ skipped",
    neutral: "⚪ neutral",
    timed_out: "⏱ timed out",
    action_required: "⚠ action required",
  };
  return values[conclusion] || "⏳ in progress";
}

function cell(value) {
  return String(value ?? "").replace(/[\\|`]/g, (character) => `\\${character}`).replace(/\r?\n/g, " ");
}

function link(label, url) {
  if (!url) {
    return cell(label);
  }
  return `[${cell(label)}](${url})`;
}
