use serde::Serialize;
use tabled::{
    builder::Builder,
    settings::{object::Columns, Alignment, Style},
};

#[derive(Serialize)]
pub struct BenchmarkReport {
    pub schema: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commit: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reference: Option<ReferenceCompiler>,
    pub cases: Vec<BenchmarkResult>,
}

#[derive(Serialize, Clone)]
pub struct ReferenceCompiler {
    pub command: String,
    pub version: String,
}

/// One row of the report. A case that is only compiled has no `execute`, and the workloads of a
/// project have no `compile`, which is reported once on the row of the project itself.
#[derive(Serialize)]
pub struct BenchmarkResult {
    pub case: String,
    /// The cx backend, or the command of the reference compiler.
    pub backend: String,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub reference: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compile: Option<TimingStats>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execute: Option<TimingStats>,
}

#[derive(Serialize)]
pub struct TimingStats {
    pub samples_ms: Vec<f64>,
    pub mean_ms: f64,
    pub median_ms: f64,
    pub min_ms: f64,
    pub max_ms: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub margin_of_error_ms: Option<f64>,
}

impl TimingStats {
    pub fn new(mut samples_ms: Vec<f64>) -> Self {
        samples_ms.sort_by(|left, right| left.partial_cmp(right).unwrap());
        let mean_ms = samples_ms.iter().sum::<f64>() / samples_ms.len() as f64;

        Self {
            mean_ms,
            median_ms: samples_ms[samples_ms.len() / 2],
            min_ms: samples_ms[0],
            max_ms: samples_ms[samples_ms.len() - 1],
            margin_of_error_ms: margin_of_error_ms(&samples_ms, mean_ms),
            samples_ms,
        }
    }
}

pub fn render_pretty_table(report: &BenchmarkReport) -> String {
    let mut builder = Builder::default();
    builder.push_record(header(report));
    for result in &report.cases {
        builder.push_record(row(report, result));
    }

    let mut table = builder.build();
    table.modify(Columns::new(2..), Alignment::right());
    table.with(Style::rounded());

    format!("\nResults: Benchmarks\n{}\n\n", table)
}

pub fn render_github_table(report: &BenchmarkReport) -> String {
    let header = header(report);
    let mut output = String::from("## Benchmark Results:\n\n");
    output.push_str(&format!("| {} |\n", header.join(" | ")));
    output.push_str("| --- | --- |");
    output.push_str(&" ---: |".repeat(header.len() - 2));
    output.push('\n');

    for result in &report.cases {
        let cells = row(report, result)
            .into_iter()
            .map(|cell| cell.replace('|', "\\|"))
            .collect::<Vec<_>>();
        output.push_str(&format!("| {} |\n", cells.join(" | ")));
    }

    if let Some(reference) = &report.reference {
        output.push_str(&format!(
            "\n`vs {}` compares each cx time with the `{}` time ({}).\n",
            reference.command, reference.command, reference.version
        ));
    }

    output.push('\n');
    output
}

fn header(report: &BenchmarkReport) -> Vec<String> {
    let mut header = vec!["Case".to_string(), "Backend".to_string()];
    for timing in ["Compile", "Execute"] {
        header.push(timing.to_string());
        if let Some(reference) = &report.reference {
            header.push(format!("vs {}", reference.command));
        }
    }
    header
}

fn row(report: &BenchmarkReport, result: &BenchmarkResult) -> Vec<String> {
    let reference = report
        .cases
        .iter()
        .find(|other| other.reference && !result.reference && other.case == result.case);
    let mut row = vec![result.case.clone(), result.backend.clone()];
    for (timing, reference_timing) in [
        (&result.compile, reference.map(|reference| &reference.compile)),
        (&result.execute, reference.map(|reference| &reference.execute)),
    ] {
        row.push(format_stats(timing.as_ref()));
        if report.reference.is_some() {
            row.push(format_ratio(
                timing.as_ref(),
                reference_timing.and_then(Option::as_ref),
            ));
        }
    }
    row
}

const ABSENT: &str = "—";

fn format_stats(stats: Option<&TimingStats>) -> String {
    stats.map_or(ABSENT.to_string(), |stats| {
        format_timing(stats.mean_ms, stats.margin_of_error_ms)
    })
}

fn format_ratio(stats: Option<&TimingStats>, reference: Option<&TimingStats>) -> String {
    match (stats, reference) {
        (Some(stats), Some(reference)) if stats.mean_ms > 0.0 && reference.mean_ms > 0.0 => {
            let ratio = stats.mean_ms / reference.mean_ms;
            let factor = format!("{:.2}", ratio.max(ratio.recip()));
            match factor.as_str() {
                "1.00" => "same".to_string(),
                _ if ratio > 1.0 => format!("{factor}× slower"),
                _ => format!("{factor}× faster"),
            }
        }
        _ => ABSENT.to_string(),
    }
}

fn format_timing(mean_ms: f64, margin_ms: Option<f64>) -> String {
    let (mean, margin, unit) = if mean_ms >= 1000.0 {
        (
            mean_ms / 1000.0,
            margin_ms.map(|margin| margin / 1000.0),
            "s",
        )
    } else {
        (mean_ms, margin_ms, "ms")
    };
    let margin = margin
        .map(|margin| format!("{margin:.2}"))
        .unwrap_or_else(|| "n/a".to_string());

    format!("{mean:.2} ± {margin} {unit}")
}

fn margin_of_error_ms(samples_ms: &[f64], mean_ms: f64) -> Option<f64> {
    let sample_count = samples_ms.len();
    if sample_count < 2 {
        return None;
    }

    let variance = samples_ms
        .iter()
        .map(|sample| (sample - mean_ms).powi(2))
        .sum::<f64>()
        / (sample_count - 1) as f64;
    let standard_error = (variance / sample_count as f64).sqrt();

    Some(t_critical_95(sample_count - 1) * standard_error)
}

fn t_critical_95(degrees_of_freedom: usize) -> f64 {
    const VALUES: [f64; 30] = [
        12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 2.160,
        2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 2.060, 2.056,
        2.052, 2.048, 2.045, 2.042,
    ];

    VALUES
        .get(degrees_of_freedom.saturating_sub(1))
        .copied()
        .unwrap_or(1.96)
}
