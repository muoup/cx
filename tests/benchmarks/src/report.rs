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

/// One row of the report. A case that is only compiled has no `execute`. The workloads of a
/// project have no `compile`: it is reported once on the row of the project itself, whose
/// `execute` is the total of its workloads.
#[derive(Serialize)]
pub struct BenchmarkResult {
    pub case: String,
    /// Set on the rows of the individual workloads of a case, which are rendered apart from
    /// the rows of the cases themselves.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workload: Option<String>,
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
    let mut output = String::from("\n");
    for table in tables(report) {
        let mut builder = Builder::default();
        builder.push_record(table.header);
        for row in table.rows {
            builder.push_record(row);
        }

        let mut rendered = builder.build();
        rendered.modify(Columns::new(2..), Alignment::right());
        rendered.with(Style::rounded());
        output.push_str(&format!("Results: {}\n{rendered}\n\n", table.title));
    }
    output
}

pub fn render_github_table(report: &BenchmarkReport) -> String {
    let mut tables = tables(report).into_iter();
    let mut output = String::from("## Benchmark Results:\n\n");
    output.push_str(&markdown_table(&tables.next().expect("a report has its table of cases")));

    if let Some(reference) = &report.reference {
        output.push_str(&format!(
            "\n`vs {}` compares each cx time with the `{}` time ({}).\n",
            reference.command, reference.command, reference.version
        ));
    }

    for table in tables {
        output.push_str(&format!(
            "\n<details>\n<summary>{}</summary>\n\n{}\n</details>\n",
            table.title,
            markdown_table(&table)
        ));
    }

    output.push('\n');
    output
}

fn markdown_table(table: &Table) -> String {
    let mut output = format!("| {} |\n", table.header.join(" | "));
    output.push_str("| --- | --- |");
    output.push_str(&" ---: |".repeat(table.header.len() - 2));
    output.push('\n');

    for row in &table.rows {
        let cells = row
            .iter()
            .map(|cell| cell.replace('|', "\\|"))
            .collect::<Vec<_>>();
        output.push_str(&format!("| {} |\n", cells.join(" | ")));
    }
    output
}

struct Table {
    title: String,
    header: Vec<String>,
    rows: Vec<Vec<String>>,
}

#[derive(Clone, Copy)]
enum Timing {
    Compile,
    Execute,
}

impl Timing {
    fn title(self) -> &'static str {
        match self {
            Timing::Compile => "Compile",
            Timing::Execute => "Execute",
        }
    }

    fn of(self, result: &BenchmarkResult) -> Option<&TimingStats> {
        match self {
            Timing::Compile => result.compile.as_ref(),
            Timing::Execute => result.execute.as_ref(),
        }
    }
}

/// The table of the cases, then one table for the workloads of each case that has them.
fn tables(report: &BenchmarkReport) -> Vec<Table> {
    let mut tables = vec![table(
        report,
        "Benchmarks".to_string(),
        "Case",
        &[Timing::Compile, Timing::Execute],
        |result| result.workload.is_none().then(|| result.case.clone()),
    )];

    let mut cases = Vec::new();
    for result in &report.cases {
        if result.workload.is_some() && !cases.contains(&&result.case) {
            cases.push(&result.case);
        }
    }
    for case in cases {
        tables.push(table(
            report,
            format!("{case} workloads"),
            "Workload",
            &[Timing::Execute],
            |result| result.workload.clone().filter(|_| &result.case == case),
        ));
    }
    tables
}

/// A table of the results `subject_of` names, which it leads each row with.
fn table(
    report: &BenchmarkReport,
    title: String,
    subject: &str,
    timings: &[Timing],
    subject_of: impl Fn(&BenchmarkResult) -> Option<String>,
) -> Table {
    let mut header = vec![subject.to_string(), "Backend".to_string()];
    for timing in timings {
        header.push(timing.title().to_string());
        if let Some(reference) = &report.reference {
            header.push(format!("vs {}", reference.command));
        }
    }

    let rows = report
        .cases
        .iter()
        .filter_map(|result| {
            let reference = report.cases.iter().find(|other| {
                other.reference
                    && !result.reference
                    && other.case == result.case
                    && other.workload == result.workload
            });
            let mut row = vec![subject_of(result)?, result.backend.clone()];
            for timing in timings {
                let stats = timing.of(result);
                row.push(format_stats(stats));
                if report.reference.is_some() {
                    row.push(format_ratio(stats, reference.and_then(|other| timing.of(other))));
                }
            }
            Some(row)
        })
        .collect();

    Table {
        title,
        header,
        rows,
    }
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
