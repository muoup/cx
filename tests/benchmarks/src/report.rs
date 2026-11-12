use crate::stats::TimingStats;
use cx_test_support::ReferenceCompiler;
use serde::{Deserialize, Serialize};
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

/// One row of the report. A case that is only compiled has no `execute`. The workloads of a
/// project have no `compile`: it is reported once on the row of the project itself, whose
/// `execute` is the total of its workloads.
#[derive(Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub case: String,
    /// Set on the rows of the individual workloads of a case, which are rendered apart from
    /// the rows of the cases themselves.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workload: Option<String>,
    /// The cx backend, or the command of the reference compiler.
    pub backend: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub reference: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub compile: Option<TimingStats>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execute: Option<TimingStats>,
}

pub fn render_pretty_table(report: &BenchmarkReport) -> String {
    render_pretty(&tables(report))
}

pub fn render_pretty(tables: &[Table]) -> String {
    let mut output = String::from("\n");
    for table in tables {
        let mut builder = Builder::default();
        builder.push_record(table.header.clone());
        for row in table.rows.iter().cloned() {
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

pub fn markdown_table(table: &Table) -> String {
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

pub struct Table {
    pub title: String,
    pub header: Vec<String>,
    pub rows: Vec<Vec<String>>,
}

#[derive(Clone, Copy)]
pub enum Timing {
    Compile,
    Execute,
}

impl Timing {
    pub fn title(self) -> &'static str {
        match self {
            Timing::Compile => "Compile",
            Timing::Execute => "Execute",
        }
    }

    pub fn of(self, result: &BenchmarkResult) -> Option<&TimingStats> {
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

pub const ABSENT: &str = "—";

pub fn format_stats(stats: Option<&TimingStats>) -> String {
    stats.map_or(ABSENT.to_string(), |stats| {
        let spread = (stats.samples_ms.len() > 1).then_some(stats.mad_ms);
        format_timing(stats.median_ms, spread)
    })
}

fn format_ratio(stats: Option<&TimingStats>, reference: Option<&TimingStats>) -> String {
    match (stats, reference) {
        (Some(stats), Some(reference)) if stats.median_ms > 0.0 && reference.median_ms > 0.0 => {
            let ratio = stats.median_ms / reference.median_ms;
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

fn format_timing(median_ms: f64, spread_ms: Option<f64>) -> String {
    let (scale, unit) = match median_ms >= 1000.0 {
        true => (1000.0, "s"),
        false => (1.0, "ms"),
    };
    let spread = spread_ms.map_or(String::new(), |spread| format!(" ± {:.2}", spread / scale));
    format!("{:.2}{spread} {unit}", median_ms / scale)
}
