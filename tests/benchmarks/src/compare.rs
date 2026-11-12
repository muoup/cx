use crate::report::{format_stats, markdown_table, render_pretty, BenchmarkResult, Table, Timing, ABSENT};
use crate::stats::TimingStats;
use crate::{Options, Rendered};
use cx_test_support::TestTempDir;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

const SIDES: [&str; 2] = ["baseline", "current"];
const TIMINGS: [Timing; 2] = [Timing::Compile, Timing::Execute];

#[derive(Serialize)]
struct ComparisonReport {
    schema: u32,
    rounds: usize,
    rows: Vec<ComparisonRow>,
}

#[derive(Serialize)]
struct ComparisonRow {
    case: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    workload: Option<String>,
    backend: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Comparison>,
    #[serde(skip_serializing_if = "Option::is_none")]
    execute: Option<Comparison>,
}

/// The samples of every round pooled for each side, and how often the current build was the
/// faster of the two within a round.
#[derive(Serialize)]
struct Comparison {
    baseline: TimingStats,
    current: TimingStats,
    /// The relative change of the current median from the baseline median.
    change: f64,
    rounds_faster: usize,
}

#[derive(Deserialize)]
struct Report {
    cases: Vec<BenchmarkResult>,
}

/// Runs this build and `baseline` alternately for `rounds`, each starting every other round so
/// that neither always runs on a warmer or cooler machine.
pub fn run(options: &Options, baseline: &Path) -> Result<Rendered, String> {
    let current = std::env::current_exe().map_err(|error| error.to_string())?;
    let binaries = [baseline.to_path_buf(), current];
    let arguments = arguments(options);

    let mut rounds = Vec::with_capacity(options.rounds);
    for round in 0..options.rounds {
        let order = match round % 2 {
            0 => [0, 1],
            _ => [1, 0],
        };
        let mut reports: [Vec<BenchmarkResult>; 2] = Default::default();
        for side in order {
            eprintln!("round {}/{}: {}", round + 1, options.rounds, SIDES[side]);
            reports[side] = measure(&binaries[side], &arguments)?;
        }
        rounds.push(reports);
    }

    let report = ComparisonReport {
        schema: 1,
        rounds: options.rounds,
        rows: rows(&rounds),
    };
    let table = table(&report);
    Ok(Rendered {
        json: serde_json::to_string_pretty(&report).map_err(|error| error.to_string())?,
        pretty: render_pretty(std::slice::from_ref(&table)),
        github: format!("## Benchmark Comparison:\n\n{}\n", markdown_table(&table)),
    })
}

fn arguments(options: &Options) -> Vec<String> {
    let mut arguments = vec![
        "--iterations".to_string(),
        options.iterations.to_string(),
        "--warmups".to_string(),
        options.warmups.to_string(),
    ];
    if let Some(backend) = &options.backend {
        arguments.extend(["--backend".to_string(), backend.clone()]);
    }
    for case in &options.cases {
        arguments.extend(["--case".to_string(), case.clone()]);
    }
    arguments
}

fn measure(binary: &Path, arguments: &[String]) -> Result<Vec<BenchmarkResult>, String> {
    let temp = TestTempDir::new("benchmark-compare");
    let output = temp.path().join("report.json");
    let status = Command::new(binary)
        .args(arguments)
        .args(["--format", "json", "--json-output"])
        .arg(&output)
        .stdout(Stdio::null())
        .status()
        .map_err(|error| format!("failed to run {}: {error}", binary.display()))?;
    if !status.success() {
        return Err(format!("{} exited with {status}", binary.display()));
    }

    let report = fs::read_to_string(&output).map_err(|error| error.to_string())?;
    let report: Report = serde_json::from_str(&report).map_err(|error| error.to_string())?;
    Ok(report.cases)
}

/// The rows both builds report, in the order the current build reports them.
fn rows(rounds: &[[Vec<BenchmarkResult>; 2]]) -> Vec<ComparisonRow> {
    let Some([_, first]) = rounds.first() else {
        return Vec::new();
    };
    first
        .iter()
        .filter(|row| !row.reference)
        .map(|row| {
            let [compile, execute] = TIMINGS.map(|timing| comparison(rounds, row, timing));
            ComparisonRow {
                case: row.case.clone(),
                workload: row.workload.clone(),
                backend: row.backend.clone(),
                compile,
                execute,
            }
        })
        .collect()
}

fn comparison(
    rounds: &[[Vec<BenchmarkResult>; 2]],
    row: &BenchmarkResult,
    timing: Timing,
) -> Option<Comparison> {
    let mut pooled: [Vec<f64>; 2] = Default::default();
    let mut rounds_faster = 0;
    for reports in rounds {
        let [baseline, current] = [0, 1].map(|side| {
            reports[side]
                .iter()
                .find(|other| same_row(other, row))
                .and_then(|other| timing.of(other))
        });
        let (baseline, current) = (baseline?, current?);
        rounds_faster += usize::from(current.median_ms < baseline.median_ms);
        pooled[0].extend(&baseline.samples_ms);
        pooled[1].extend(&current.samples_ms);
    }

    let [baseline, current] = pooled.map(TimingStats::new);
    Some(Comparison {
        change: current.median_ms / baseline.median_ms - 1.0,
        rounds_faster,
        baseline,
        current,
    })
}

fn same_row(left: &BenchmarkResult, right: &BenchmarkResult) -> bool {
    left.case == right.case
        && left.workload == right.workload
        && left.backend == right.backend
        && left.reference == right.reference
}

fn table(report: &ComparisonReport) -> Table {
    let mut header = vec!["Case".to_string(), "Backend".to_string()];
    for timing in TIMINGS {
        for side in SIDES {
            header.push(format!("{} ({side})", timing.title()));
        }
        header.push("Δ (rounds faster)".to_string());
    }

    let rows = report
        .rows
        .iter()
        .map(|row| {
            let subject = match &row.workload {
                Some(workload) => format!("{}: {workload}", row.case),
                None => row.case.clone(),
            };
            let mut cells = vec![subject, row.backend.clone()];
            for comparison in [&row.compile, &row.execute] {
                cells.extend(cells_of(comparison.as_ref(), report.rounds));
            }
            cells
        })
        .collect();

    Table {
        title: format!("Comparison over {} rounds", report.rounds),
        header,
        rows,
    }
}

fn cells_of(comparison: Option<&Comparison>, rounds: usize) -> [String; 3] {
    match comparison {
        Some(comparison) => [
            format_stats(Some(&comparison.baseline)),
            format_stats(Some(&comparison.current)),
            format!(
                "{:+.1}% ({}/{rounds})",
                comparison.change * 100.0,
                comparison.rounds_faster
            ),
        ],
        None => [ABSENT.to_string(), ABSENT.to_string(), ABSENT.to_string()],
    }
}
