use cx_test_support::{
    assert_stdout, backend_name, compile_file, expected_stdout, run_binary, CompilationMode,
    CompilerBackend, TestTempDir,
};
use serde::Serialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tabled::{
    settings::{object::Columns, Alignment, Style},
    Table, Tabled,
};

const DEFAULT_ITERATIONS: usize = 3;
const DEFAULT_WARMUPS: usize = 1;

#[derive(Clone, Copy)]
enum OutputFormat {
    Pretty,
    Json,
    Github,
}

struct Options {
    iterations: usize,
    warmups: usize,
    backend: Option<String>,
    format: OutputFormat,
    json_output: Option<PathBuf>,
    cases: Vec<PathBuf>,
}

#[derive(Serialize)]
struct BenchmarkReport {
    schema: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    commit: Option<String>,
    cases: Vec<BenchmarkResult>,
}

#[derive(Serialize)]
struct BenchmarkResult {
    case: String,
    backend: String,
    compile: TimingStats,
    execute: TimingStats,
}

#[derive(Serialize)]
struct TimingStats {
    samples_ms: Vec<f64>,
    mean_ms: f64,
    median_ms: f64,
    min_ms: f64,
    max_ms: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    margin_of_error_ms: Option<f64>,
}

#[derive(Tabled)]
struct BenchmarkTableRow {
    #[tabled(rename = "Case")]
    case: String,
    #[tabled(rename = "Backend")]
    backend: String,
    #[tabled(rename = "Compile")]
    compile: String,
    #[tabled(rename = "Execute")]
    execute: String,
}

fn main() {
    let options = match parse_options(env::args().skip(1)) {
        Ok(options) => options,
        Err(error) => {
            eprintln!("error: {error}");
            eprintln!("usage: cargo run -p cx-benchmarks -- [options]");
            std::process::exit(2);
        }
    };

    if let Err(error) = run(options) {
        eprintln!("benchmark failed: {error}");
        std::process::exit(1);
    }
}

fn run(options: Options) -> Result<(), String> {
    let cases = if options.cases.is_empty() {
        discover_cases(&PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("fixtures"))?
    } else {
        options.cases
    };

    if cases.is_empty() {
        return Err("no benchmark cases were found".to_string());
    }

    let backends = select_backends(options.backend.as_deref())?;
    let mut results = Vec::new();

    for case in cases {
        for backend in &backends {
            results.push(benchmark_case(
                &case,
                *backend,
                options.iterations,
                options.warmups,
            )?);
        }
    }

    let report = BenchmarkReport {
        schema: 1,
        commit: env::var("GITHUB_SHA").ok(),
        cases: results,
    };
    let serialized_report =
        serde_json::to_string_pretty(&report).map_err(|error| error.to_string())?;

    if let Some(path) = options.json_output {
        if let Some(parent) = path
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
        {
            fs::create_dir_all(parent).map_err(|error| error.to_string())?;
        }
        fs::write(path, &serialized_report).map_err(|error| error.to_string())?;
    }

    match options.format {
        OutputFormat::Json => println!("{serialized_report}"),
        OutputFormat::Pretty => print!("{}", render_pretty_table(&report)),
        OutputFormat::Github => print!("{}", render_github_table(&report)),
    }

    Ok(())
}

fn benchmark_case(
    input: &Path,
    backend: CompilerBackend,
    iterations: usize,
    warmups: usize,
) -> Result<BenchmarkResult, String> {
    let expected = expected_stdout(input)
        .map_err(|error| error.to_string())?
        .ok_or_else(|| {
            format!(
                "{} has no inline or sidecar stdout expectation",
                input.display()
            )
        })?;
    let backend_label = backend_name(backend).to_string();
    let case_label = input
        .strip_prefix(Path::new(env!("CARGO_MANIFEST_DIR")))
        .unwrap_or(input)
        .display()
        .to_string();
    let mut compile_samples = Vec::with_capacity(iterations);

    for iteration in 0..iterations {
        let temp = TestTempDir::new(&format!(
            "benchmark-{case_label}-{backend_label}-{iteration}"
        ));
        let compilation = compile_file(input, backend, CompilationMode::Executable, &temp)
            .map_err(|failure| {
                format!(
                    "{case_label} ({backend_label}) compilation failed:\n{}",
                    failure.rendered
                )
            })?;
        compile_samples.push(duration_ms(compilation.elapsed));
    }

    let execution_temp = TestTempDir::new(&format!("benchmark-{case_label}-{backend_label}-run"));
    let compilation = compile_file(input, backend, CompilationMode::Executable, &execution_temp)
        .map_err(|failure| {
            format!(
                "{case_label} ({backend_label}) setup compilation failed:\n{}",
                failure.rendered
            )
        })?;
    let working_directory = input
        .parent()
        .ok_or_else(|| format!("{} has no working directory", input.display()))?;

    for _ in 0..warmups {
        let execution = run_binary(&compilation.output, working_directory)?;
        verify_execution(
            &case_label,
            &backend_label,
            &expected,
            &execution.stdout,
            execution.success,
            execution.status_code,
            &execution.stderr,
        )?;
    }

    let mut execute_samples = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let execution = run_binary(&compilation.output, working_directory)?;
        verify_execution(
            &case_label,
            &backend_label,
            &expected,
            &execution.stdout,
            execution.success,
            execution.status_code,
            &execution.stderr,
        )?;
        execute_samples.push(duration_ms(execution.elapsed));
    }

    Ok(BenchmarkResult {
        case: case_label,
        backend: backend_label,
        compile: stats(compile_samples),
        execute: stats(execute_samples),
    })
}

fn verify_execution(
    case: &str,
    backend: &str,
    expected: &str,
    actual: &str,
    success: bool,
    status_code: Option<i32>,
    stderr: &str,
) -> Result<(), String> {
    if !success {
        return Err(format!(
            "{case} ({backend}) exited with {status_code:?}:\n{stderr}"
        ));
    }

    assert_stdout(expected, actual, &format!("{case} ({backend})"))
}

fn stats(mut samples_ms: Vec<f64>) -> TimingStats {
    samples_ms.sort_by(|left, right| left.partial_cmp(right).unwrap());
    let sum = samples_ms.iter().sum::<f64>();
    let median_ms = samples_ms[samples_ms.len() / 2];

    let mut timing = TimingStats {
        mean_ms: sum / samples_ms.len() as f64,
        median_ms,
        min_ms: samples_ms[0],
        max_ms: samples_ms[samples_ms.len() - 1],
        samples_ms,
        margin_of_error_ms: None,
    };
    timing.margin_of_error_ms = margin_of_error_ms(&timing);
    timing
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

fn render_pretty_table(report: &BenchmarkReport) -> String {
    let rows = report
        .cases
        .iter()
        .map(|result| BenchmarkTableRow {
            case: result.case.clone(),
            backend: result.backend.clone(),
            compile: format_stats(&result.compile),
            execute: format_stats(&result.execute),
        })
        .collect::<Vec<_>>();
    let mut table = Table::new(rows);
    table.modify(Columns::one(2), Alignment::right());
    table.modify(Columns::one(3), Alignment::right());
    table.with(Style::rounded());

    format!("\nResults: Benchmarks\n{}\n\n", table)
}

fn render_github_table(report: &BenchmarkReport) -> String {
    let mut output = String::from("## Benchmark Results:\n\n");
    output.push_str("| Case | Backend | Compile | Execute |\n");
    output.push_str("| --- | --- | ---: | ---: |\n");

    for result in &report.cases {
        output.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            result.case.replace('|', "\\|"),
            result.backend,
            format_stats(&result.compile),
            format_stats(&result.execute),
        ));
    }

    output.push('\n');
    output
}

fn format_stats(stats: &TimingStats) -> String {
    format_timing(stats.mean_ms, stats.margin_of_error_ms)
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

fn margin_of_error_ms(stats: &TimingStats) -> Option<f64> {
    let sample_count = stats.samples_ms.len();
    if sample_count < 2 {
        return None;
    }

    let variance = stats
        .samples_ms
        .iter()
        .map(|sample| (sample - stats.mean_ms).powi(2))
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

#[cfg(test)]
mod tests {
    use super::{format_stats, format_timing, margin_of_error_ms, stats};

    #[test]
    fn formats_milliseconds_and_seconds() {
        assert_eq!(format_timing(999.0, Some(12.5)), "999.00 ± 12.50 ms");
        assert_eq!(format_timing(1250.0, Some(25.0)), "1.25 ± 0.03 s");
    }

    #[test]
    fn reports_margin_of_error_for_multiple_samples() {
        let timing = stats(vec![100.0, 110.0, 120.0]);
        let margin = margin_of_error_ms(&timing).unwrap();

        assert!((margin - 24.84).abs() < 0.01);
        assert_eq!(format_stats(&timing), "110.00 ± 24.84 ms");
    }

    #[test]
    fn reports_unavailable_margin_for_one_sample() {
        assert_eq!(format_stats(&stats(vec![1250.0])), "1.25 ± n/a s");
    }
}

fn discover_cases(root: &Path) -> Result<Vec<PathBuf>, String> {
    let mut cases = Vec::new();
    discover_cases_in(root, &mut cases)?;
    cases.sort();
    Ok(cases)
}

fn discover_cases_in(root: &Path, cases: &mut Vec<PathBuf>) -> Result<(), String> {
    let entries = fs::read_dir(root).map_err(|error| {
        format!(
            "failed to read benchmark directory {}: {error}",
            root.display()
        )
    })?;

    for entry in entries {
        let path = entry
            .map_err(|error| format!("failed to read benchmark entry: {error}"))?
            .path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();

        if name.starts_with('_') {
            continue;
        }
        if path.is_dir() {
            discover_cases_in(&path, cases)?;
            continue;
        }
        if matches!(
            path.extension().and_then(|extension| extension.to_str()),
            Some("cx") | Some("c")
        ) {
            cases.push(path);
        }
    }

    Ok(())
}

fn select_backends(selection: Option<&str>) -> Result<Vec<CompilerBackend>, String> {
    match selection.unwrap_or("available") {
        "available" => Ok(available_backends()),
        "cranelift" => Ok(vec![CompilerBackend::Cranelift]),
        "llvm" => {
            if cfg!(feature = "backend-llvm") {
                Ok(vec![CompilerBackend::LLVM])
            } else {
                Err("LLVM benchmarks require the backend-llvm feature".to_string())
            }
        }
        "both" => {
            if !cfg!(feature = "backend-llvm") {
                return Err(
                    "the both backend selection requires the backend-llvm feature".to_string(),
                );
            }
            Ok(vec![CompilerBackend::Cranelift, CompilerBackend::LLVM])
        }
        other => Err(format!("unknown backend selection: {other}")),
    }
}

fn available_backends() -> Vec<CompilerBackend> {
    let mut backends = vec![CompilerBackend::Cranelift];
    if cfg!(feature = "backend-llvm") {
        backends.push(CompilerBackend::LLVM);
    }
    backends
}

fn parse_options(args: impl IntoIterator<Item = String>) -> Result<Options, String> {
    let mut options = Options {
        iterations: DEFAULT_ITERATIONS,
        warmups: DEFAULT_WARMUPS,
        backend: None,
        format: OutputFormat::Pretty,
        json_output: None,
        cases: Vec::new(),
    };
    let mut args = args.into_iter();

    while let Some(argument) = args.next() {
        match argument.as_str() {
            "--iterations" => options.iterations = parse_count(&mut args, "iterations")?,
            "--warmups" => options.warmups = parse_count(&mut args, "warmups")?,
            "--backend" => options.backend = Some(next_value(&mut args, "backend")?),
            "--format" => {
                options.format = match next_value(&mut args, "format")?.as_str() {
                    "pretty" => OutputFormat::Pretty,
                    "json" => OutputFormat::Json,
                    "github" => OutputFormat::Github,
                    other => return Err(format!("unknown output format: {other}")),
                }
            }
            "--json-output" => {
                options.json_output = Some(PathBuf::from(next_value(&mut args, "json-output")?))
            }
            "--case" => options
                .cases
                .push(PathBuf::from(next_value(&mut args, "case")?)),
            "--help" | "-h" => return Err(help_text()),
            other => return Err(format!("unknown option: {other}")),
        }
    }

    if options.iterations == 0 {
        return Err("iterations must be greater than zero".to_string());
    }

    let current_directory = env::current_dir().map_err(|error| error.to_string())?;
    for case in &mut options.cases {
        if case.is_relative() {
            *case = current_directory.join(&*case);
        }
    }
    if let Some(path) = &mut options.json_output {
        if path.is_relative() {
            *path = current_directory.join(&*path);
        }
    }

    Ok(options)
}

fn parse_count(args: &mut impl Iterator<Item = String>, name: &str) -> Result<usize, String> {
    next_value(args, name)?
        .parse::<usize>()
        .map_err(|_| format!("{name} must be a positive integer"))
}

fn next_value(args: &mut impl Iterator<Item = String>, name: &str) -> Result<String, String> {
    args.next()
        .ok_or_else(|| format!("--{name} requires a value"))
}

fn help_text() -> String {
    "options: --iterations N --warmups N --backend available|cranelift|llvm|both --format pretty|json|github --json-output PATH --case PATH".to_string()
}
