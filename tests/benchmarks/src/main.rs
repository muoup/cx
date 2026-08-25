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

    match options.format {
        OutputFormat::Json => println!(
            "{}",
            serde_json::to_string_pretty(&report).map_err(|error| error.to_string())?
        ),
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

    TimingStats {
        mean_ms: sum / samples_ms.len() as f64,
        median_ms,
        min_ms: samples_ms[0],
        max_ms: samples_ms[samples_ms.len() - 1],
        samples_ms,
    }
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

fn render_pretty_table(report: &BenchmarkReport) -> String {
    let rows = report
        .cases
        .iter()
        .map(|result| BenchmarkTableRow {
            case: result.case.replace('|', "\\|"),
            backend: result.backend.clone(),
            compile: format!("{} ms", format_stats(&result.compile)),
            execute: format!("{} ms", format_stats(&result.execute)),
        })
        .collect::<Vec<_>>();
    let mut table = Table::new(rows);
    table.modify(Columns::one(2), Alignment::right());
    table.modify(Columns::one(3), Alignment::right());
    table.with(Style::rounded());

    format!("\nResults: Benchmarks\n{}\n\n", table)
}

fn render_github_table(report: &BenchmarkReport) -> String {
    let mut output = String::from("## CX benchmarks\n\n");
    output.push_str("| Case | Backend | Compile | Execute |\n");
    output.push_str("| --- | --- | ---: | ---: |\n");

    for result in &report.cases {
        output.push_str(&format!(
            "| {} | {} | {} ms | {} ms |\n",
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
    format!(
        "{:.2} mean ({:.2} median, {:.2}-{:.2})",
        stats.mean_ms, stats.median_ms, stats.min_ms, stats.max_ms
    )
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
    "options: --iterations N --warmups N --backend available|cranelift|llvm|both --format pretty|json|github --case PATH".to_string()
}
