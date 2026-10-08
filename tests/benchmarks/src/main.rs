mod case;
mod report;
mod toolchain;

use case::{all_cases, project_names, selected_case, Case, Workload};
use cx_test_support::{assert_stdout, run_command, CompilerBackend, TestTempDir};
use report::{
    render_github_table, render_pretty_table, BenchmarkReport, BenchmarkResult, ReferenceCompiler,
    TimingStats,
};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;
use toolchain::Toolchain;

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
    reference: Option<String>,
    format: OutputFormat,
    json_output: Option<PathBuf>,
    cases: Vec<String>,
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
    let invocation_directory = env::current_dir().map_err(|error| error.to_string())?;
    let cases = if options.cases.is_empty() {
        all_cases()?
    } else {
        options
            .cases
            .iter()
            .map(|selection| selected_case(selection, &invocation_directory))
            .collect::<Result<Vec<_>, _>>()?
    };

    if cases.is_empty() {
        return Err("no benchmark cases were found".to_string());
    }

    let reference = options
        .reference
        .as_deref()
        .map(ReferenceCompiler::detect)
        .transpose()?;
    let mut toolchains = select_backends(options.backend.as_deref())?
        .into_iter()
        .map(Toolchain::Cx)
        .collect::<Vec<_>>();
    toolchains.extend(reference.as_ref().map(Toolchain::Reference));

    let mut results = Vec::new();
    for case in &cases {
        let mut rows = toolchains
            .iter()
            .filter(|toolchain| toolchain.builds(case))
            .map(|toolchain| {
                benchmark_case(case, *toolchain, options.iterations, options.warmups)
                    .map(Vec::into_iter)
            })
            .collect::<Result<Vec<_>, _>>()?;

        // Every toolchain reports the same rows for a case, which are grouped here by row
        // rather than by toolchain.
        while let Some(row) = rows.iter_mut().map(Iterator::next).collect::<Option<Vec<_>>>() {
            if row.is_empty() {
                break;
            }
            results.extend(row);
        }
    }

    let report = BenchmarkReport {
        schema: 2,
        commit: env::var("GITHUB_SHA").ok(),
        reference,
        cases: results,
    };
    let serialized_report =
        serde_json::to_string_pretty(&report).map_err(|error| error.to_string())?;

    if let Some(path) = options.json_output {
        let path = invocation_directory.join(path);
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

/// Times `iterations` builds of the case, each in a fresh directory, then the workloads of the
/// case against the last of those builds.
fn benchmark_case(
    case: &Case,
    toolchain: Toolchain,
    iterations: usize,
    warmups: usize,
) -> Result<Vec<BenchmarkResult>, String> {
    let toolchain_label = toolchain.label();
    let name = format!("{} ({toolchain_label})", case.label);
    let mut compile_samples = Vec::with_capacity(iterations);
    let mut built = None;

    for iteration in 0..iterations {
        let temp = TestTempDir::new(&format!("benchmark-{name}-{iteration}"));
        let build = toolchain
            .build(case, &temp)
            .map_err(|error| format!("{name} compilation failed:\n{error}"))?;
        compile_samples.push(duration_ms(build.elapsed));
        built = Some((temp, build.output));
    }

    let (_temp, binary) = built.expect("a benchmark runs at least one iteration");
    let result = |case: String, compile, execute| BenchmarkResult {
        case,
        backend: toolchain_label.clone(),
        reference: toolchain.is_reference(),
        compile,
        execute,
    };
    let compile = Some(TimingStats::new(compile_samples));

    if let [workload @ Workload { label: None, .. }] = case.workloads.as_slice() {
        let execute = time_workload(&name, &binary, workload, iterations, warmups)?;
        return Ok(vec![result(case.label.clone(), compile, Some(execute))]);
    }

    let mut results = vec![result(case.label.clone(), compile, None)];
    for workload in &case.workloads {
        let label = match &workload.label {
            Some(label) => format!("{}: {label}", case.label),
            None => case.label.clone(),
        };
        let name = format!("{label} ({toolchain_label})");
        let execute = time_workload(&name, &binary, workload, iterations, warmups)?;
        results.push(result(label, None, Some(execute)));
    }
    Ok(results)
}

fn time_workload(
    name: &str,
    binary: &Path,
    workload: &Workload,
    iterations: usize,
    warmups: usize,
) -> Result<TimingStats, String> {
    let mut samples = Vec::with_capacity(iterations);

    for run in 0..warmups + iterations {
        let execution = run_command(
            Command::new(binary)
                .args(&workload.arguments)
                .current_dir(&workload.working_directory),
        )?;
        if !execution.success {
            return Err(format!(
                "{name} exited with {:?}:\n{}",
                execution.status_code, execution.stderr
            ));
        }
        assert_stdout(&workload.expected_stdout, &execution.stdout, name)?;

        if run >= warmups {
            samples.push(duration_ms(execution.elapsed));
        }
    }

    Ok(TimingStats::new(samples))
}

fn duration_ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
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
        reference: None,
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
            "--reference" => options.reference = Some(next_value(&mut args, "reference")?),
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
            "--case" => options.cases.push(next_value(&mut args, "case")?),
            "--help" | "-h" => return Err(help_text()),
            other => return Err(format!("unknown option: {other}")),
        }
    }

    if options.iterations == 0 {
        return Err("iterations must be greater than zero".to_string());
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
    format!(
        "options: --iterations N --warmups N --backend available|cranelift|llvm|both --reference COMMAND --format pretty|json|github --json-output PATH --case PATH|{}",
        project_names().collect::<Vec<_>>().join("|")
    )
}
