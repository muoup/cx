use chl_test_support::{
    assert_stdout, backend_name, build_project, run_command, CompilerBackend, ProjectBuild,
    TestTempDir,
};
use serde::Deserialize;
use std::path::Path;
use std::process::Command;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Spec {
    #[serde(default)]
    build: Vec<Build>,
    #[serde(default)]
    error: Vec<Failure>,
    #[serde(default)]
    run: Vec<Run>,
    #[serde(default)]
    cc: Vec<Cc>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Build {
    #[serde(default)]
    targets: Vec<String>,
    profile: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Failure {
    #[serde(default)]
    targets: Vec<String>,
    profile: Option<String>,
    code: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Run {
    target: String,
    #[serde(default)]
    status: i32,
    stdout: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Cc {
    sources: Vec<String>,
    ffi_lib: String,
    #[serde(default)]
    status: i32,
    stdout: Option<String>,
}

pub(crate) fn run_project_test(root: &Path) {
    let source = std::fs::read_to_string(root.join("expect.toml"))
        .unwrap_or_else(|error| panic!("Failed to read {}/expect.toml: {error}", root.display()));
    let spec: Spec = toml::from_str(&source)
        .unwrap_or_else(|error| panic!("Failed to parse {}/expect.toml: {error}", root.display()));

    let mut failures = Vec::new();
    for backend in backends() {
        if let Err(failure) = run_backend(root, &spec, backend) {
            failures.push(format!("{}: {failure}", backend_name(backend)));
        }
    }
    if !failures.is_empty() {
        panic!("Project failures for {}:\n\n{}", root.display(), failures.join("\n\n"));
    }
}

fn backends() -> Vec<CompilerBackend> {
    let mut backends = vec![CompilerBackend::Cranelift];
    if cfg!(feature = "backend-llvm") {
        backends.push(CompilerBackend::LLVM);
    }
    backends
}

fn run_backend(root: &Path, spec: &Spec, backend: CompilerBackend) -> Result<(), String> {
    let label = format!("project-{}-{}", root.display(), backend_name(backend));
    let temp_dir = TestTempDir::new(&label);

    for failure in &spec.error {
        let attempt = TestTempDir::new(&format!("{label}-error"));
        match build_project(root, &failure.targets, failure.profile.as_deref(), backend, &attempt) {
            Ok(_) => return Err(format!("expected {} but the build succeeded", failure.code)),
            Err(actual) if actual.code != failure.code => {
                return Err(format!("expected {} but got:\n{}", failure.code, actual.rendered));
            }
            Err(_) => {}
        }
    }

    let mut build = None;
    for step in &spec.build {
        build = Some(
            build_project(root, &step.targets, step.profile.as_deref(), backend, &temp_dir)
                .map_err(|failure| format!("build failed:\n{}", failure.rendered))?,
        );
    }
    let build = match (build, spec.run.is_empty() && spec.cc.is_empty()) {
        (Some(build), _) => build,
        (None, true) => return Ok(()),
        (None, false) => return Err("run and cc steps need a [[build]] step".into()),
    };

    for run in &spec.run {
        let executable = build
            .executable(&run.target)
            .ok_or_else(|| format!("'{}' is not an executable of the project", run.target))?;
        let mut command = Command::new(executable);
        command.current_dir(root);
        expect(&format!("'{}'", run.target), &mut command, run.status, run.stdout.as_deref())?;
    }
    for cc in &spec.cc {
        run_cc(root, &build, cc, &temp_dir)?;
    }
    Ok(())
}

fn run_cc(root: &Path, build: &ProjectBuild, cc: &Cc, temp_dir: &TestTempDir) -> Result<(), String> {
    let library = build
        .ffi_lib(&cc.ffi_lib)
        .ok_or_else(|| format!("'{}' is not an ffi-lib of the project", cc.ffi_lib))?;
    let output = temp_dir.path().join("cc.out");
    let mut command = crate::reference_compiler().command();
    command
        .args(cc.sources.iter().map(|source| root.join(source)))
        .arg("-I")
        .arg(&library.headers)
        .arg(&library.object)
        .args(library.links.iter().map(|link| format!("-l{}", link.name)))
        .arg("-o")
        .arg(&output);
    let compilation = run_command(&mut command)?;
    if !compilation.success {
        return Err(format!("the reference compiler rejected {:?}:\n{}", cc.sources, compilation.stderr));
    }
    let mut command = Command::new(&output);
    command.current_dir(root);
    expect(&format!("cc {:?}", cc.sources), &mut command, cc.status, cc.stdout.as_deref())
}

fn expect(label: &str, command: &mut Command, status: i32, stdout: Option<&str>) -> Result<(), String> {
    let execution = run_command(command)?;
    if let Some(stdout) = stdout {
        assert_stdout(stdout, &execution.stdout, label)?;
    }
    match execution.status_code == Some(status) {
        true => Ok(()),
        false => Err(format!(
            "{label} exit status mismatch:\nexpected: {status}\nactual: {:?}\nstderr: {}",
            execution.status_code, execution.stderr
        )),
    }
}
