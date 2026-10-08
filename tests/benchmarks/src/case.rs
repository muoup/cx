use cx_test_support::{expected_stdout, CompilerBackend, ProjectBinary};
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};

/// A program the benchmark builds, and the runs of it that are timed.
pub struct Case {
    pub label: String,
    pub sources: Sources,
    /// The cx backends able to build the case.
    pub backends: Vec<CompilerBackend>,
    pub workloads: Vec<Workload>,
}

pub enum Sources {
    File(PathBuf),
    /// Without `link`, the sources are compiled to objects and no binary is produced.
    Project { binary: ProjectBinary, link: bool },
}

pub struct Workload {
    /// Tells the workload from the others of its case; a case with a single unnamed workload is
    /// reported on one row.
    pub label: Option<String>,
    pub arguments: Vec<OsString>,
    pub working_directory: PathBuf,
    pub expected_stdout: String,
}

/// A `cx.toml` project that is benchmarked where it lives in the repository.
struct Project {
    name: &'static str,
    /// Relative to the repository root.
    root: &'static str,
    binary: &'static str,
    backends: &'static [CompilerBackend],
    /// Sources of the binary, relative to the project, that the benchmark leaves out.
    skipped_sources: &'static [&'static str],
    run: Run,
}

enum Run {
    /// The sources are compiled and never linked.
    CompileOnly,
    /// Run without arguments. `expectation`, relative to the project, carries the expected
    /// stdout.
    Alone { expectation: &'static str },
    /// Run once for each `workloads/<name>/*.<extension>`, passed as the only argument and
    /// checked against its `.cx-output` sidecar.
    Scripts { extension: &'static str },
}

const PROJECTS: &[Project] = &[
    Project {
        name: "lua",
        root: "examples/c-parity/lua",
        binary: "lua",
        // The interpreter loop takes the addresses of labels, which Cranelift lowering lacks.
        backends: &[CompilerBackend::LLVM],
        skipped_sources: &[],
        // TODO: The cx-built interpreter fails upstream's `testes/all.lua` at calls.lua:541
        // (binary chunks). Once that passes, run it here as a workload too.
        run: Run::Scripts { extension: "lua" },
    },
    Project {
        name: "doomgeneric",
        root: "examples/c-parity/doomgeneric",
        binary: "doomgeneric",
        // Doom defines variadic functions, which Cranelift lowering lacks.
        backends: &[CompilerBackend::LLVM],
        // The raylib adapter needs headers that only exist once examples/build-raylib.sh has run.
        skipped_sources: &["doomgeneric_raylib.c"],
        run: Run::CompileOnly,
    },
    Project {
        name: "zlib",
        root: "examples/c-parity/zlib",
        binary: "roundtrip",
        backends: &[CompilerBackend::Cranelift, CompilerBackend::LLVM],
        skipped_sources: &[],
        run: Run::Alone {
            expectation: "roundtrip.c",
        },
    },
];

pub fn project_names() -> impl Iterator<Item = &'static str> {
    PROJECTS.iter().map(|project| project.name)
}

/// Every case of the suite: the single-file fixtures, then the projects.
pub fn all_cases() -> Result<Vec<Case>, String> {
    let mut files = Vec::new();
    discover_files(&manifest_directory().join("fixtures"), &mut files)?;
    files.sort();

    let mut cases = files.iter().map(|file| file_case(file)).collect::<Result<Vec<_>, _>>()?;
    for project in PROJECTS {
        cases.push(project_case(project)?);
    }
    Ok(cases)
}

/// The case `selection` names: a project, or the path of a source file.
pub fn selected_case(selection: &str, invocation_directory: &Path) -> Result<Case, String> {
    match PROJECTS.iter().find(|project| project.name == selection) {
        Some(project) => project_case(project),
        None => file_case(&invocation_directory.join(selection)),
    }
}

fn file_case(input: &Path) -> Result<Case, String> {
    let expected_stdout = expected_stdout(input)
        .map_err(|error| error.to_string())?
        .ok_or_else(|| {
            format!(
                "{} has no inline or sidecar stdout expectation",
                input.display()
            )
        })?;
    let working_directory = input
        .parent()
        .ok_or_else(|| format!("{} has no working directory", input.display()))?;

    Ok(Case {
        label: input
            .strip_prefix(manifest_directory())
            .unwrap_or(input)
            .display()
            .to_string(),
        sources: Sources::File(input.to_path_buf()),
        backends: vec![CompilerBackend::Cranelift, CompilerBackend::LLVM],
        workloads: vec![Workload {
            label: None,
            arguments: Vec::new(),
            working_directory: working_directory.to_path_buf(),
            expected_stdout,
        }],
    })
}

fn project_case(project: &Project) -> Result<Case, String> {
    let root = manifest_directory().join("../..").join(project.root);
    let root = fs::canonicalize(&root)
        .map_err(|error| format!("failed to find {}: {error}", root.display()))?;
    let mut binary = ProjectBinary::load(&root, project.binary).map_err(|error| {
        format!(
            "failed to load the {} benchmark: {error}\nif {} has a submodule, check it out with `git submodule update --init {}`",
            project.name, project.root, project.root
        )
    })?;
    binary
        .sources
        .retain(|source| !project.skipped_sources.iter().any(|skipped| source == Path::new(skipped)));

    let workloads = match project.run {
        Run::CompileOnly => Vec::new(),
        Run::Alone { expectation } => {
            let expectation = root.join(expectation);
            vec![Workload {
                label: None,
                arguments: Vec::new(),
                working_directory: root.clone(),
                expected_stdout: expected_stdout(&expectation)
                    .map_err(|error| error.to_string())?
                    .ok_or_else(|| {
                        format!("{} has no stdout expectation", expectation.display())
                    })?,
            }]
        }
        Run::Scripts { extension } => script_workloads(project.name, extension)?,
    };

    Ok(Case {
        label: project.name.to_string(),
        sources: Sources::Project {
            binary,
            link: !matches!(project.run, Run::CompileOnly),
        },
        backends: project.backends.to_vec(),
        workloads,
    })
}

fn script_workloads(project: &str, extension: &str) -> Result<Vec<Workload>, String> {
    let directory = manifest_directory().join("workloads").join(project);
    let mut scripts = read_directory(&directory)?
        .into_iter()
        .filter(|path| path.extension().is_some_and(|found| found == extension))
        .collect::<Vec<_>>();
    scripts.sort();
    if scripts.is_empty() {
        return Err(format!("{} has no .{extension} workloads", directory.display()));
    }

    scripts
        .into_iter()
        .map(|script| {
            let sidecar = script.with_extension("cx-output");
            let expected_stdout = fs::read_to_string(&sidecar)
                .map_err(|error| format!("failed to read {}: {error}", sidecar.display()))?;
            let name = script.file_name().expect("a script has a file name");

            Ok(Workload {
                label: Some(name.to_string_lossy().into_owned()),
                arguments: vec![name.to_os_string()],
                working_directory: directory.clone(),
                expected_stdout,
            })
        })
        .collect()
}

fn discover_files(root: &Path, files: &mut Vec<PathBuf>) -> Result<(), String> {
    for path in read_directory(root)? {
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default();

        if name.starts_with('_') {
            continue;
        }
        if path.is_dir() {
            discover_files(&path, files)?;
            continue;
        }
        if matches!(
            path.extension().and_then(|extension| extension.to_str()),
            Some("cx") | Some("c")
        ) {
            files.push(path);
        }
    }

    Ok(())
}

fn read_directory(directory: &Path) -> Result<Vec<PathBuf>, String> {
    let entries = fs::read_dir(directory).map_err(|error| {
        format!(
            "failed to read benchmark directory {}: {error}",
            directory.display()
        )
    })?;

    entries
        .map(|entry| {
            entry
                .map(|entry| entry.path())
                .map_err(|error| format!("failed to read benchmark entry: {error}"))
        })
        .collect()
}

fn manifest_directory() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}
