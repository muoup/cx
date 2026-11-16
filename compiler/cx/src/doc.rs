use std::path::{Path, PathBuf};

use cx_doc::{
    extract::{self, ModuleSource, Sources},
    model::Documentation,
};
use cx_log::CXResult;
use cx_pipeline::pipeline::{FailurePolicy, Pipeline};
use cx_pipeline_data::{
    jobs::CompilationStep,
    package::{PackageId, Packages},
    ArchitectureConfig, CompilationMode, CompilationUnit, CompilerConfig, GlobalCompilationContext,
    OptimizationLevel, UnsafeFlags,
};

use crate::{
    args::{default_backend, DocArgs},
    help::Topic,
    log::error,
    resolve_invocation_path, setup_internal_directory,
};

pub(crate) fn document(args: DocArgs) -> CXResult<()> {
    let invocation_directory = std::env::current_dir()
        .map_err(|err| error(format!("failed to get current directory: {err}"), None))?;
    let requested = resolve_invocation_path(&invocation_directory, args.path.unwrap_or_else(|| ".".into()));
    let path = requested.canonicalize().map_err(|err| {
        error(format!("cannot read '{}': {err}", requested.display()), Some(Topic::Doc))
    })?;

    let (root, files) = match path.is_dir() {
        true => (path.clone(), sources_under(&path)?),
        false => (path.parent().unwrap_or(&path).to_path_buf(), vec![path.clone()]),
    };
    let (packages, package) = packages(&root);
    let units = files
        .iter()
        .filter_map(|file| packages.unit(package, file))
        .collect::<Vec<_>>();

    let context = GlobalCompilationContext::new(CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend: default_backend(),
        optimization_level: OptimizationLevel::O0,
        require_explicit_return: None,
        output: invocation_directory.join("cx-doc-output"),

        module_mode: true,
        record_types: false,
        unsafe_flags: UnsafeFlags::default(),
        verbose: false,
        dump: false,

        packages,
        internal_directory: setup_internal_directory(&invocation_directory)?.join("cx-doc"),
        compilation_mode: CompilationMode::Executable,

        link_entries: vec![],
        native_objects: vec![],
        header_modules: Default::default(),
    });

    let failures = Pipeline::new(&context, CompilationStep::HMIR)
        .roots(units.iter().cloned())
        .policy(FailurePolicy::Continue)
        .run(|_| {})
        .err()
        .unwrap_or_default();
    if !failures.is_empty() {
        for failure in &failures {
            let _ = failure.error.print();
        }
        return Err(error(
            format!("failed to document {} module(s)", failures.len()),
            None,
        ));
    }

    let documentation = Documentation {
        modules: modules(&context, &units, &root),
    };
    let json = documentation.to_json();
    match args.output {
        Some(output) => {
            let output = resolve_invocation_path(&invocation_directory, output);
            std::fs::write(&output, json).map_err(|err| {
                error(format!("failed to write {}: {err}", output.display()), None)
            })?;
        }
        None => println!("{json}"),
    }
    Ok(())
}

fn modules(
    context: &GlobalCompilationContext,
    units: &[CompilationUnit],
    root: &Path,
) -> Vec<cx_doc::model::ModuleDoc> {
    let db = &context.module_db;
    let mut sources = Sources::default();
    let mut modules = Vec::new();
    for unit in units {
        let Some(hmir) = db.hmir.get(unit.namespace()) else {
            continue;
        };
        let Some(tokens) = db.lex_tokens.lock().get(unit.namespace()).cloned() else {
            continue;
        };
        let input = ModuleSource {
            unit: &hmir,
            file: unit.module().as_path(),
            tokens: &tokens,
        };
        let Some(extraction) = extract::module(input, root, &mut sources) else {
            continue;
        };
        for warning in &extraction.warnings {
            eprintln!("warning: {warning}");
        }
        modules.push(extraction.module);
    }
    modules.sort_by(|left, right| left.module.cmp(&right.module));
    modules
}

// The standard library documents itself under its own name; anything else is a standalone package
fn packages(root: &Path) -> (Packages, PackageId) {
    let packages = Packages::new(vec![]);
    match packages.owner(root) {
        Some(id) => (packages, id),
        None => Packages::standalone(root.to_path_buf(), vec![], vec![]),
    }
}

fn sources_under(directory: &Path) -> CXResult<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut pending = vec![directory.to_path_buf()];
    while let Some(directory) = pending.pop() {
        let entries = std::fs::read_dir(&directory).map_err(|err| {
            error(format!("cannot read '{}': {err}", directory.display()), Some(Topic::Doc))
        })?;
        for entry in entries.flatten() {
            let path = entry.path();
            let hidden = entry.file_name().to_string_lossy().starts_with('.');
            if hidden {
                continue;
            }
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|extension| extension == "chl") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}
