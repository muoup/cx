use crate::log::error;
use cx_log::CXResult;

use cx_pipeline_data::{CompilerBackend, OptimizationLevel};

use crate::help::{self, Topic};

#[derive(Debug)]
pub enum Command {
    Help(Topic),
    Version,
    /// Legacy single-file mode: cx <file.cx> [options]
    CompileFile(FileArgs),
    /// Project build mode: cx build [target] [options]
    Build(BuildArgs),
    /// Project run mode: cx run [target] [options] [-- args...]
    Run(RunArgs),
    /// Initialize a new project: cx init <project-name>
    Init(InitArgs),
}

#[derive(Debug)]
pub struct InitArgs {
    pub project_name: String,
}

#[derive(Debug)]
pub struct FileArgs {
    pub input_files: Vec<String>,
    pub include_dirs: Vec<String>,
    pub predefined_macros: Vec<(String, String)>,
    pub output_file: Option<String>,
    pub compile_only: bool,
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub unsafe_mode: bool,
    pub verbose: bool,
    pub dump: bool,
    pub require_explicit_return: Option<bool>,
}

#[derive(Debug)]
pub struct BuildArgs {
    pub target: Option<String>,
    pub backend: Option<CompilerBackend>,
    pub optimization_level: Option<OptimizationLevel>,
    pub unsafe_mode: bool,
    pub verbose: bool,
    pub dump: bool,
    pub require_explicit_return: Option<bool>,
}

#[derive(Debug)]
pub struct RunArgs {
    pub build: BuildArgs,
    pub executable_args: Vec<String>,
}

#[derive(Debug, Default)]
struct CommonArgs {
    backend: Option<CompilerBackend>,
    optimization_level: Option<OptimizationLevel>,
    unsafe_mode: bool,
    verbose: bool,
    dump: bool,
    require_explicit_return: Option<bool>,
}

#[derive(Debug)]
struct ParsedCommonArgs {
    common: CommonArgs,
    rest: Vec<String>,
}

#[derive(Debug, Default)]
struct FileSpecificArgs {
    input_files: Vec<String>,
    include_dirs: Vec<String>,
    predefined_macros: Vec<(String, String)>,
    output_file: Option<String>,
    compile_only: bool,
}

pub(crate) fn default_backend() -> CompilerBackend {
    #[cfg(feature = "backend-llvm")]
    {
        CompilerBackend::LLVM
    }
    #[cfg(not(feature = "backend-llvm"))]
    {
        CompilerBackend::Cranelift
    }
}

pub(crate) fn default_backend_name() -> &'static str {
    match default_backend() {
        CompilerBackend::LLVM => "llvm",
        CompilerBackend::Cranelift => "cranelift",
    }
}

fn parse_common_flags(args: impl IntoIterator<Item = String>) -> ParsedCommonArgs {
    let mut common = CommonArgs::default();
    let mut rest = Vec::new();
    let mut args_iter = args.into_iter();

    while let Some(arg) = args_iter.next() {
        if matches!(arg.as_str(), "-o" | "-I" | "-D") {
            rest.push(arg);
            if let Some(path) = args_iter.next() {
                rest.push(path);
            }
            continue;
        }

        match arg.as_str() {
            #[cfg(feature = "backend-llvm")]
            "--backend-llvm" => common.backend = Some(CompilerBackend::LLVM),
            "--backend-cranelift" => common.backend = Some(CompilerBackend::Cranelift),
            "-O0" => common.optimization_level = Some(OptimizationLevel::O0),
            "-O1" => common.optimization_level = Some(OptimizationLevel::O1),
            "-O2" => common.optimization_level = Some(OptimizationLevel::O2),
            "-O3" => common.optimization_level = Some(OptimizationLevel::O3),
            "-Osize" => common.optimization_level = Some(OptimizationLevel::Osize),
            "-Ofast" => common.optimization_level = Some(OptimizationLevel::Ofast),
            "--unsafe" => common.unsafe_mode = true,
            "--verbose" => common.verbose = true,
            "--dump" => common.dump = true,
            "--allow-implicit-return" => common.require_explicit_return = Some(false),
            "--require-explicit-return" => common.require_explicit_return = Some(true),
            _ => rest.push(arg),
        }
    }

    ParsedCommonArgs { common, rest }
}

pub fn parse_args(args: impl IntoIterator<Item = String>) -> CXResult<Command> {
    let mut args_iter = args.into_iter();
    let Some(first_arg) = args_iter.next() else {
        return Err(error(
            "expected a command or source file",
            Some(Topic::General),
        ));
    };
    let (topic, mut args) = match first_arg.as_str() {
        "build" => (Topic::Build, args_iter.collect::<Vec<_>>()),
        "run" => (Topic::Run, args_iter.collect::<Vec<_>>()),
        "init" => (Topic::Init, args_iter.collect::<Vec<_>>()),
        _ if help::is_help_flag(&first_arg) => return Ok(Command::Help(Topic::General)),
        _ if help::is_version_flag(&first_arg) => return Ok(Command::Version),
        _ => (
            Topic::File,
            std::iter::once(first_arg).chain(args_iter).collect(),
        ),
    };
    let executable_args = if topic == Topic::Run {
        if let Some(separator) = args.iter().position(|arg| arg == "--") {
            let executable_args = args.split_off(separator + 1);
            args.pop();
            executable_args
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };
    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        if topic == Topic::File && matches!(arg.as_str(), "-o" | "-I" | "-D") {
            iter.next();
        } else if help::is_help_flag(arg) {
            return Ok(Command::Help(topic));
        } else if help::is_version_flag(arg) {
            return Ok(Command::Version);
        }
    }
    match topic {
        Topic::Build => Ok(Command::Build(parse_build_args(args, topic)?)),
        Topic::Run => Ok(Command::Run(RunArgs {
            build: parse_build_args(args, topic)?,
            executable_args,
        })),
        Topic::Init => parse_init_args(args),
        Topic::File | Topic::General => parse_file_args(args),
    }
}

fn parse_build_args(args: impl IntoIterator<Item = String>, topic: Topic) -> CXResult<BuildArgs> {
    let ParsedCommonArgs { common, rest } = parse_common_flags(args);
    let mut target = None;

    for arg in rest {
        match arg.as_str() {
            "-c" | "-o" => {
                return Err(error(
                    format!("option '{arg}' is not supported by '{}'", help::name(topic)),
                    Some(topic),
                ))
            }
            _ => {}
        }

        if arg.starts_with('-') {
            return Err(error(format!("unknown option '{arg}'"), Some(topic)));
        }

        if target.is_some() {
            return Err(error("expected at most one target", Some(topic)));
        }
        target = Some(arg);
    }

    Ok(BuildArgs {
        target,
        backend: common.backend,
        optimization_level: common.optimization_level,
        unsafe_mode: common.unsafe_mode,
        verbose: common.verbose,
        dump: common.dump,
        require_explicit_return: common.require_explicit_return,
    })
}

fn parse_file_args(args: impl IntoIterator<Item = String>) -> CXResult<Command> {
    let ParsedCommonArgs { common, rest } = parse_common_flags(args);
    let FileSpecificArgs {
        input_files,
        include_dirs,
        predefined_macros,
        output_file,
        compile_only,
    } = parse_file_specific_args(rest)?;

    if input_files.is_empty() {
        return Err(error(
            "expected at least one source file",
            Some(Topic::File),
        ));
    }

    if input_files
        .iter()
        .any(|file| !file.ends_with(".cx") && !file.ends_with(".c"))
    {
        return Err(error(
            "input files must have a .cx or .c extension",
            Some(Topic::File),
        ));
    }

    if compile_only && input_files.len() > 1 && output_file.is_some() {
        return Err(error(
            "option '-o' cannot be used with '-c' and multiple input files",
            Some(Topic::File),
        ));
    }

    Ok(Command::CompileFile(FileArgs {
        input_files,
        include_dirs,
        predefined_macros,
        output_file,
        compile_only,
        backend: common.backend.unwrap_or_else(default_backend),
        optimization_level: common.optimization_level.unwrap_or_default(),
        unsafe_mode: common.unsafe_mode,
        verbose: common.verbose,
        dump: common.dump,
        require_explicit_return: common.require_explicit_return,
    }))
}

fn parse_file_specific_args(args: impl IntoIterator<Item = String>) -> CXResult<FileSpecificArgs> {
    let mut parsed = FileSpecificArgs::default();
    let mut args_iter = args.into_iter();

    while let Some(arg) = args_iter.next() {
        if arg == "-c" {
            parsed.compile_only = true;
            continue;
        }

        if arg == "-o" {
            parsed.output_file = Some(args_iter.next().ok_or_else(|| {
                error(
                    "option '-o' requires an output file path",
                    Some(Topic::File),
                )
            })?);
            continue;
        }

        if arg == "-I" {
            parsed.include_dirs.push(args_iter.next().ok_or_else(|| {
                error("option '-I' requires a directory path", Some(Topic::File))
            })?);
            continue;
        }

        if let Some(path) = arg.strip_prefix("-I") {
            if !path.is_empty() {
                parsed.include_dirs.push(path.to_string());
                continue;
            }
        }

        if arg == "-D" {
            let definition = args_iter.next().ok_or_else(|| {
                error("option '-D' requires a macro definition", Some(Topic::File))
            })?;
            parsed
                .predefined_macros
                .push(parse_macro_definition(&definition)?);
            continue;
        }

        if let Some(definition) = arg.strip_prefix("-D") {
            if !definition.is_empty() {
                parsed
                    .predefined_macros
                    .push(parse_macro_definition(definition)?);
                continue;
            }
        }

        if arg.starts_with('-') {
            return Err(error(format!("unknown option '{arg}'"), Some(Topic::File)));
        }

        parsed.input_files.push(arg);
    }

    Ok(parsed)
}

fn parse_macro_definition(definition: &str) -> CXResult<(String, String)> {
    let (name, value) = definition
        .split_once('=')
        .map_or((definition, "1"), |(name, value)| (name, value));

    let mut characters = name.chars();
    let valid_start = characters
        .next()
        .is_some_and(|character| character == '_' || character.is_ascii_alphabetic());
    let valid_rest =
        characters.all(|character| character == '_' || character.is_ascii_alphanumeric());

    if !valid_start || !valid_rest {
        return Err(error(
            format!("invalid macro name in -D definition: '{name}'"),
            Some(Topic::File),
        ));
    }

    Ok((name.to_string(), value.to_string()))
}

fn parse_init_args(args: impl IntoIterator<Item = String>) -> CXResult<Command> {
    let mut args_iter = args.into_iter();
    let project_name = args_iter
        .next()
        .ok_or_else(|| error("expected a project name", Some(Topic::Init)))?;

    if project_name.starts_with('-') {
        return Err(error(
            format!("invalid project name: '{project_name}'"),
            Some(Topic::Init),
        ));
    }

    if args_iter.next().is_some() {
        return Err(error(
            "expected exactly one project name",
            Some(Topic::Init),
        ));
    }

    Ok(Command::Init(InitArgs { project_name }))
}
