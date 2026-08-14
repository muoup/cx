use cx_pipeline_data::{CompilerBackend, OptimizationLevel};

#[derive(Debug)]
pub enum Command {
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
    pub output_file: Option<String>,
    pub compile_only: bool,
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub unsafe_mode: bool,
    pub verbose: bool,
}

#[derive(Debug)]
pub struct BuildArgs {
    pub target: Option<String>,
    pub backend: Option<CompilerBackend>,
    pub optimization_level: Option<OptimizationLevel>,
    pub unsafe_mode: bool,
    pub verbose: bool,
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
    output_file: Option<String>,
    compile_only: bool,
}

pub fn print_help() {
    println!("Usage:");
    println!("  cx <file.cx|file.c>... [options]");
    println!("  cx build [target] [options]");
    println!("  cx run [target] [options] [-- args...]");
    println!("  cx init <project-name>");
    println!();
    println!("Commands:");
    println!("  build [target]       Build from cx.toml (all targets or a specific one)");
    println!("  run [target]         Build and run a project binary");
    println!("  init <project-name>  Create a new CX project");
    println!();
    println!("Legacy single-file mode:");
    println!("  <file.cx|file.c>...  Compile source files without using cx.toml");
    println!();
    println!("Options:");
    #[cfg(feature = "backend-llvm")]
    {
        println!("  --backend-llvm       Use the LLVM backend for code generation. (default)");
        println!("  --backend-cranelift  Use the Cranelift backend for code generation.");
    }
    #[cfg(not(feature = "backend-llvm"))]
    {
        println!("  --backend-cranelift  Use the Cranelift backend for code generation (default).");
    }
    println!("  -c                   Compile only; emit an object file.");
    println!("  -I <directory>       Add a header search directory (also accepts -Idir).");
    println!("  -o <output_file>     Specify the output file name.");
    println!("  -O0                  No optimization.");
    println!("  -O1                  Basic optimization.");
    println!("  -O2                  More optimization.");
    println!("  -O3                  Aggressive optimization.");
    println!("  -Osize               Optimize for code size.");
    println!("  -Ofast               Allow fast, but imprecise floating-point optimizations.");
    println!("  --unsafe             Skip MIR invariant validation (liveness still runs).");
    println!("  --verbose            Print each compilation step on its own line.");
    println!("  -h, --help, -help    Display this help message.");
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
        if arg == "-o" {
            rest.push(arg);
            if let Some(path) = args_iter.next() {
                rest.push(path);
            }
            continue;
        }

        match arg.as_str() {
            "-h" | "--help" | "-help" => {
                print_help();
                std::process::exit(0);
            }
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
            _ => rest.push(arg),
        }
    }

    ParsedCommonArgs { common, rest }
}

pub fn parse_args() -> Result<Command, String> {
    let args = std::env::args().skip(1).collect::<Vec<String>>();
    let mut args_iter = args.into_iter();

    let Some(first_arg) = args_iter.next() else {
        print_help();
        std::process::exit(1);
    };

    if first_arg == "build" {
        return parse_build_args(args_iter);
    }

    if first_arg == "run" {
        return parse_run_args(args_iter);
    }

    if first_arg == "init" {
        return parse_init_args(args_iter);
    }

    // Check for flags that might come before the file
    if matches!(first_arg.as_str(), "-h" | "--help" | "-help") {
        print_help();
        std::process::exit(0);
    }

    // Legacy single-file mode
    parse_file_args(std::iter::once(first_arg).chain(args_iter))
}

fn parse_build_args(args: impl IntoIterator<Item = String>) -> Result<Command, String> {
    Ok(Command::Build(parse_build_args_inner(args)?))
}

fn parse_build_args_inner(args: impl IntoIterator<Item = String>) -> Result<BuildArgs, String> {
    let ParsedCommonArgs { common, rest } = parse_common_flags(args);
    let mut target = None;

    for arg in rest {
        match arg.as_str() {
            "-c" => return Err("-c flag is not supported with `cx build`".to_string()),
            "-o" => return Err("-o flag is not supported with `cx build`".to_string()),
            _ => {}
        }

        if arg.starts_with('-') {
            return Err(format!("Unknown flag: {arg}"));
        }

        if target.is_some() {
            return Err("Multiple targets not supported".to_string());
        }
        target = Some(arg);
    }

    Ok(BuildArgs {
        target,
        backend: common.backend,
        optimization_level: common.optimization_level,
        unsafe_mode: common.unsafe_mode,
        verbose: common.verbose,
    })
}

fn parse_run_args(args: impl IntoIterator<Item = String>) -> Result<Command, String> {
    let mut build_args = Vec::new();
    let mut executable_args = Vec::new();
    let mut after_separator = false;

    for arg in args {
        if after_separator {
            executable_args.push(arg);
            continue;
        }

        if arg == "--" {
            after_separator = true;
        } else {
            build_args.push(arg);
        }
    }

    Ok(Command::Run(RunArgs {
        build: parse_build_args_inner(build_args)?,
        executable_args,
    }))
}

fn parse_file_args(args: impl IntoIterator<Item = String>) -> Result<Command, String> {
    let ParsedCommonArgs { common, rest } = parse_common_flags(args);
    let FileSpecificArgs {
        input_files,
        include_dirs,
        output_file,
        compile_only,
    } = parse_file_specific_args(rest)?;

    if input_files.is_empty() {
        return Err("Usage: cx <file.cx|file.c>... [options]".to_string());
    }

    if input_files
        .iter()
        .any(|file| !file.ends_with(".cx") && !file.ends_with(".c"))
    {
        return Err("Input files must have a .cx or .c extension".to_string());
    }

    if compile_only && input_files.len() > 1 && output_file.is_some() {
        return Err("-o flag is not supported with -c and multiple input files".to_string());
    }

    Ok(Command::CompileFile(FileArgs {
        input_files,
        include_dirs,
        output_file,
        compile_only,
        backend: common.backend.unwrap_or_else(default_backend),
        optimization_level: common.optimization_level.unwrap_or_default(),
        unsafe_mode: common.unsafe_mode,
        verbose: common.verbose,
    }))
}

fn parse_file_specific_args(
    args: impl IntoIterator<Item = String>,
) -> Result<FileSpecificArgs, String> {
    let mut parsed = FileSpecificArgs::default();
    let mut args_iter = args.into_iter();

    while let Some(arg) = args_iter.next() {
        if arg == "-c" {
            parsed.compile_only = true;
            continue;
        }

        if arg == "-o" {
            parsed.output_file = Some(
                args_iter
                    .next()
                    .ok_or_else(|| "-o flag requires an output file path".to_string())?,
            );
            continue;
        }

        if arg == "-I" {
            parsed.include_dirs.push(
                args_iter
                    .next()
                    .ok_or_else(|| "-I flag requires a directory path".to_string())?,
            );
            continue;
        }

        if let Some(path) = arg.strip_prefix("-I") {
            if !path.is_empty() {
                parsed.include_dirs.push(path.to_string());
                continue;
            }
        }

        if arg.starts_with('-') {
            return Err(format!("Unknown flag: {arg}"));
        }

        parsed.input_files.push(arg);
    }

    Ok(parsed)
}

fn parse_init_args(args: impl IntoIterator<Item = String>) -> Result<Command, String> {
    let mut args_iter = args.into_iter();
    let project_name = args_iter
        .next()
        .ok_or_else(|| "Usage: cx init <project-name>".to_string())?;

    if project_name.starts_with('-') {
        return Err(format!("Invalid project name: '{project_name}'"));
    }

    if args_iter.next().is_some() {
        return Err("cx init takes exactly one argument: the project name".to_string());
    }

    Ok(Command::Init(InitArgs { project_name }))
}
