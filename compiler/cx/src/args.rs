use cx_pipeline_data::{CompilerBackend, OptimizationLevel};

#[derive(Debug)]
pub enum Command {
    /// Legacy single-file mode: cx <file.cx> [options]
    CompileFile(FileArgs),
    /// Project build mode: cx build [target] [options]
    Build(BuildArgs),
    /// Initialize a new project: cx init <project-name>
    Init(InitArgs),
}

#[derive(Debug)]
pub struct InitArgs {
    pub project_name: String,
}

#[derive(Debug)]
pub struct FileArgs {
    pub input_file: String,
    pub output_file: String,
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub analysis: bool,
    pub verbose: bool,
}

#[derive(Debug)]
pub struct BuildArgs {
    pub target: Option<String>,
    pub backend: Option<CompilerBackend>,
    pub optimization_level: Option<OptimizationLevel>,
    pub analysis: Option<bool>,
    pub verbose: bool,
}

pub fn print_help() {
    println!("Usage: cx <file> [options]");
    println!("       cx build [target] [options]");
    println!();
    println!("Commands:");
    println!("  init <project-name>  Create a new CX project");
    println!("  build [target]       Build project from cx.toml (all targets or a specific one)");
    println!("  [target]             Build a single target without using cx.toml");
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
    println!("  -o <output_file>     Specify the output file name.");
    println!("  -O0                  No optimization.");
    println!("  -O1                  Basic optimization.");
    println!("  -O2                  More optimization.");
    println!("  -O3                  Aggressive optimization.");
    println!("  -Osize               Optimize for code size.");
    println!("  -Ofast               Allow fast, but imprecise floating-point optimizations.");
    println!("  --analysis           Run FMIR analysis for safe functions.");
    println!("  --verbose            Print each compilation step on its own line.");
    println!("  -help                Display this help message.");
}

fn default_backend() -> CompilerBackend {
    #[cfg(feature = "backend-llvm")]
    {
        CompilerBackend::LLVM
    }
    #[cfg(not(feature = "backend-llvm"))]
    {
        CompilerBackend::Cranelift
    }
}

fn parse_common_flag(
    arg: &str,
    args_iter: &mut std::iter::Skip<std::slice::Iter<'_, String>>,
    backend: &mut Option<CompilerBackend>,
    optimization_level: &mut Option<OptimizationLevel>,
    analysis: &mut bool,
    verbose: &mut bool,
    output_file: Option<&mut String>,
) -> Result<bool, String> {
    match arg {
        "-help" => {
            print_help();
            std::process::exit(0);
        }
        #[cfg(feature = "backend-llvm")]
        "--backend-llvm" => *backend = Some(CompilerBackend::LLVM),
        "--backend-cranelift" => *backend = Some(CompilerBackend::Cranelift),
        "-O0" => *optimization_level = Some(OptimizationLevel::O0),
        "-O1" => *optimization_level = Some(OptimizationLevel::O1),
        "-O2" => *optimization_level = Some(OptimizationLevel::O2),
        "-O3" => *optimization_level = Some(OptimizationLevel::O3),
        "-Osize" => *optimization_level = Some(OptimizationLevel::Osize),
        "-Ofast" => *optimization_level = Some(OptimizationLevel::Ofast),
        "--analysis" => *analysis = true,
        "--verbose" => *verbose = true,
        "-o" => {
            if let Some(out) = output_file {
                if let Some(path) = args_iter.next() {
                    *out = path.clone();
                } else {
                    return Err("-o flag requires an output file path".to_string());
                }
            } else {
                return Err("-o flag is not supported with `cx build`".to_string());
            }
        }
        _ => return Ok(false),
    }
    Ok(true)
}

pub fn parse_args() -> Result<Command, String> {
    let args = std::env::args().collect::<Vec<String>>();
    let mut args_iter = args.iter().skip(1);

    if args_iter.len() == 0 {
        print_help();
        std::process::exit(1);
    }

    let first_arg = args_iter.next().unwrap();

    if first_arg == "build" {
        return parse_build_args(&mut args_iter);
    }

    if first_arg == "init" {
        return parse_init_args(&mut args_iter);
    }

    // Check for flags that might come before the file
    if first_arg == "-help" {
        print_help();
        std::process::exit(0);
    }

    // Legacy single-file mode
    parse_file_args(first_arg, &mut args_iter)
}

fn parse_build_args(
    args_iter: &mut std::iter::Skip<std::slice::Iter<'_, String>>,
) -> Result<Command, String> {
    let mut target = None;
    let mut backend = None;
    let mut optimization_level = None;
    let mut analysis = false;
    let mut verbose = false;

    while let Some(arg) = args_iter.next() {
        if parse_common_flag(
            arg.as_str(),
            args_iter,
            &mut backend,
            &mut optimization_level,
            &mut analysis,
            &mut verbose,
            None,
        )? {
            continue;
        }

        if arg.starts_with('-') {
            return Err(format!("Unknown flag: {arg}"));
        }

        if target.is_some() {
            return Err("Multiple targets not supported".to_string());
        }
        target = Some(arg.clone());
    }

    Ok(Command::Build(BuildArgs {
        target,
        backend,
        optimization_level,
        analysis: if analysis { Some(true) } else { None },
        verbose,
    }))
}

fn parse_file_args(
    first_arg: &str,
    args_iter: &mut std::iter::Skip<std::slice::Iter<'_, String>>,
) -> Result<Command, String> {
    let mut output_file = "a.out".to_string();
    let mut backend = None;
    let mut optimization_level = None;
    let mut analysis = false;
    let mut verbose = false;

    // First arg is the input file (unless it's a flag)
    if first_arg.starts_with('-') {
        return Err(format!("Unknown flag: {first_arg}"));
    }
    let mut input_file = Some(first_arg.to_string());

    while let Some(arg) = args_iter.next() {
        if parse_common_flag(
            arg.as_str(),
            args_iter,
            &mut backend,
            &mut optimization_level,
            &mut analysis,
            &mut verbose,
            Some(&mut output_file),
        )? {
            continue;
        }

        if arg.starts_with('-') {
            return Err(format!("Unknown flag: {arg}"));
        }

        if input_file.is_some() {
            return Err("Multiple input files not currently supported".to_string());
        }
        input_file = Some(arg.clone());
    }

    let input_file = input_file.ok_or_else(|| format!("Usage: cx <file> [options]"))?;

    if !input_file.ends_with(".cx") {
        return Err("Input file must have a .cx extension".to_string());
    }

    Ok(Command::CompileFile(FileArgs {
        input_file,
        output_file,
        backend: backend.unwrap_or_else(default_backend),
        optimization_level: optimization_level.unwrap_or_default(),
        analysis,
        verbose,
    }))
}

fn parse_init_args(
    args_iter: &mut std::iter::Skip<std::slice::Iter<'_, String>>,
) -> Result<Command, String> {
    let project_name = args_iter
        .next()
        .ok_or_else(|| "Usage: cx init <project-name>".to_string())?
        .clone();

    if project_name.starts_with('-') {
        return Err(format!("Invalid project name: '{project_name}'"));
    }

    if args_iter.next().is_some() {
        return Err("cx init takes exactly one argument: the project name".to_string());
    }

    Ok(Command::Init(InitArgs { project_name }))
}
