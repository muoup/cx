use cx_pipeline_data::{CompilerBackend, OptimizationLevel};

#[derive(Debug)]
pub struct AppArgs {
    pub input_file: String,
    pub output_file: String,
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
}

pub fn print_help() {
    println!("Usage: cx <file> [options]");
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
    println!("  -help                Display this help message.");
}

pub fn parse_args() -> Result<AppArgs, String> {
    let args = std::env::args().collect::<Vec<String>>();
    let mut input_file = None;
    let mut output_file = "a.out".to_string(); // Default output file
    let mut backend = Default::default();
    let mut optimization_level = Default::default();
    
    let mut args_iter = args.iter().skip(1);

    if args_iter.len() == 0 {
        print_help();
        std::process::exit(1);
    }

    while let Some(arg) = args_iter.next() {
        match arg.as_str() {
            "-help" => {
                print_help();
                std::process::exit(0);
            }
            #[cfg(feature = "backend-llvm")]
            "--backend-llvm" => backend = CompilerBackend::LLVM,
            "--backend-cranelift" => backend = CompilerBackend::Cranelift,
            "-O0" => optimization_level = OptimizationLevel::O0,
            "-O1" => optimization_level = OptimizationLevel::O1,
            "-O2" => optimization_level = OptimizationLevel::O2,
            "-O3" => optimization_level = OptimizationLevel::O3,
            "-Osize" => optimization_level = OptimizationLevel::Osize,
            "-Ofast" => optimization_level = OptimizationLevel::Ofast,
            "-o" => {
                if let Some(path) = args_iter.next() {
                    output_file = path.clone();
                } else {
                    return Err("-o flag requires an output file path".to_string());
                }
            }
            _ if arg.starts_with('-') => {
                return Err(format!("Unknown flag: {arg}"));
            }
            _ => {
                if input_file.is_some() {
                    return Err("Multiple input files not current supported".to_string());
                }
                input_file = Some(arg.clone());
            }
        }
    }

    let input_file = input_file.ok_or_else(|| format!("Usage: {} <file> [options]", args[0]))?;

    if !input_file.ends_with(".cx") {
        return Err("Input file must have a .cx extension".to_string());
    }

    Ok(AppArgs {
        input_file,
        output_file,
        backend,
        optimization_level,
    })
}
