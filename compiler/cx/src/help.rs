#[derive(Clone, Copy)]
pub(crate) enum Topic {
    General,
    File,
    Build,
    Run,
    Init,
}

pub(crate) fn dispatch(topic: Topic) {
    match topic {
        Topic::General => print_general_help(),
        Topic::File => print_file_help(),
        Topic::Build => print_build_help(),
        Topic::Run => print_run_help(),
        Topic::Init => print_init_help(),
    }
}

pub(crate) fn print_version() {
    println!("cx 0.0 pre-alpha");
}

pub(crate) fn is_help_flag(flag: &str) -> bool {
    matches!(flag, "-h" | "--help" | "-help")
}

pub(crate) fn is_version_flag(flag: &str) -> bool {
    flag == "--version"
}

fn print_general_help() {
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
    print_common_options();
    println!("  -c                   Compile only; emit an object file.");
    println!("  -I <directory>       Add a header search directory (also accepts -Idir).");
    println!("  -D <name>[=value]    Define a preprocessor macro (also accepts -Dname[=value]).");
    println!("  -o <output_file>     Specify the output file name.");
}

fn print_file_help() {
    println!("Usage: cx <file.cx|file.c>... [options]");
    println!();
    println!("Compile source files without using cx.toml.");
    println!();
    print_common_options();
    println!("  -c                   Compile only; emit an object file.");
    println!("  -I <directory>       Add a header search directory (also accepts -Idir).");
    println!("  -D <name>[=value]    Define a preprocessor macro (also accepts -Dname[=value]).");
    println!("  -o <output_file>     Specify the output file name.");
}

fn print_build_help() {
    println!("Usage: cx build [target] [options]");
    println!();
    println!("Build all project targets, or one target when specified.");
    println!();
    print_common_options();
}

fn print_run_help() {
    println!("Usage: cx run [target] [options] [-- args...]");
    println!();
    println!("Build and run one project binary. Use -- to pass arguments to the binary.");
    println!();
    print_common_options();
}

fn print_init_help() {
    println!("Usage: cx init <project-name>");
    println!();
    println!("Create a new CX project in a directory named project-name.");
}

fn print_common_options() {
    #[cfg(feature = "backend-llvm")]
    {
        println!("  --backend-llvm       Use the LLVM backend for code generation. (default)");
        println!("  --backend-cranelift  Use the Cranelift backend for code generation.");
    }
    #[cfg(not(feature = "backend-llvm"))]
    {
        println!("  --backend-cranelift  Use the Cranelift backend for code generation (default).");
    }
    println!("  -O0                  No optimization.");
    println!("  -O1                  Basic optimization.");
    println!("  -O2                  More optimization.");
    println!("  -O3                  Aggressive optimization.");
    println!("  -Osize               Optimize for code size.");
    println!("  -Ofast               Allow fast, but imprecise floating-point optimizations.");
    println!("  --unsafe             Skip all safety checks for faster builds (use with caution).");
    println!("  --allow-implicit-return  Permit missing returns in non-void functions.");
    println!("  --require-explicit-return  Require explicit returns in non-void functions.");
    println!("  --verbose            Print each compilation step on its own line.");
    println!("  --version            Display the compiler version.");
    println!("  -h, --help, -help    Display this help message.");
}
