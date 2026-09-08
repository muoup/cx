use crate::{
    args::{self, Command},
    help::Topic,
};

fn parse(args: &[&str]) -> Command {
    args::parse_args(args.iter().map(|arg| (*arg).to_owned()))
        .unwrap_or_else(|error| panic!("{}", error.message()))
}

#[test]
fn malformed_commands_are_codeless_and_show_their_usage() {
    for (args, usage) in [
        (vec!["run", "-c"], "Usage: cx run [target]"),
        (vec!["build", "--unknown"], "Usage: cx build [target]"),
        (vec!["init"], "Usage: cx init <project-name>"),
        (vec!["main.cx", "-o"], "Usage: cx <file.cx|file.c>"),
    ] {
        let error = args::parse_args(args.into_iter().map(str::to_owned)).unwrap_err();
        assert!(error.code().is_empty());
        let mut output = Vec::new();
        error.output(&mut output).unwrap();
        let output = String::from_utf8(output).unwrap();
        assert!(output.starts_with("error: "), "{output}");
        assert!(output.contains(usage), "{output}");
        assert!(output.contains("--help"), "{output}");
        assert!(error.source_span().is_none());
    }
}

#[test]
fn help_and_version_are_successful_parse_outcomes() {
    assert!(matches!(parse(&["--help"]), Command::Help(Topic::General)));
    assert!(matches!(
        parse(&["run", "--help"]),
        Command::Help(Topic::Run)
    ));
    assert!(matches!(
        parse(&["init", "name", "--help"]),
        Command::Help(Topic::Init)
    ));
    assert!(matches!(parse(&["build", "--version"]), Command::Version));
}

#[test]
fn option_operands_are_not_interpreted_as_common_flags() {
    let Command::CompileFile(args) = parse(&[
        "main.cx",
        "-I",
        "--help",
        "-o",
        "--verbose",
        "-D",
        "DEBUG",
        "--dump",
    ]) else {
        panic!("expected file compilation");
    };
    assert_eq!(args.include_dirs, ["--help"]);
    assert_eq!(args.output_file.as_deref(), Some("--verbose"));
    assert_eq!(
        args.predefined_macros,
        [("DEBUG".to_owned(), "1".to_owned())]
    );
    assert!(!args.verbose);
    assert!(args.dump);
}

#[test]
fn run_forwards_all_arguments_after_separator() {
    let Command::Run(args) = parse(&["run", "app", "--", "--help", "-c", "--", "argument"]) else {
        panic!("expected run command");
    };
    assert_eq!(args.build.target.as_deref(), Some("app"));
    assert_eq!(args.executable_args, ["--help", "-c", "--", "argument"]);
}

#[test]
fn file_flags_accept_attached_and_separate_values() {
    let Command::CompileFile(args) = parse(&[
        "one.cx",
        "two.c",
        "-Iinclude",
        "-I",
        "headers",
        "-DANSWER=42",
        "-D",
        "DEBUG",
        "-c",
    ]) else {
        panic!("expected file compilation");
    };
    assert_eq!(args.input_files, ["one.cx", "two.c"]);
    assert_eq!(args.include_dirs, ["include", "headers"]);
    assert_eq!(
        args.predefined_macros,
        [
            ("ANSWER".to_owned(), "42".to_owned()),
            ("DEBUG".to_owned(), "1".to_owned())
        ]
    );
    assert!(args.compile_only);
}
