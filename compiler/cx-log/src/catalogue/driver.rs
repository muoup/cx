use super::define_errors;

define_errors! {
    LINKER_EXECUTION: String = "D0001" => |error| format!("Failed to execute linker: {error}");
    LINKING_FAILED: (String, String) = "D0002" => |(kind, error)| format!("{kind} linking failed: {error}");
    UNKNOWN_LINK_KIND: (String, String) = "D0003" => |(kind, library)| format!("Unknown link kind '{kind}' for library '{library}'");
    PATH_ENCODING: String = "D0004" => |kind| format!("{kind} path is not valid UTF-8");
    FILE_OPERATION: (String, String, Option<String>, String) = "D0005" => |(operation, resource, path, error)| {
        let mut message = format!("Failed to {operation} {resource}");
        if let Some(path) = path {
            message.push_str(&format!(" '{path}'"));
        }
        message.push_str(&format!(": {error}"));
        message
    };
    NO_SOURCES: Option<String> = "D0006" => |patterns| {
        let mut message = "No source files were selected for compilation".to_owned();
        if let Some(patterns) = patterns {
            message.push_str(&format!(" by match patterns: {patterns}"));
        }
        message
    };
    UNSUPPORTED_FEATURE: (String, String) = "D0007" => |(feature, context)| format!("{feature} is not supported in {context}");
    MISSING_CONFIG: (String, String) = "D0008" => |(item, context)| format!("Missing {item} in {context}");
    BINARY_SOURCES: String = "D0009" => |name| format!("Binary '{name}' must define 'entry' or 'match'");
    SINGLE_FILE_IMPORT: String = "D0010" => |name| format!("Import '{name}' is not available in single-file compilation mode. Only compiler library modules under `std::` may be imported here; use `cx build` for project/module imports.");
    DUPLICATE_NAMESPACE: String = "D0011" => |namespace| format!("Duplicate module namespace found during decomposition: {namespace}");
    EMPTY_MATCH_PATTERN: () = "D0012" => |()| "Match pattern cannot be empty".into();
    ABSOLUTE_MATCH_PATTERN: String = "D0013" => |pattern| format!("Match pattern must be relative: {pattern}");
    RELATIVE_MATCH_PATH: (String, String, String) = "D0014" => |(path, base, error)| format!("Failed to make {path} relative to {base}: {error}");
}
