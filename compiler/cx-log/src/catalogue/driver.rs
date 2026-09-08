use super::define_errors;

define_errors! {
    FAILED_TO_EXECUTE_LINKER: String = "D0001" => |arg0| format!("Failed to execute linker: {arg0}");
    RELOCATABLE_LINKING_FAILED: String = "D0002" => |arg0| format!("Relocatable linking failed: {arg0}");
    UNKNOWN_LINK_KIND_FOR_LIBRARY: (String, String) = "D0003" => |(arg0, arg1)| format!("Unknown link kind '{arg0}' for library '{arg1}'");
    LINKING_FAILED: String = "D0004" => |arg0| format!("Linking failed: {arg0}");
    BASE_FILE_PATH_IS_NOT_VALID_UTF_8: () = "D0005" => |()| "Base file path is not valid UTF-8".into();
    FAILED_TO_CREATE_OBJECT_OUTPUT_DIRECTORY: (String, String) = "D0006" => |(arg0, arg1)| format!("Failed to create object output directory {arg0}: {arg1}");
    FAILED_TO_WRITE_OBJECT_FILE: (String, String) = "D0007" => |(arg0, arg1)| format!("Failed to write object file {arg0}: {arg1}");
    NO_SOURCE_FILES_WERE_SELECTED_FOR_COMPILATION: () = "D0008" => |()| "No source files were selected for compilation".into();
    SOURCE_FILE_PATH_IS_NOT_VALID_UTF_8: () = "D0009" => |()| "Source file path is not valid UTF-8".into();
    MULTI_FILE_COMPILATION_ONLY_SUPPORTS_EXECUTABLE_OUTPUT: () = "D0010" => |()| "Multi-file compilation only supports executable output".into();
    CX_TOML_HAS_NO_WORKSPACE_SECTION: () = "D0011" => |()| "cx.toml has no [workspace] section".into();
    TARGET_NOT_FOUND_IN_CX_TOML: String = "D0012" => |arg0| format!("Target '{arg0}' not found in cx.toml");
    FAILED_TO_CREATE_OUTPUT_DIRECTORY: (String, String) = "D0013" => |(arg0, arg1)| format!("Failed to create output directory {arg0}: {arg1}");
    BINARY_MUST_DEFINE_ENTRY_OR_MATCH: String = "D0014" => |arg0| format!("Binary '{arg0}' must define 'entry' or 'match'");
    FAILED_TO_WRITE_HEADER: (String, String) = "D0015" => |(arg0, arg1)| format!("Failed to write header {arg0}: {arg1}");
    IMPORT_IS_NOT_AVAILABLE_IN_SINGLE_FILE_COMPILATION: String = "D0016" => |arg0| format!("Import '{arg0}' is not available in single-file compilation mode. Only compiler library modules under `std::` may be imported here; use `cx build` for project/module imports.");
    FAILED_TO_CREATE_DUMP_DIRECTORY: (String, String) = "D0017" => |(arg0, arg1)| format!("Failed to create dump directory {arg0}: {arg1}");
    FAILED_TO_CREATE_DUMP_FILE: (String, String) = "D0018" => |(arg0, arg1)| format!("Failed to create dump file {arg0}: {arg1}");
    FAILED_TO_READ: (String, String) = "D0019" => |(arg0, arg1)| format!("Failed to read {arg0}: {arg1}");
    DUPLICATE_MODULE_NAMESPACE_FOUND_DURING_DECOMPOSITION: String = "D0020" => |arg0| format!("Duplicate module namespace found during decomposition: {arg0}");
    FAILED_TO_CREATE_OBJECT_DIRECTORY: (String, String) = "D0021" => |(arg0, arg1)| format!("Failed to create object directory '{arg0}': {arg1}");
    INTERNAL_DIRECTORY_PATH_IS_NOT_VALID_UTF_8: () = "D0022" => |()| "Internal directory path is not valid UTF-8".into();
    EMPTY_MATCH_PATTERN: () = "D0023" => |()| "match pattern cannot be empty".into();
    ABSOLUTE_MATCH_PATTERN: String = "D0024" => |pattern| format!("match pattern must be relative: {pattern}");
    NO_MATCHED_SOURCES: String = "D0025" => |patterns| format!("match patterns selected no source files: {patterns}");
    READ_MATCH_DIRECTORY: (String, String) = "D0026" => |(path, error)| format!("failed to read match directory {path}: {error}");
    READ_MATCH_ENTRY: String = "D0027" => |error| format!("failed to read match entry: {error}");
    RELATIVE_MATCH_PATH: (String, String, String) = "D0028" => |(path, base, error)| format!("failed to make {path} relative to {base}: {error}");
}
