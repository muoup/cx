use super::define_errors;

define_errors! {
    EXPECTED_SYNTAX: (String, Option<String>, Option<String>) = "P0001" => |(expected, context, found)| {
        let mut message = format!("Expected {expected}");
        if let Some(context) = context {
            message.push_str(&format!(" {context}"));
        }
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    UNEXPECTED_TOKEN: (Option<String>, Option<String>) = "P0002" => |(token, context)| {
        let mut message = "Unexpected token".to_owned();
        if let Some(token) = token {
            message.push_str(&format!(" '{token}'"));
        }
        if let Some(context) = context {
            message.push_str(&format!(" in {context}"));
        }
        message
    };
    UNEXPECTED_END: Option<String> = "P0003" => |context| {
        let mut message = "Unexpected end of input".to_owned();
        if let Some(context) = context {
            message.push_str(&format!(" in {context}"));
        }
        message
    };
    UNCLOSED_SYNTAX: String = "P0004" => |construct| format!("Unclosed {construct}");
    INVALID_LITERAL: (String, Option<String>) = "P0005" => |(kind, value)| {
        let mut message = format!("Invalid {kind} literal");
        if let Some(value) = value {
            message.push_str(&format!(": {value}"));
        }
        message
    };
    UNKNOWN_NAME: (String, String) = "P0006" => |(kind, name)| format!("Unknown {kind} '{name}'");
    AMBIGUOUS_LOOKUP: (String, String) = "P0007" => |(name, candidates)| format!("Ambiguous identifier '{name}', candidates: {candidates}");
    DUPLICATE_ITEM: (String, String) = "P0008" => |(item, context)| format!("Duplicate {item} in {context}");
    INVALID_CONTEXT: (String, String) = "P0009" => |(feature, context)| format!("{feature} cannot be used in {context}");
    REQUIRED_CONTEXT: (String, String) = "P0010" => |(feature, context)| format!("{feature} requires {context}");
    UNSUPPORTED_FEATURE: (String, String) = "P0011" => |(feature, context)| format!("{feature} is not supported in {context}");
    RESERVED_KEYWORD: String = "P0012" => |keyword| format!("'{keyword}' is a reserved keyword");
    READ_FILE: (String, String, String) = "P0013" => |(kind, path, error)| format!("Failed to read {kind} '{path}': {error}");
    INCLUDE_NOT_FOUND: String = "P0014" => |file| format!("Included file not found: {file}");
    PREPROCESSOR_ERROR: String = "P0015" => |message| format!("#error{}{}", if message.trim().is_empty() { "" } else { ": " }, message.trim());
    EVAL_EXPRESSION: () = "P0016" => |()| "Failed to evaluate preprocessor expression".into();
    STACK_STATE: (String, String, String) = "P0017" => |(stack, expected, context)| format!("Expected {stack} stack to be {expected} while {context}");
    UNRESOLVED_VARIABLE_TYPE: () = "P0018" => |()| "Could not resolve type for variable declaration".into();
    CANNOT_IMPORT_CURRENT_MODULE: (String,) = "P0019" => |(name,)| format!("Cannot import current module '{name}'");
    MATCH_DEFAULT: () = "P0020" => |()| "Match arms use binding patterns; replace 'default' with '_' to discard the binding".into();
    ASSOCIATED_FUNCTION: bool = "P0021" => |comptime| {
        let kind = if comptime { "comptime function" } else { "function" };
        format!("Associated {kind} declarations must have exactly two segments")
    };
}
