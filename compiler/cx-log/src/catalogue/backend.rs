use super::define_errors;

define_errors! {
    MISSING_ENTITY: (String, String) = "B0001" => |(entity, context)| format!("Missing {entity} in {context}");
    ENTITY_REQUIREMENT: (String, String, Option<String>) = "B0002" => |(subject, expected, found)| {
        let mut message = format!("{subject} requires {expected}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    INDEX_BOUNDS: (String, String) = "B0003" => |(subject, index)| format!("{subject} index {index} is out of bounds");
    ARGUMENT_COUNT: (String, String, String) = "B0004" => |(subject, expected, found)| format!("{subject} expects {expected} arguments, found {found}");
    UNSUPPORTED_FEATURE: (String, String) = "B0005" => |(feature, backend)| format!("{feature} is not supported by {backend}");
    OPERATION_FAILED: (String, Option<String>) = "B0006" => |(operation, error)| {
        let mut message = format!("Failed to {operation}");
        if let Some(error) = error {
            message.push_str(&format!(": {error}"));
        }
        message
    };
    TARGET_LAYOUT: (String, String, String, String) = "B0007" => |(backend, property, expected, found)| format!("LMIR target uses {property} {expected}, but {backend} target uses {found}");
}
