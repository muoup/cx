use super::define_errors;

define_errors! {
    UNSUPPORTED_FEATURE: (String, String) = "B0005" => |(feature, backend)| format!("{feature} is not supported by {backend}");
    OPERATION_FAILED: (String, Option<String>) = "B0006" => |(operation, error)| {
        let mut message = format!("failed to {operation}");
        if let Some(error) = error {
            message.push_str(&format!(": {error}"));
        }
        message
    };

    MISSING_ENTITY: (String, String) = "BX001" => |(entity, context)| format!("missing {entity} in {context}");
    ENTITY_REQUIREMENT: (String, String, Option<String>) = "BX002" => |(subject, expected, found)| {
        let mut message = format!("{subject} requires {expected}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    INDEX_BOUNDS: (String, String) = "BX003" => |(subject, index)| format!("{subject} index {index} is out of bounds");
    ARGUMENT_COUNT: (String, String, String) = "BX004" => |(subject, expected, found)| format!("{subject} expects {expected} arguments, found {found}");
    TARGET_LAYOUT: (String, String, String, String) = "BX005" => |(backend, property, expected, found)| format!("LMIR target uses {property} {expected}, but {backend} target uses {found}");
    BUILDER_FAILED: (String, String) = "BX006" => |(backend, error)| format!("{backend} rejected the generated code: {error}");
}
