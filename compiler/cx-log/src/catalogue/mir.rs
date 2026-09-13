use super::define_errors;

define_errors! {
    EXPECTED_CONSTANT: String = "M0001" => |context| format!("Expected a compile-time value in {context}");
    FUNCTION_RETURN: String = "M0002" => |name| format!("Function '{name}' with non-void return type must have an explicit return statement");
    ARRAY_TOO_LONG: (usize, usize) = "M0003" => |(actual, length)| format!("Array initializer has {actual} elements but the array length is {length}");
    INCOMPATIBLE_GLOBAL: String = "M0004" => |name| format!("Incompatible global declaration '{name}'");
    INVALID_CONTEXT: (String, String) = "M0005" => |(feature, context)| format!("{feature} cannot be used in {context}");
    REQUIRED_CONTEXT: (String, String) = "M0006" => |(feature, context)| format!("{feature} requires {context}");
    DUPLICATE_ENTITY: (String, String) = "M0007" => |(entity, context)| format!("Duplicate {entity} in {context}");
    MISSING_ENTITY: (String, String) = "M0008" => |(entity, context)| format!("Missing {entity} in {context}");
    MISSING_MAPPING: (String, String) = "M0009" => |(entity, mapping)| format!("No {mapping} mapping for {entity}");
    ENTITY_REQUIREMENT: (String, String, Option<String>) = "M0010" => |(subject, expected, found)| {
        let mut message = format!("{subject} requires {expected}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    ENTITY_MISMATCH: (String, String) = "M0011" => |(subject, expected)| format!("{subject} does not match {expected}");
    INDEX_BOUNDS: (String, String) = "M0012" => |(subject, index)| format!("{subject} index {index} is out of bounds");
    INVALID_LAYOUT: (String, String, Option<String>) = "M0013" => |(subject, requirement, found)| {
        let mut message = format!("Invalid layout for {subject}: requires {requirement}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    RECURSIVE_TYPE: String = "M0014" => |id| format!("Cannot compute layout of recursive MIR type {id}");
    INVALID_BITFIELD_WIDTH: (usize, usize) = "M0015" => |(width, storage_bits)| format!("Invalid bitfield width: {width} exceeds storage size of {storage_bits} bits");
    SIZE_OVERFLOW: () = "M0016" => |()| "MIR type layout size overflowed usize".into();
    MOVE_VALUE: String = "M0017" => |value| format!("Cannot move value: {value}");
    RUNTIME_CAPTURE_ESCAPE: () = "M0018" => |()| "A staged value with runtime captures escaped its originating function".into();
    RETAINED_PARAMETER: () = "M0019" => |()| "Staged template retained a comptime function parameter".into();
    UNEXPANDED_STAGED: () = "M0020" => |()| "Nested staged instruction was not expanded".into();
    COMPTIME_INVALID_OPERATION: String = "M0021" => |operation| format!("{operation} is not supported in comptime contexts");
    COMPTIME_POINTER_OVERFLOW: () = "M0022" => |()| "Pointer arithmetic overflowed during compile-time evaluation".into();
    COMPTIME_ZERO_DIVISOR: String = "M0023" => |operation| format!("{operation} by zero during compile-time evaluation");
    COMPTIME_STEP_LIMIT: u64 = "M0024" => |steps| format!("Comptime evaluation exceeded {steps} steps");
    COMPTIME_CALL_DEPTH: usize = "M0025" => |depth| format!("Comptime call depth exceeded {depth}");
    COMPTIME_ASSERTION: Option<String> = "M0026" => |message| message.unwrap_or_else(|| "Assertion failed at compile time".into());
    COMPTIME_UNREACHABLE: () = "M0027" => |()| "Unreachable code executed at compile time".into();
    COMPTIME_GLOBAL_CYCLE: () = "M0028" => |()| "Cyclic dependency between global initializers".into();
    COMPTIME_UNAVAILABLE: String = "M0029" => |entity| format!("{entity} is not available during comptime evaluation");
}
