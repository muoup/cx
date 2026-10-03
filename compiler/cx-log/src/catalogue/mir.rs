use super::define_errors;

define_errors! {
    EXPECTED_CONSTANT: (String, String) = "M0001" => |(subject, kind)| format!("{subject} is not a compile-time {kind}");
    FUNCTION_RETURN: String = "M0002" => |name| format!("function '{name}' with non-void return type must have an explicit return statement");
    ARRAY_TOO_LONG: (usize, usize) = "M0003" => |(actual, length)| format!("array initializer has {actual} elements but the array length is {length}");
    INCOMPATIBLE_GLOBAL: String = "M0004" => |name| format!("incompatible global declaration '{name}'");
    INVALID_CONTEXT: (String, String) = "M0005" => |(feature, context)| format!("{feature} cannot be used in {context}");
    REQUIRED_CONTEXT: (String, String) = "M0006" => |(feature, context)| format!("{feature} requires {context}");
    DUPLICATE_ENTITY: (String, String) = "M0007" => |(entity, context)| format!("duplicate {entity} in {context}");
    MISSING_ENTITY: (String, String) = "M0008" => |(entity, context)| format!("missing {entity} in {context}");
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
        let mut message = format!("invalid layout for {subject}: requires {requirement}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    INVALID_BITFIELD_WIDTH: (usize, usize) = "M0015" => |(width, storage_bits)| format!("invalid bitfield width: {width} exceeds storage size of {storage_bits} bits");
    MOVE_VALUE: String = "M0017" => |value| format!("cannot move value: {value}");
    COMPTIME_INVALID_OPERATION: String = "M0021" => |operation| format!("{operation} is not supported in comptime contexts");
    COMPTIME_POINTER_OVERFLOW: () = "M0022" => |()| "pointer arithmetic overflowed during compile-time evaluation".into();
    COMPTIME_ZERO_DIVISOR: String = "M0023" => |operation| format!("{operation} by zero during compile-time evaluation");
    COMPTIME_STEP_LIMIT: u64 = "M0024" => |steps| format!("comptime evaluation exceeded {steps} steps");
    COMPTIME_CALL_DEPTH: usize = "M0025" => |depth| format!("comptime call depth exceeded {depth}");
    COMPTIME_ASSERTION: Option<String> = "M0026" => |message| message.unwrap_or_else(|| "assertion failed at compile time".into());
    COMPTIME_UNREACHABLE: () = "M0027" => |()| "unreachable code executed at compile time".into();
    DEPENDENCY_CYCLE: String = "M0028" => |name| format!("'{name}' depends on itself");
    COMPTIME_UNAVAILABLE: String = "M0029" => |entity| format!("{entity} is not available during comptime evaluation");
    RUNTIME_UNAVAILABLE: String = "M0030" => |name| format!("'{name}' is not available at runtime");
    COMPTIME_FUNCTION_AT_RUNTIME: String = "M0031" => |name| format!("comptime function '{name}' cannot be used at runtime");
    COMPTIME_VALUE_AT_RUNTIME: () = "M0032" => |()| "comptime-only value used as a runtime constant".into();
    COMPTIME_NO_BODY: String = "M0033" => |name| format!("'{name}' has no body to evaluate at compile time");
    COMPTIME_UNSET: String = "M0034" => |subject| format!("{subject} has no value at compile time");
    COMPTIME_NO_MATCH: () = "M0035" => |()| "no match arm matches this value at compile time".into();
    COMPTIME_LOOP_LIMIT: usize = "M0036" => |limit| format!("compile-time loop exceeded {limit} iterations");
    COMPTIME_UNDEFINED_ARITHMETIC: String = "M0037" => |operation| format!("'{operation}' has no defined result for these operands at compile time");
    COMPTIME_NO_TRUTH_VALUE: String = "M0038" => |value| format!("{value} has no truth value at compile time");
    COMPTIME_CONTROL_ESCAPE: () = "M0039" => |()| "control flow cannot leave a compile-time expression".into();

    MISSING_MAPPING: (String, String) = "MX001" => |(entity, mapping)| format!("no {mapping} mapping for {entity}");
    RECURSIVE_TYPE: String = "MX002" => |id| format!("cannot compute layout of recursive MIR type {id}");
    SIZE_OVERFLOW: () = "MX003" => |()| "MIR type layout size overflowed usize".into();
    RUNTIME_CAPTURE_ESCAPE: () = "MX004" => |()| "a staged value with runtime captures escaped its originating function".into();
    RETAINED_PARAMETER: () = "MX005" => |()| "staged template retained a comptime function parameter".into();
    UNEXPANDED_STAGED: () = "MX006" => |()| "nested staged instruction was not expanded".into();
    MIR_USE_UNAVAILABLE: String = "MX007" => |entity| format!("use of unavailable {entity}");
    MIR_NODROP_DISCARD: String = "MX008" => |entity| format!("cannot discard live nodrop {entity}");
    MIR_BLOCK_ARGUMENTS: (usize, usize) = "MX009" => |(given, expected)| format!("block edge passes {given} values but destination expects {expected}");
    MIR_UNDEFINED_REGISTER: String = "MX010" => |register| format!("register {register} is not available in this block");
    MIR_UNTERMINATED_BLOCK: String = "MX011" => |block| format!("reachable MIR block {block} has no terminator");
    UNEXPECTED_DEF: (String, String) = "MX012" => |(name, expected)| format!("'{name}' was expected to be {expected}");
    UNRESOLVED_NOMINAL: () = "MX013" => |()| "nominal type reached the type table before being resolved".into();
    CONSTANT_TYPE: (String, String) = "MX014" => |(kind, ty)| format!("{kind} constant has type '{ty}'");
    UNSET_PAYLOAD: () = "MX015" => |()| "matched a tagged union whose payload is not set".into();
    UNSUPPORTED_LOWERING: String = "MX016" => |subject| format!("{subject} reached MIR generation");
    MALFORMED_HIR: String = "MX017" => |subject| format!("malformed HIR: {subject}");
    COMPTIME_INVARIANT: String = "MX018" => |subject| format!("compile-time evaluator reached {subject}");
}
