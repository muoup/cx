use super::define_errors;

define_errors! {
    UNKNOWN_SYMBOL: String = "T0001" => |name| format!("Unknown symbol '{name}'");
    UNEXPECTED_SYMBOL: (String, String) = "T0002" => |(name, expected)| format!("Unexpected symbol '{name}', expected {expected}");
    UNKNOWN_MEMBER: (String, String) = "T0003" => |(owner, member)| format!("Unknown member '{member}' in {owner}");
    VARIABLE_REDECLARATION: String = "T0004" => |name| format!("Variable '{name}' redeclared with a different type");
    ASSIGN_TO_CONST: () = "T0005" => |()| "Cannot assign to a const type".into();
    INCOMPATIBLE_DECLARATION: (String, String) = "T0006" => |(kind, name)| format!("Incompatible {kind} declarations for '{name}'");
    DUPLICATE_ITEM: (String, String) = "T0007" => |(item, context)| format!("Duplicate {item} in {context}");
    AMBIGUOUS_SYMBOL: String = "T0008" => |candidates| format!("Ambiguous symbol reference, candidates: {candidates}");
    INCOMPLETE_TYPE: String = "T0009" => |subject| format!("{subject} has an incomplete type");
    RECURSIVE_TYPE: String = "T0010" => |subject| format!("{subject} has a recursive type");
    TYPE_REQUIREMENT: (String, String, Option<String>) = "T0011" => |(subject, expected, found)| {
        let mut message = format!("{subject} requires {expected}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    TYPE_MISMATCH: (String, String, String) = "T0012" => |(context, expected, found)| format!("Type mismatch in {context}: expected {expected}, found {found}");
    INVALID_CAST: (String, String, String) = "T0013" => |(kind, from, to)| format!("No {kind} cast from {from} to {to}");
    INVALID_BINARY_OPERANDS: (String, String, String) = "T0014" => |(operation, left, right)| format!("Invalid operands to {operation}: {left} and {right}");
    ARGUMENT_COUNT: (String, usize, usize, bool) = "T0015" => |(subject, expected, found, var_args)| {
        let qualifier = if var_args { "at least " } else { "" };
        format!("{subject} expects {qualifier}{expected} arguments, found {found}")
    };
    INITIALIZER_LIMIT: (String, Option<usize>) = "T0016" => |(subject, limit)| {
        let mut message = format!("Too many elements in {subject} initializer");
        if let Some(limit) = limit {
            message.push_str(&format!("; at most {limit} allowed"));
        }
        message
    };
    INVALID_CONTEXT: (String, String) = "T0017" => |(feature, context)| format!("{feature} cannot be used in {context}");
    REQUIRED_CONTEXT: (String, String) = "T0018" => |(feature, context)| format!("{feature} requires {context}");
    INVALID_FORM: (String, String) = "T0019" => |(subject, invalid)| format!("Unexpected {invalid} in {subject}");
    MISSING_RETURN_VALUE: String = "T0020" => |function| format!("Invalid unvalued return in a non-void returning function {function}");
    INTEGER_LITERAL_RANGE: String = "T0021" => |value| format!("Integer literal {value} does not fit any permitted type");
    TEMPLATE_ARGUMENTS: (String, bool) = "T0022" => |(subject, required)| {
        let requirement = if required { "requires" } else { "does not accept" };
        format!("{subject} {requirement} template arguments")
    };
    TEMPLATE_DEDUCTION: String = "T0023" => |subject| format!("Could not deduce {subject}");
    CONFLICTING_DEDUCTIONS: (String, String, String) = "T0024" => |(parameter, first, second)| format!("Conflicting deductions for template argument '{parameter}': {first} vs {second}");
    TEMPLATE_APPLICATION: () = "T0025" => |()| "Failed to apply template arguments".into();
    TEMPLATE_NOT_CONCRETE: String = "T0026" => |name| format!("Template arguments did not resolve type '{name}' to a concrete type");
    LOCAL_VARIABLE_REQUIRED: String = "T0027" => |feature| format!("{feature} only accepts locally defined variables");
    UNSAFE_OPERATION: String = "T0028" => |feature| format!("{feature} is unsafe and so cannot be used in safe contexts, wrap this expression in an `@unsafe` block to bypass this restriction");
    FIELD_TRAIT: (String, String, String) = "T0029" => |(field, field_trait, required)| format!("Aggregate containing {field_trait} field '{field}' must also be marked as {required}");
    ADOPT_LOCAL: () = "T0030" => |()| "@adopt of a local binding is not allowed; use move for local bindings".into();
    UNPACK_REQUIRED_FIELD: (String, String) = "T0031" => |(owner, field)| format!("@unpack of {owner} must bind @nodrop field '{field}'");
    UNREACHABLE_MATCH_ARM: () = "T0032" => |_| format!("Unreachable match arm: this pattern is already covered by a previous arm");
    NONEXHAUSTIVE_MATCH: Option<String> = "T0033" => |missing| {
        let mut message = "Match must be exhaustive".to_owned();
        if let Some(missing) = missing {
            message.push_str(&format!("; missing variants: {missing}"));
        }
        message.push_str("; add the missing arms or a catch-all binding such as '_ => ...'");
        message
    };
    MATCH_PAYLOAD_BINDING: (String, String) = "T0034" => |(variant, owner)| format!("Variant '{variant}' of tagged union '{owner}' has a non-void type, but no inner name was provided in the pattern");
    INVALID_PATTERN: () = "T0035" => |()| "Pattern does not match the type of the value being matched".into();
    MISSING_YIELD: String = "T0036" => |subject| format!("{subject} must yield a value on every path");
    MIXED_YIELDS: (Option<String>, Option<String>) = "T0037" => |(expected, found)| {
        format!("Yielding {}, but expected {}", expected.unwrap_or("no value".into()), found.unwrap_or("no value".into()))
    };
    DEFER_FALLTHROUGH: () = "T0038" => |()| "Deferred expression must fall through normally".into();
    INDEX_BOUNDS: (String, String, Option<String>) = "T0039" => |(subject, index, length)| {
        let mut message = format!("{subject} index {index} is out of bounds");
        if let Some(length) = length {
            message.push_str(&format!(" (length {length})"));
        }
        message
    };
    MISSING_ENTITY: (String, String) = "T0040" => |(entity, context)| format!("Missing {entity} in {context}");
    UNSUPPORTED_FEATURE: String = "T0042" => |feature| format!("{feature} is not currently supported");

    POP_EMPTY_SCOPE: () = "TX001" => |()| "Attempted to pop a scope from an empty scope stack".into();
}
