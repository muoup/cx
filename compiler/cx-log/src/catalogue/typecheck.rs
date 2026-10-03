use super::define_errors;

define_errors! {
    UNKNOWN_SYMBOL: String = "T0001" => |name| format!("unknown symbol '{name}'");
    UNEXPECTED_KIND: (String, String) = "T0002" => |(subject, expected)| format!("{subject} is not {expected}");
    UNKNOWN_MEMBER: (String, String) = "T0003" => |(owner, member)| format!("'{owner}' has no member '{member}'");
    VARIABLE_REDECLARATION: String = "T0004" => |name| format!("variable '{name}' redeclared with a different type");
    MUTATE_CONST: (String, String) = "T0005" => |(action, ty)| format!("cannot {action} a value of type '{ty}'");
    INCOMPATIBLE_DECLARATION: String = "T0006" => |name| format!("incompatible declarations for '{name}'");
    DUPLICATE_ITEM: (String, String) = "T0007" => |(item, context)| format!("duplicate {item} in {context}");
    AMBIGUOUS_SYMBOL: String = "T0008" => |candidates| format!("ambiguous symbol reference, candidates: {candidates}");
    INCOMPLETE_TYPE: String = "T0009" => |subject| format!("{subject} has an incomplete type");
    INVALID_OBJECT_TYPE: (String, String) = "T0010" => |(subject, problem)| format!("{subject} has {problem}");
    TYPE_REQUIREMENT: (String, String, Option<String>) = "T0011" => |(subject, expected, found)| {
        let mut message = format!("{subject} requires {expected}");
        if let Some(found) = found {
            message.push_str(&format!(", found {found}"));
        }
        message
    };
    TYPE_MISMATCH: (String, String, String) = "T0012" => |(context, expected, found)| format!("type mismatch in {context}: expected {expected}, found {found}");
    INVALID_CONVERSION: (String, String) = "T0013" => |(from, to)| format!("cannot convert {from} to '{to}'");
    INVALID_BINARY_OPERANDS: (String, String, String) = "T0014" => |(operation, left, right)| format!("no operator '{operation}' for '{left}' and '{right}'");
    ARGUMENT_COUNT: (String, usize, usize, bool) = "T0015" => |(subject, expected, found, var_args)| {
        let qualifier = if var_args { "at least " } else { "" };
        format!("{subject} expects {qualifier}{expected} arguments, found {found}")
    };
    INITIALIZER_LIMIT: (String, Option<usize>) = "T0016" => |(subject, limit)| {
        let mut message = format!("too many elements in {subject} initializer");
        if let Some(limit) = limit {
            message.push_str(&format!("; at most {limit} allowed"));
        }
        message
    };
    INVALID_CONTEXT: (String, String) = "T0017" => |(feature, context)| format!("{feature} cannot be used in {context}");
    REQUIRED_CONTEXT: (String, String) = "T0018" => |(feature, context)| format!("{feature} outside of {context}");
    INVALID_FORM: (String, String) = "T0019" => |(subject, invalid)| format!("unexpected {invalid} in {subject}");
    MISSING_RETURN_VALUE: () = "T0020" => |()| "return requires a value in a non-void function".into();
    INTEGER_LITERAL_RANGE: () = "T0021" => |_| "integer literal does not fit any permitted type".into();
    TEMPLATE_ARGUMENTS: (String, bool) = "T0022" => |(subject, required)| {
        let requirement = if required { "requires" } else { "does not accept" };
        format!("{subject} {requirement} template arguments")
    };
    TEMPLATE_DEDUCTION: String = "T0023" => |subject| format!("cannot deduce {subject}");
    CONFLICTING_DEDUCTIONS: (String, String, String) = "T0024" => |(parameter, first, second)| format!("conflicting deductions for comptime argument '{parameter}': {first} vs {second}");
    TEMPLATE_APPLICATION: () = "T0025" => |()| "failed to apply template arguments".into();
    TEMPLATE_NOT_CONCRETE: String = "T0026" => |name| format!("template arguments did not resolve type '{name}' to a concrete type");
    LOCAL_VARIABLE_REQUIRED: String = "T0027" => |feature| format!("{feature} only accepts locally defined variables");
    UNSAFE_OPERATION: String = "T0028" => |feature| format!("{feature} is unsafe and so cannot be used in safe contexts, wrap this expression in an `@unsafe` block to bypass this restriction");
    FIELD_TRAIT: (String, String, String) = "T0029" => |(field, field_trait, required)| format!("aggregate containing {field_trait} field '{field}' must also be marked as {required}");
    ADOPT_LOCAL: () = "T0030" => |()| "@adopt of a local binding is not allowed; use move for local bindings".into();
    UNPACK_REQUIRED_FIELD: (String, String) = "T0031" => |(owner, field)| format!("@unpack of '{owner}' must bind @nodrop field '{field}'");
    UNREACHABLE_MATCH_ARM: () = "T0032" => |_| "unreachable match arm: this pattern is already covered by a previous arm".into();
    NONEXHAUSTIVE_MATCH: Option<String> = "T0033" => |missing| {
        let mut message = "match must be exhaustive".to_owned();
        if let Some(missing) = missing {
            message.push_str(&format!("; missing variants: {missing}"));
        }
        message.push_str("; add the missing arms or a catch-all binding such as '_ => ...'");
        message
    };
    MATCH_PAYLOAD_BINDING: (String, String) = "T0034" => |(variant, owner)| format!("variant '{variant}' of tagged union '{owner}' has a non-void type, but no inner name was provided in the pattern");
    INVALID_PATTERN: () = "T0035" => |()| "pattern does not fit the matched value".into();
    MISSING_YIELD: String = "T0036" => |subject| format!("{subject} must yield a value on every path");
    MIXED_YIELDS: (Option<String>, Option<String>) = "T0037" => |(expected, found)| {
        format!("yielding {}, but expected {}", expected.unwrap_or("no value".into()), found.unwrap_or("no value".into()))
    };
    DEFER_FALLTHROUGH: () = "T0038" => |()| "deferred expression must fall through".into();
    INDEX_BOUNDS: (String, String, Option<String>) = "T0039" => |(subject, index, length)| {
        let mut message = format!("{subject} index {index} is out of bounds");
        if let Some(length) = length {
            message.push_str(&format!(" (length {length})"));
        }
        message
    };
    MISSING_ENTITY: (String, String) = "T0040" => |(entity, context)| format!("missing {entity} in {context}");
    UNSUPPORTED_FEATURE: String = "T0042" => |feature| format!("{feature} is not currently supported");
    BITFIELD_REFERENCE: String = "T0043" => |usage| format!("cannot {usage} a bitfield");
    REDEFINITION: String = "T0044" => |name| format!("redefinition of '{name}'");
    INVALID_OPERAND: (String, String) = "T0045" => |(operation, ty)| format!("cannot {operation} '{ty}'");
    FLOATING_OPERAND: String = "T0046" => |operation| format!("'{operation}' cannot be applied to floating values");
    NO_TRUTH_VALUE: String = "T0047" => |ty| format!("'{ty}' has no truth value");
    DISCARDED_NODROP: (String, String) = "T0048" => |(from, to)| format!("cannot convert '{from}' to '{to}': the @nodrop result would be discarded");
    UNKNOWN_VARIANT: (String, String) = "T0049" => |(owner, variant)| format!("'{owner}' has no variant '{variant}'");
    INVALID_POINTEE: String = "T0050" => |ty| format!("cannot form a pointer to '{ty}'");
    VOID_FIELD: () = "T0051" => |()| "aggregate fields cannot have type 'void'".into();
    CANNOT_INFER: String = "T0052" => |subject| format!("cannot infer {subject}");
    EXPECTED_TYPE: String = "T0053" => |found| format!("expected a type, found {found}");
    LIST_INITIALIZATION: String = "T0054" => |ty| format!("'{ty}' cannot be initialized from a list");
    NOT_ADDRESSABLE: String = "T0055" => |action| format!("cannot {action} a value that is not addressable");
    VOID_RETURN_VALUE: () = "T0056" => |()| "cannot return a value from a void function".into();
    NORETURN_RETURN: () = "T0057" => |()| "cannot return from a function that never returns".into();
    DEFER_JUMP: String = "T0058" => |jump| format!("cannot {jump} from a deferred expression");
    POINTER_PATTERN: () = "T0059" => |()| "pattern subject is a pointer; dereference it explicitly".into();
    VALUE_PATTERN_CONSTANT: () = "T0060" => |()| "value pattern is not an integer constant; bind the value with 'auto name'".into();
    FLOATING_CASE: () = "T0061" => |()| "floating patterns cannot be matched by cases".into();
    BINDING_BEHIND_REFERENCE: String = "T0062" => |binding| format!("'{binding}' binds by value, but the matched value is behind a reference; bind it with 'auto&'");
    BINDING_MOVES_IN_USE: (String, String) = "T0063" => |(binding, ty)| format!("'{binding}' would move '{ty}' out of a value that is still in use; borrow it with 'auto&' or match on a moved value");
    UNPACK_OWNED: Option<String> = "T0064" => |found| {
        let mut message = "@unpack takes an owned structure".to_owned();
        if let Some(found) = found {
            message.push_str(&format!(", found '{found}'; move the value into it"));
        }
        message
    };
    COMPTIME_ONLY_TYPE: String = "T0065" => |ty| format!("comptime-only type '{ty}' used at runtime");
    VOID_POSTCONDITION_BINDING: () = "T0066" => |()| "void function has no result to bind in its postcondition".into();
    INCOMPATIBLE_TAG: String = "T0067" => |name| format!("incompatible tag declarations for '{name}'");
    UNSUPPORTED_LITERAL: String = "T0068" => |kind| format!("{kind} literals are not supported");
    BINDING_COPIES_IN_USE: String = "T0069" => |ty| format!("binding would copy '{ty}' out of a value that is still in use");

    POP_EMPTY_SCOPE: () = "TX001" => |()| "attempted to pop a scope from an empty scope stack".into();
}
