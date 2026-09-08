use super::define_errors;

fn ownership_message(function: String, message: String, discarded: bool) -> String {
    let mut message = format!("Ownership error in function '{function}': {message}");
    if discarded {
        message.push_str("; '_' is an intentionally unused binding, but it still follows ownership rules; any @nodrop value must be moved or leaked");
    }
    message
}

define_errors! {
    PROVEN_FALSE_ASSERTION: (String, Option<String>) = "A0001" => |(function, message)| {
        let mut result = format!("Assertion in function {function} is provably false");
        if let Some(message) = message {
            result.push_str(": ");
            result.push_str(&message);
        }
        result
    };
    PARTIAL_MOVE: (String, String, bool) = "A0002" => |(function, place, discarded)| ownership_message(function, format!("@nodrop place '{place}' is moved on only some control-flow paths"), discarded);
    SCOPE_EXIT_NOT_CONSUMED: (String, String, bool) = "A0003" => |(function, place, discarded)| ownership_message(function, format!("@nodrop place '{place}' is not moved or leaked before scope exit"), discarded);
    FUNCTION_EXIT_NOT_CONSUMED: (String, String, bool) = "A0004" => |(function, place, discarded)| ownership_message(function, format!("@nodrop place '{place}' is not moved or leaked before function exit"), discarded);
    PARAMETER_NOT_CONSUMED: (String, String, bool) = "A0005" => |(function, place, discarded)| ownership_message(function, format!("@nodrop parameter '{place}' is not moved or leaked before function exit"), discarded);
    USE_AFTER_MOVE: (String, String, bool) = "A0006" => |(function, place, discarded)| ownership_message(function, format!("place '{place}' used after it was moved"), discarded);
    USE_BEFORE_INITIALIZATION: (String, String, bool) = "A0007" => |(function, place, discarded)| ownership_message(function, format!("place '{place}' used before it was initialized"), discarded);
    REPEATED_MOVE: (String, String, bool) = "A0008" => |(function, place, discarded)| ownership_message(function, format!("place '{place}' moved more than once"), discarded);
    MOVE_BEFORE_INITIALIZATION: (String, String, bool) = "A0009" => |(function, place, discarded)| ownership_message(function, format!("place '{place}' moved before it was initialized"), discarded);
}
