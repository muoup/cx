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
    PARTIAL_MOVE: (String, String, bool) = "A0002" => |(function, place, discarded)| ownership_message(function, format!("@nodrop variable '{place}' has inconsistent ownership across control-flow paths"), discarded);
    VALUE_NOT_CONSUMED: (String, String, String, String, bool) = "A0003" => |(function, kind, place, exit, discarded)| ownership_message(function, format!("@nodrop {kind} '{place}' is not moved or leaked before {exit} exit"), discarded);
    AFTER_MOVE: (String, String, String, bool) = "A0004" => |(function, place, operation, discarded)| ownership_message(function, format!("Variable '{place}' {operation} after it was moved"), discarded);
    BEFORE_INITIALIZATION: (String, String, String, bool) = "A0005" => |(function, place, operation, discarded)| ownership_message(function, format!("Variable '{place}' {operation} before it was initialized"), discarded);
}
