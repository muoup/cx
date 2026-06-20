pub trait CXErrorMessage {
    fn code(&self) -> String;

    fn message(&self) -> String;

    fn as_string(&self) -> String {
        format!("{}: {}", self.code(), self.message())
    }
}