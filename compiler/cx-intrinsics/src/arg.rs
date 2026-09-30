#[derive(Debug)]
pub enum IntrinsicArg<'a, V, T> {
    Value(&'a V),
    Type(&'a T),
    Flag(bool),
    Message(&'a str),
}
