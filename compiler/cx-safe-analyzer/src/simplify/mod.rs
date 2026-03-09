pub mod unary;
pub mod binary;

#[derive(Clone, Debug)]
pub enum ConstValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Unit,
}

pub fn int_to_bool(value: i64) -> bool {
    value != 0
}

pub fn bool_to_int(value: bool) -> i64 {
    if value { 1 } else { 0 }
}