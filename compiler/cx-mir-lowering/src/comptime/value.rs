use cx_mir::mir::{
    expression::MIRExpression,
    r#type::{MIRFloatType, MIRIntegerType},
};
use cx_util::unsafe_float::FloatWrapper;

pub enum ComptimeValue<'emit> {
    Integer {
        val: i64,
        itype: MIRIntegerType,
    },
    Float {
        val: FloatWrapper,
        ftype: MIRFloatType,
    },

    Emit(&'emit MIRExpression),
}
