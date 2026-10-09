use cx_hmir::{HMIRDefID, expr::HMIRExprID};

use crate::value::StaticValue;

#[derive(Debug)]
pub struct LoweringContext {
    value_cache: HashMap<(HMIRDefID, HMIRExprID), StaticValue>,
}