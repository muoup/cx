use log::warn;
use crate::parse::ast::Expression;
use crate::parse::verify::context::VerifyContext;
use crate::parse::verify::ValueTypeRef;

pub(crate) fn verify_type(context: &mut VerifyContext, type_: &str) -> Option<ValueTypeRef> {
    let Some(val_type) = context.types_table.get(type_).cloned() else {
        warn!("Type {} not found", type_);
        return None
    };

    Some(val_type)
}

pub(crate) fn verify_lval(context: &mut VerifyContext, lval: &Expression) -> Option<ValueTypeRef> {
    unimplemented!()
}
