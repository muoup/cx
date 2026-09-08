use cx_log::CXResult;
use cx_mir::{
    MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID, MIRFunctionMode, MIRInstrKind,
    MIRTypeID,
};
use cx_thir::thir::expression::THIRExpression;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{MIRBuilder, lowering};

fn capture_prototype(return_type: MIRTypeID, safe: bool) -> MIRFnPrototype {
    MIRFnPrototype {
        signature: MIRFnSignature::new(
            CXIdent::from("__capture"),
            None,
            Vec::new(),
            return_type,
            MIRFunctionMode::Comptime,
            false,
            safe,
        ),
        linkage: LinkageMode::Static,
    }
}

pub fn capture_expression(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRFunction> {
    let in_safe_context = builder.try_fun()
        .map(|f| f.prototype().signature.safe)
        .unwrap_or(false);
    let mut saved_function = builder.take_current_function();
    let saved_capture = saved_function
        .as_mut()
        .and_then(|function| function.take_capture());
    let expr_type = lowering::lower_type(builder, &expression._type)?;

    builder.start_custom_function(
        MIRFunction::new(
            MIRFunctionID::new(usize::MAX),
            capture_prototype(expr_type, in_safe_context),
            None,
        ),
        saved_function.as_ref(),
    );
    builder.fun_mut().set_capture(saved_capture);

    let value = lowering::lower_expression(builder, expression)?;
    builder.fun_mut().emit(
        MIRInstrKind::Return { value: Some(value) },
        TokenRange::internal(),
    );

    let mut func = builder
        .take_current_function()
        .expect("capture builder is present (2)");

    let capture = func.take_capture();
    if let Some(mut saved_function) = saved_function {
        saved_function.set_capture(capture);
        builder.restore_current_function(saved_function);
    }

    Ok(func.finish())
}
