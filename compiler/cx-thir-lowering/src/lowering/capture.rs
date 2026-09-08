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
    let in_safe_context = builder
        .try_fun()
        .map(|f| f.prototype().signature.safe)
        .unwrap_or(false);
    let mut saved_function = builder.take_current_function();
    let result = (|| -> CXResult<()> {
        let expr_type = lowering::lower_type(builder, &expression._type)?;
        builder.start_custom_function(
            MIRFunction::new(
                MIRFunctionID::new(usize::MAX),
                capture_prototype(expr_type, in_safe_context),
                None,
            ),
            saved_function.as_ref(),
        );
        builder.fun_mut().set_capture(
            saved_function
                .as_mut()
                .and_then(|function| function.take_capture()),
        );

        let value = lowering::lower_expression(builder, expression)?;
        builder.fun_mut().emit(
            MIRInstrKind::Return { value: Some(value) },
            TokenRange::internal(),
        );
        Ok(())
    })();

    let mut func = builder.take_current_function();
    if let Some(mut saved_function) = saved_function {
        if let Some(func) = func.as_mut() {
            saved_function.set_capture(func.take_capture());
        }
        builder.restore_current_function(saved_function);
    }

    result?;
    Ok(func.expect("capture builder is present").finish())
}
