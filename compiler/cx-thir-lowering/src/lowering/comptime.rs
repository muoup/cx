use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRConstant, MIRFnParam, MIRFunctionID, MIRInstruction, MIRInstructionKind,
    MIRValue,
};
use cx_mir_comptime::{ComptimeContext, evaluate_body};
use cx_thir::thir::{comptime::THIRComptimeFn, data::THIRFunctionBody, expression::THIRExpression};
use cx_tokens::TokenRange;

use crate::{
    builder::{MIRBuilder, MIRTypeRegistryBuilder},
    log::mir_error,
    lowering::{emit_implicit_return, lower_expression},
};

impl ComptimeContext for MIRBuilder<'_> {
    type Registry = MIRTypeRegistryBuilder;

    fn function(&self, id: MIRFunctionID) -> Option<&cx_mir::MIRComptimeFunction<'_>> {
        self.module().comptime_function(id)
    }

    fn global(&self, id: cx_mir::MIRGlobalID) -> Option<&cx_mir::MIRGlobalVariable> {
        self.module().global(id)
    }

    fn types(&self) -> &Self::Registry {
        self.types()
    }
}

pub(crate) fn lower_comptime_function(
    builder: &mut MIRBuilder<'_>,
    id: MIRFunctionID,
    function: &THIRComptimeFn,
) -> CXResult<()> {
    let Some(body) = function.body.as_ref() else {
        return Ok(());
    };
    let prototype = builder
        .module()
        .comptime_function(id)
        .expect("comptime function was not declared")
        .prototype()
        .clone();
    builder.start_comptime_function(id, prototype);
    let declarations = builder
        .fun()
        .body()
        .comptime_prototype()
        .expect("comptime function body must have a prototype")
        .signature()
        .params()
        .to_vec();

    for (parameter, declaration) in function.prototype.params().iter().zip(declarations) {
        let param = MIRFnParam::new(
            declaration.name.clone(),
            declaration.ty.result_type(),
            false,
        );
        let scope = builder.fun().current_scope_id();
        let place = builder.fun_mut().body_mut().add_parameter(&param, scope);
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            TokenRange::internal(),
        ));
        builder
            .fun_mut()
            .bind_local(parameter.local_id, MIRValue::PlaceRef(place));
        if let Some(name) = &parameter.name {
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::PlaceRef(place));
        }
    }

    match body {
        THIRFunctionBody::Expression(expression) => {
            let value = lower_expression(builder, expression)?;
            emit_implicit_return(builder, Some(value), expression.token_range.clone())?;
        }
        THIRFunctionBody::Block { exprs, token_range } => {
            for expression in exprs {
                lower_expression(builder, expression)?;
            }
            if function.prototype.return_type()._type.is_void() {
                emit_implicit_return(builder, None, token_range.clone())?;
            }
        }
    }
    builder.finish_function()
}

pub(crate) fn evaluate_integer(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    context: &str,
) -> CXResult<usize> {
    match evaluate(builder, expression)? {
        MIRConstant::Integer { value, .. } => usize::try_from(value).map_err(|_| {
            mir_error(
                &expression.token_range,
                (&mir::EXPECTED_CONSTANT, context.to_owned()),
            )
        }),
        _ => Err(mir_error(
            &expression.token_range,
            (&mir::EXPECTED_CONSTANT, context.to_owned()),
        )),
    }
}

pub(crate) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let parent = builder.take_current_function();
    let id = builder.module_mut().allocate_function_id();
    builder.start_comptime_scratch(id);

    let lowered = (|| {
        let value = lower_expression(builder, expression)?;
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Return { value: Some(value) },
            expression.token_range.clone(),
        ));
        Ok(())
    })();
    let body = builder.finish_comptime_scratch();
    if let Some(parent) = parent {
        builder.restore_current_function(parent);
    }
    lowered?;
    evaluate_body(builder, &body, &[])
}

pub(crate) fn evaluate_function(
    builder: &MIRBuilder<'_>,
    id: MIRFunctionID,
    args: &[MIRConstant],
) -> CXResult<MIRConstant> {
    let body = builder
        .module()
        .comptime_function(id)
        .and_then(|function| function.body())
        .expect("comptime function must be defined before evaluation");
    evaluate_body(builder, body, args)
}
