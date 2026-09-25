use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRComptimeOperand, MIRComptimeParameter, MIRComptimeValue, MIRConstant,
    MIRFunctionID, MIRInstruction, MIRInstructionKind, MIRValue,
};
use cx_mir_comptime::{ComptimeContext, evaluate_body};
use cx_thir::thir::{comptime::THIRComptimeFn, data::THIRFunctionBody, expression::THIRExpression};
use cx_tokens::TokenRange;

use crate::{
    builder::{MIRBuilder, MIRTypeRegistryBuilder},
    log::mir_error,
    lowering::{
        LowerResult, LowerStop,
        control_flow::{auto_cleanup, lower_sequence},
        emit_implicit_return, lower_expression, staged,
    },
};

impl<'thir> ComptimeContext<'thir> for MIRBuilder<'thir> {
    type Registry = MIRTypeRegistryBuilder;

    fn function(&self, id: MIRFunctionID) -> Option<&cx_mir::MIRComptimeFunction<'thir>> {
        self.module().comptime_function(id)
    }

    fn global(&self, id: cx_mir::MIRGlobalID) -> Option<&cx_mir::MIRGlobalVariable> {
        self.module().global(id)
    }

    fn types(&self) -> &Self::Registry {
        self.types()
    }

    fn add_staged_expression(
        &self,
        expression: cx_mir::MIRStagedExpression<'thir>,
    ) -> cx_mir::MIRStagedID {
        self.module().add_staged_expression(expression)
    }
}

pub(crate) fn lower_comptime_function<'thir>(
    builder: &mut MIRBuilder<'thir>,
    id: MIRFunctionID,
    function: &'thir THIRComptimeFn,
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
        let scope = builder.fun().current_scope_id();
        let binding = builder.fun_mut().body_mut().add_comptime_parameter(
            declaration.ty,
            declaration.name,
            parameter.value_type._type.is_nodrop(),
            scope,
        );
        match binding {
            MIRComptimeParameter::Runtime(place) => {
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
            MIRComptimeParameter::Comptime(register) => {
                builder.fun_mut().bind_comptime_local(
                    parameter.local_id,
                    MIRComptimeOperand::Comptime(register),
                );
            }
        }
    }

    let lowered = (|| -> LowerResult<()> {
        match body {
            THIRFunctionBody::Expression(expression) => {
                if function.prototype.return_type().expr {
                    let value = staged::lower_operand(builder, expression)?;
                    let root_scope = builder
                        .fun()
                        .scope_stack()
                        .first()
                        .expect("active function has no root scope")
                        .id();
                    auto_cleanup(builder, root_scope, true, expression.token_range.clone())?;
                    builder.emit_comptime(
                        cx_mir::MIRComptimeOp::Return { value: Some(value) },
                        expression.token_range.clone(),
                    );
                } else {
                    let value = lower_expression(builder, expression)?;
                    emit_implicit_return(builder, Some(value), expression.token_range.clone())
                        .map_err(LowerStop::Diagnostic)?;
                }
            }
            THIRFunctionBody::Block { exprs, token_range } => {
                lower_sequence(builder, exprs, true)?;
                if function.prototype.return_type()._type.is_void() {
                    emit_implicit_return(builder, None, token_range.clone())
                        .map_err(LowerStop::Diagnostic)?;
                }
            }
        }
        Ok(())
    })();
    if let Err(LowerStop::Diagnostic(error)) = lowered {
        return Err(error);
    }
    builder.finish_function()
}

pub(crate) fn evaluate_integer<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expression: &'thir THIRExpression,
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

pub(crate) fn evaluate<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expression: &'thir THIRExpression,
) -> CXResult<MIRConstant> {
    let parent = builder.take_current_function();
    let id = builder.module_mut().allocate_function_id();
    builder.start_comptime_scratch(id);

    let lowered = (|| -> LowerResult<()> {
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
    if let Err(LowerStop::Diagnostic(error)) = lowered {
        return Err(error);
    }

    match evaluate_body(builder, &body, &[], "<comptime expression>")? {
        MIRComptimeValue::Constant(value) => Ok(value),
        _ => Err(mir_error(
            &expression.token_range,
            (&mir::EXPECTED_CONSTANT, "compile-time expression".into()),
        )),
    }
}

pub(crate) fn evaluate_function<'thir>(
    builder: &MIRBuilder<'thir>,
    id: MIRFunctionID,
    args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    let function = builder
        .module()
        .comptime_function(id)
        .expect("comptime function must be defined before evaluation");
    let body = function
        .body()
        .expect("comptime function must be defined before evaluation");

    evaluate_body(builder, body, args, function.prototype().name().as_str())
}
