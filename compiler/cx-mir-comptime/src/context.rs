use cx_log::CXResult;
use cx_mir::{
    MIRConstant, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID,
    MIRGlobalKind, MIRGlobalState, MIRTypeRegistryBuilder, MIRUnit, MIRValue,
};
use cx_thir::thir::expression::THIRExpression;
use cx_tokens::TokenRange;

use crate::error::log_comptime_error;

pub trait ComptimeResolver {
    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction>;

    fn global_constant(&self, _id: MIRGlobalID) -> Option<MIRConstant> {
        None
    }

    fn global_initializer(&self, _id: MIRGlobalID) -> Option<MIRFunctionID> {
        None
    }

    fn global_kind(&self, _id: MIRGlobalID) -> Option<MIRGlobalKind> {
        None
    }

    fn types(&self) -> Option<&MIRTypeRegistryBuilder> {
        None
    }
}

impl ComptimeResolver for MIRUnit {
    fn resolve(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.function(id)
    }

    fn global_constant(&self, id: MIRGlobalID) -> Option<MIRConstant> {
        match &self.global(id)?.kind {
            MIRGlobalKind::Variable {
                state: MIRGlobalState::Initialized(value),
                ..
            } => Some(value.clone()),
            _ => None,
        }
    }

    fn global_initializer(&self, id: MIRGlobalID) -> Option<MIRFunctionID> {
        match &self.global(id)?.kind {
            MIRGlobalKind::Variable {
                state: MIRGlobalState::Initializer(function),
                ..
            } => Some(*function),
            _ => None,
        }
    }

    fn global_kind(&self, id: MIRGlobalID) -> Option<MIRGlobalKind> {
        Some(self.global(id)?.kind.clone())
    }

    fn types(&self) -> Option<&MIRTypeRegistryBuilder> {
        Some(self.types())
    }
}

pub trait MIRContext {
    fn current_prototype(&self) -> &MIRFnPrototype;

    fn comptime_resolver(&self) -> &dyn ComptimeResolver;

    fn lower_thir(&mut self, expression: &THIRExpression) -> CXResult<MIRValue>;

    fn capture_expression(&mut self, expression: &THIRExpression) -> CXResult<MIRFunction>;

    fn log_error<T>(&self, range: TokenRange, message: impl Into<String>) -> CXResult<T> {
        log_comptime_error(self, range, message)
    }
}
