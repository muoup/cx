pub(crate) mod function;
pub(crate) mod lowering;
pub(crate) mod module;
pub(crate) mod resolver;
pub(crate) mod symbols;

use cx_hmir::{
    HMIRBody, HMIRDefKind, HMIRDefRef, HMIROp, HMIRUnit,
    expr::{HMIRExprID, HMIRExprKind, constant::HMIRConstant, op::HMIRTypeOp},
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatWidth, HMIRIntWidth},
    },
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRConstant, MIRFloatType, MIRIntType, MIRType, MIRTypeID, MIRTypeKind, MIRUnit};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;

use crate::{
    env::{module::MIRModuleBuilder, resolver::HMIRResolver, symbols::HMIRSymbolEnv},
    log::hmir_error,
};

#[derive(Debug)]
pub struct HMIREnvironment<'global, 'hmir> {
    global: &'global ModuleData,
    architecture: ArchitectureConfig,

    module: MIRModuleBuilder,
    symbols: HMIRSymbolEnv<'global, 'hmir>,
    types: HMIRResolver,
}

impl<'global, 'hmir> HMIREnvironment<'global, 'hmir> {
    pub fn new(
        global: &'global ModuleData,
        unit: &'hmir HMIRUnit,
        architecture: ArchitectureConfig,
    ) -> Self {
        Self {
            global,
            architecture,

            module: MIRModuleBuilder::new(architecture),
            symbols: HMIRSymbolEnv::new(&global.hmir, unit),
            types: HMIRResolver::new(),
        }
    }

    pub fn realize_type(&mut self, kind: HMIRTypeKind, span: &TokenRange) -> HMIRTypeID {
        self.types.realize_type(kind, span)
    }

    pub fn same_type(&self, a: HMIRTypeID, b: HMIRTypeID) -> bool {
        if a == b {
            return true;
        }

        match (self.resolve_type_id(a).kind(), self.resolve_type_id(b).kind()) {
            (HMIRTypeKind::Void, HMIRTypeKind::Void)
            | (HMIRTypeKind::Unreachable, HMIRTypeKind::Unreachable)
            | (HMIRTypeKind::Type, HMIRTypeKind::Type)
            | (HMIRTypeKind::Str, HMIRTypeKind::Str) => true,

            (HMIRTypeKind::Int(a), HMIRTypeKind::Int(b)) => a == b,
            (HMIRTypeKind::Float(a), HMIRTypeKind::Float(b)) => a == b,

            (HMIRTypeKind::PointerTo(a), HMIRTypeKind::PointerTo(b))
            | (HMIRTypeKind::ReferenceTo(a), HMIRTypeKind::ReferenceTo(b)) => {
                self.same_type(*a, *b)
            }

            (
                HMIRTypeKind::Array {
                    element: a,
                    length: a_length,
                },
                HMIRTypeKind::Array {
                    element: b,
                    length: b_length,
                },
            ) => a_length == b_length && self.same_type(*a, *b),

            _ => false,
        }
    }

    pub fn eval_type(&mut self, body: &HMIRBody, expr: HMIRExprID) -> CXResult<HMIRTypeID> {
        let expr = body.expr(expr);
        let span = expr.span();

        match expr.kind() {
            HMIRExprKind::Constant(HMIRConstant::Type(id)) => Ok(*id),

            HMIRExprKind::Def(HMIRDefRef::Local(def)) => {
                match self.symbols.resolve_definition(*def).kind() {
                    HMIRDefKind::Type(id) => Ok(*id),
                    _ => todo!("eval_type: local definition {def} is not a type"),
                }
            }

            HMIRExprKind::Comptime(inner) => self.eval_type(body, *inner),

            HMIRExprKind::Op(HMIROp::Type(op)) => match op {
                HMIRTypeOp::Pointer(inner) => {
                    let inner = self.eval_type(body, *inner)?;
                    Ok(self.realize_type(HMIRTypeKind::PointerTo(inner), span))
                }
                HMIRTypeOp::Reference(inner) => {
                    let inner = self.eval_type(body, *inner)?;
                    Ok(self.realize_type(HMIRTypeKind::ReferenceTo(inner), span))
                }
                HMIRTypeOp::PointerInner(operand) => {
                    let ty = self.eval_type(body, *operand)?;
                    match self.pointer_inner(&ty) {
                        Some(inner) => Ok(inner),
                        None => todo!("eval_type: type.pointer_inner of a non-pointer type"),
                    }
                }
                HMIRTypeOp::ReferenceInner(operand) => {
                    let ty = self.eval_type(body, *operand)?;
                    match self.reference_inner(&ty) {
                        Some(inner) => Ok(inner),
                        None => todo!("eval_type: type.reference_inner of a non-reference type"),
                    }
                }
                HMIRTypeOp::Array { element, length } => {
                    let element = self.eval_type(body, *element)?;
                    let length = length.map(|length| eval_length(body, length));
                    Ok(self.realize_type(HMIRTypeKind::Array { element, length }, span))
                }
                HMIRTypeOp::Const(inner) => self.eval_type(body, *inner),
                other => todo!("eval_type: type operation '{}'", other.path()),
            },

            other => todo!("eval_type: expression {other:?}"),
        }
    }

    pub fn lower_type(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<MIRTypeID> {
        if let Some(id) = self.types.lowered_type(ty) {
            return Ok(id);
        }

        let kind = match self.resolve_type_id(ty).kind().clone() {
            HMIRTypeKind::Void | HMIRTypeKind::Unreachable => MIRTypeKind::Void,
            HMIRTypeKind::Type => {
                return Err(hmir_error(
                    span,
                    &typecheck::COMPTIME_ONLY_TYPE,
                    "type".to_string(),
                ));
            }
            HMIRTypeKind::StagedExpr { .. } => {
                return Err(hmir_error(
                    span,
                    &typecheck::COMPTIME_ONLY_TYPE,
                    "expr".to_string(),
                ));
            }
            HMIRTypeKind::Str => MIRTypeKind::Str,
            HMIRTypeKind::Int(int) => MIRTypeKind::Integer {
                ty: mir_int(int.width()),
            },
            HMIRTypeKind::Float(float) => MIRTypeKind::Float {
                ty: mir_float(float.width()),
            },
            HMIRTypeKind::PointerTo(inner) => MIRTypeKind::PointerTo {
                inner: self.lower_type(inner, span)?,
            },
            HMIRTypeKind::ReferenceTo(inner) => MIRTypeKind::MemoryReference {
                inner: self.lower_type(inner, span)?,
            },
            HMIRTypeKind::Array { element, length } => {
                let inner = self.lower_type(element, span)?;
                match length {
                    Some(length) => MIRTypeKind::Array {
                        length: length as usize,
                        inner,
                    },
                    None => MIRTypeKind::IncompleteArray { inner },
                }
            }
            HMIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },

            HMIRTypeKind::Struct { .. } => todo!("lower_type: struct types"),
            HMIRTypeKind::Union { .. } => todo!("lower_type: union types"),
            HMIRTypeKind::TaggedUnion { .. } => todo!("lower_type: tagged union types"),
            HMIRTypeKind::Function(_) => todo!("lower_type: function types"),
        };

        let id = self.module.intern_type(MIRType::new(kind));
        self.types.cache_lowered(ty, id);
        Ok(id)
    }

    pub fn lower_constant(
        &mut self,
        constant: &HMIRConstant,
        span: &TokenRange,
    ) -> CXResult<MIRConstant> {
        Ok(match constant {
            HMIRConstant::Unit => MIRConstant::Unit,
            HMIRConstant::Int { value, ty } => match self.resolve_type_id(*ty).kind() {
                HMIRTypeKind::Int(int) => MIRConstant::Integer {
                    ty: mir_int(int.width()),
                    value: *value,
                },
                other => todo!("lower_constant: integer constant of type {other:?}"),
            },
            HMIRConstant::Float { value, ty } => match self.resolve_type_id(*ty).kind() {
                HMIRTypeKind::Float(float) => MIRConstant::Float {
                    value: *value,
                    ty: mir_float(float.width()),
                },
                other => todo!("lower_constant: float constant of type {other:?}"),
            },
            HMIRConstant::Null(ty) => MIRConstant::Nullptr {
                ty: self.lower_type(*ty, span)?,
            },
            HMIRConstant::Str(value) => MIRConstant::String(value.clone()),
            HMIRConstant::Type(_) => {
                return Err(hmir_error(
                    span,
                    &typecheck::COMPTIME_ONLY_TYPE,
                    "type".to_string(),
                ));
            }
            other => todo!("lower_constant: constant {other:?}"),
        })
    }

    pub fn module(&self) -> &MIRModuleBuilder {
        &self.module
    }

    pub fn module_mut(&mut self) -> &mut MIRModuleBuilder {
        &mut self.module
    }

    pub fn symbols(&self) -> &HMIRSymbolEnv<'global, 'hmir> {
        &self.symbols
    }

    pub fn symbols_mut(&mut self) -> &mut HMIRSymbolEnv<'global, 'hmir> {
        &mut self.symbols
    }

    pub fn finish(self) -> MIRUnit {
        self.module.finish()
    }
}

impl HMIRTypeContext for HMIREnvironment<'_, '_> {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType {
        match self.types.realized_type(id) {
            Some(ty) => ty,
            None => self.symbols.local_unit().resolve_type(id),
        }
    }
}

fn eval_length(body: &HMIRBody, expr: HMIRExprID) -> u64 {
    match body.expr(expr).kind() {
        HMIRExprKind::Constant(HMIRConstant::Int { value, .. }) => (*value).max(0) as u64,
        HMIRExprKind::Comptime(inner) => eval_length(body, *inner),
        other => todo!("eval_type: array length expression {other:?}"),
    }
}

fn mir_int(width: HMIRIntWidth) -> MIRIntType {
    match width {
        HMIRIntWidth::I1 => MIRIntType::I1,
        HMIRIntWidth::I8 => MIRIntType::I8,
        HMIRIntWidth::I16 => MIRIntType::I16,
        HMIRIntWidth::I32 => MIRIntType::I32,
        HMIRIntWidth::I64 => MIRIntType::I64,
        HMIRIntWidth::I128 => MIRIntType::I128,
    }
}

fn mir_float(width: HMIRFloatWidth) -> MIRFloatType {
    match width {
        HMIRFloatWidth::F32 => MIRFloatType::F32,
        HMIRFloatWidth::F64 => MIRFloatType::F64,
    }
}
