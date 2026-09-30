use cx_hir::ast::{
    function::{
        HIRComptimeFnPrototype, HIRFunctionBody, HIRFunctionContract, HIRFunctionPrototype,
    },
    global_var::HIREnumVariant,
    template::HIRTemplatePrototype,
    types::HIRType,
};
use cx_hmir::{
    HMIRBinaryOp, HMIRBlockKind, HMIRConstant, HMIRContract, HMIRControlOp, HMIRDefKind,
    HMIRExprID, HMIRExprKind, HMIRFunction, HMIRGlobal, HMIRIntWidth, HMIRLocalID,
    HMIRNativeOp, HMIRSignature, HMIRTypeDesc,
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    body::BodyLowering,
    plan::{DefSource, PlannedDef},
};

impl BodyLowering<'_> {
    pub(crate) fn lower_def(mut self, plan: &PlannedDef) -> HMIRDefKind {
        let span = plan.span();
        match plan.source() {
            DefSource::OpaqueType => {
                let ty_of = self.type_of_types(span);
                HMIRDefKind::Global(Box::new(HMIRGlobal::new(
                    self.finish(),
                    ty_of,
                    None,
                    false,
                    LinkageMode::Standard,
                )))
            }
            DefSource::Type { template: None, ty } => {
                let ty_of = self.type_of_types(span);
                let initializer = self.lower_type(ty);
                HMIRDefKind::Global(Box::new(HMIRGlobal::new(
                    self.finish(),
                    ty_of,
                    Some(initializer),
                    false,
                    LinkageMode::Standard,
                )))
            }
            DefSource::Type {
                template: Some(template),
                ty,
            } => self.lower_type_generator(template, ty, span),
            DefSource::Function {
                prototype,
                template,
                body,
            } => self.lower_function(prototype, *template, *body),
            DefSource::ComptimeFunction {
                prototype,
                template,
                body,
            } => self.lower_comptime_function(prototype, *template, body),
            DefSource::Global {
                ty,
                mutable,
                initializer,
                linkage,
            } => {
                let ty = self.lower_type(ty);
                let initializer = initializer.map(|initializer| self.lower_expr(initializer));
                HMIRDefKind::Global(Box::new(HMIRGlobal::new(
                    self.finish(),
                    ty,
                    initializer,
                    *mutable,
                    *linkage,
                )))
            }
            DefSource::EnumVariant { variants, index } => {
                self.lower_enum_variant(plan.name(), variants, *index)
            }
        }
    }

    fn lower_type_generator(
        mut self,
        template: &HIRTemplatePrototype,
        ty: &HIRType,
        span: &TokenRange,
    ) -> HMIRDefKind {
        let (params, return_type, root) = self.with_stage(true, |this| {
            let params = this.template_params(Some(template), span);
            let return_type = this.type_of_types(span);
            let ty = this.lower_type(ty);
            (params, return_type, this.returning_block(ty, span))
        });
        let signature = HMIRSignature::new(
            params,
            return_type,
            false,
            LinkageMode::Standard,
            HMIRContract::default(),
        );
        HMIRDefKind::Function(Box::new(HMIRFunction::new(
            self.finish(),
            signature,
            Some(root),
        )))
    }

    fn lower_function(
        mut self,
        prototype: &HIRFunctionPrototype,
        template: Option<&HIRTemplatePrototype>,
        body: Option<&HIRFunctionBody>,
    ) -> HMIRDefKind {
        let mut params = self.template_params(template, &prototype.range);
        for param in &prototype.params {
            let ty = self.lower_type(&param.ty);
            params.push(self.declare(param.name.as_ref(), ty, false, false, &param.ty.range));
        }
        let return_type = self.lower_type(&prototype.return_type);
        let contract = self.lower_contract(&prototype.contract, &prototype.range);
        let root = body.map(|body| self.lower_function_body(body));
        let signature = HMIRSignature::new(
            params,
            return_type,
            prototype.var_args,
            prototype.linkage,
            contract,
        );
        HMIRDefKind::Function(Box::new(HMIRFunction::new(self.finish(), signature, root)))
    }

    fn lower_comptime_function(
        mut self,
        prototype: &HIRComptimeFnPrototype,
        template: Option<&HIRTemplatePrototype>,
        body: &HIRFunctionBody,
    ) -> HMIRDefKind {
        let (params, return_type, root) = self.with_stage(true, |this| {
            let mut params = this.template_params(template, &prototype.range);
            for param in &prototype.params {
                let ty = this.lower_comptime_value_type(&param.value_type);
                params.push(this.declare(
                    param.name.as_ref(),
                    ty,
                    true,
                    param.value_type.expr,
                    &param.value_type.ty.range,
                ));
            }
            let return_type = this.lower_comptime_value_type(&prototype.return_type);
            (params, return_type, this.lower_function_body(body))
        });
        let signature = HMIRSignature::new(
            params,
            return_type,
            false,
            LinkageMode::Standard,
            HMIRContract::default(),
        );
        HMIRDefKind::Function(Box::new(HMIRFunction::new(
            self.finish(),
            signature,
            Some(root),
        )))
    }

    fn lower_enum_variant(
        mut self,
        name: &QualifiedName,
        variants: &[HIREnumVariant],
        index: usize,
    ) -> HMIRDefKind {
        let span = TokenRange::internal();
        let int = HMIRTypeDesc::Int {
            width: HMIRIntWidth::I32,
            signed: true,
        };
        let ty = self.type_constant(int.clone(), &span);
        let initializer = match (&variants[index].value, index) {
            (Some(value), _) => self.lower_expr(value),
            (None, 0) => self.int_constant(int, 0, &span),
            (None, _) => {
                let previous = QualifiedName::new(
                    name.namespace.clone(),
                    variants[index - 1].name.clone(),
                );
                let lhs = self.def_expr(previous, &span);
                let rhs = self.int_constant(int, 1, &span);
                self.native(
                    HMIRNativeOp::BinOp {
                        op: HMIRBinaryOp::Add,
                        lhs,
                        rhs,
                    },
                    &span,
                )
            }
        };
        HMIRDefKind::Global(Box::new(HMIRGlobal::new(
            self.finish(),
            ty,
            Some(initializer),
            false,
            LinkageMode::Standard,
        )))
    }

    fn template_params(
        &mut self,
        template: Option<&HIRTemplatePrototype>,
        span: &TokenRange,
    ) -> Vec<HMIRLocalID> {
        let Some(template) = template else {
            return Vec::new();
        };
        template
            .types
            .iter()
            .map(|name| {
                let ty = self.type_of_types(span);
                self.declare(Some(name), ty, true, false, span)
            })
            .collect()
    }

    fn lower_contract(&mut self, contract: &HIRFunctionContract, span: &TokenRange) -> HMIRContract {
        let precondition = contract
            .precondition
            .as_ref()
            .map(|condition| self.lower_expr(condition));
        let postcondition = contract.postcondition.as_ref().map(|(binding, condition)| {
            self.scoped(|this| {
                let binding = binding.as_ref().map(|name| {
                    let ty = this.hole(span);
                    this.declare_local(Some(name), ty, span)
                });
                (binding, this.lower_expr(condition))
            })
        });
        HMIRContract::new(contract.safe, precondition, postcondition)
    }

    fn lower_function_body(&mut self, body: &HIRFunctionBody) -> HMIRExprID {
        match body {
            HIRFunctionBody::Block { statements, range } => self.scoped(|this| {
                let statements = statements
                    .iter()
                    .map(|statement| this.lower_expr(statement))
                    .collect();
                this.block(HMIRBlockKind::Scope, statements, range)
            }),
            HIRFunctionBody::Expression(expr) => {
                let value = self.lower_expr(expr);
                self.returning_block(value, &expr.range)
            }
        }
    }

    fn returning_block(&mut self, value: HMIRExprID, span: &TokenRange) -> HMIRExprID {
        let ret = self.control(HMIRControlOp::Return(Some(value)), span);
        self.block(HMIRBlockKind::Scope, vec![ret], span)
    }

    fn int_constant(&mut self, desc: HMIRTypeDesc, value: i128, span: &TokenRange) -> HMIRExprID {
        let ty = self.intern(desc);
        self.push(HMIRExprKind::Constant(HMIRConstant::Int { value, ty }), span)
    }
}
