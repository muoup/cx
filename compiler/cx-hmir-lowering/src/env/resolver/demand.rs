use rustc_hash::FxHashSet;
use cx_hmir::{
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{flow, types},
    log::hmir_error,
    value::{HMIRValueKind, constant::HMIRConstant},
};

use super::{HMIRResolver, memo::MemoKey};

impl LocalLoweringContext<'_, '_, '_> {
    pub fn deferred_type(&mut self, expr: HMIRExprID) -> HMIRTypeID {
        let owner = self.source();
        let unit = self
            .env_mut()
            .symbols_mut()
            .owner(owner)
            .expect("expression owner exists");
        let point = unit
            .body(expr.body())
            .and_then(|body| body.visible_through())
            .or(self.visible_through());
        let scope = self.memo_context(point, None, true);
        let id = self
            .env_mut()
            .resolver_mut()
            .reserve(MemoKey::Expression(expr, scope));
        HMIRResolver::type_id(id)
    }

    pub fn type_visible(&self, ty: HMIRTypeID) -> bool {
        let ty = self.env().underlying(ty);
        match (
            self.visible_through(),
            self.env().resolver().source_point(ty),
        ) {
            (Some(at), Some(declared)) if at.brand() == declared.brand() => {
                declared.index() <= at.index()
            }
            _ => true,
        }
    }

    pub fn complete_object(&self, ty: HMIRTypeID) -> bool {
        self.type_visible(ty) && types::is_complete(self.env(), ty)
    }

    pub fn prepare_type(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<()> {
        let mut seen = FxHashSet::default();
        let mut complete = true;
        self.prepare_type_inner(ty, span, &mut seen, &mut complete)?;
        if complete {
            for ty in seen {
                self.env_mut().resolver_mut().mark_prepared(ty);
            }
        }
        Ok(())
    }

    fn prepare_type_inner(
        &mut self,
        ty: HMIRTypeID,
        span: &TokenRange,
        seen: &mut FxHashSet<HMIRTypeID>,
        complete: &mut bool,
    ) -> CXResult<()> {
        if self.env().resolver().prepared(ty) || !seen.insert(ty) {
            return Ok(());
        }
        self.force_type(ty, span)?;
        if self.env().resolver().is_incomplete(ty) {
            *complete = false;
            return Ok(());
        }
        let kind = self
            .env()
            .resolver()
            .realized_type(ty)
            .expect("type expression completed")
            .kind()
            .clone();
        let children = match kind {
            HMIRTypeKind::PointerTo(inner)
            | HMIRTypeKind::ReferenceTo { inner, .. }
            | HMIRTypeKind::Nominal(inner)
            | HMIRTypeKind::Staged(inner) => vec![inner],
            HMIRTypeKind::Array { element, .. } => vec![element],
            HMIRTypeKind::Function(signature) => signature
                .params()
                .iter()
                .map(|param| *param.ty())
                .chain(std::iter::once(*signature.return_type()))
                .collect(),
            HMIRTypeKind::Struct { fields }
            | HMIRTypeKind::Union { fields }
            | HMIRTypeKind::TaggedUnion { fields } => {
                fields.iter().map(|field| *field.ty()).collect()
            }
            _ => Vec::new(),
        };
        for child in children {
            self.prepare_type_inner(child, span, seen, complete)?;
        }
        let defined = self
            .env()
            .resolver()
            .realized_type(ty)
            .expect("prepared type exists");
        types::validate(self.env(), defined)?;
        Ok(())
    }

    pub fn force_type(&mut self, ty: HMIRTypeID, span: &TokenRange) -> CXResult<()> {
        self.force_type_inner(ty, span, &mut FxHashSet::default())
    }

    fn force_type_inner(
        &mut self,
        ty: HMIRTypeID,
        span: &TokenRange,
        seen: &mut FxHashSet<HMIRTypeID>,
    ) -> CXResult<()> {
        if !seen.insert(ty) {
            return Err(self
                .env()
                .resolver()
                .cycle(HMIRResolver::type_memo(ty), span));
        }
        if let Some(defined) = self.env().resolver().realized_type(ty) {
            if let HMIRTypeKind::Nominal(inner) = defined.kind() {
                return self.force_type_inner(*inner, span, seen);
            }
            let pending = self.env().resolver().is_incomplete(ty)
                && self.env().resolver().pending(HMIRResolver::type_memo(ty));
            if !pending || !self.type_visible(ty) {
                return Ok(());
            }
        }
        let value = flow::top(self.evaluate(HMIRResolver::type_memo(ty), span))?;
        let HMIRValueKind::Constant(HMIRConstant::Type(target)) = value.kind() else {
            return Err(hmir_error(
                span,
                &typecheck::EXPECTED_TYPE,
                format!(
                    "expression of type '{}'",
                    types::name(self.env(), value.ty())
                ),
            ));
        };
        let target = *target;
        if target != ty {
            self.force_type_inner(target, span, seen)?;
            self.env_mut().resolver_mut().alias_type(ty, target, span);
        } else if self.env().resolver().realized_type(ty).is_none() {
            return Err(self
                .env()
                .resolver()
                .cycle(HMIRResolver::type_memo(ty), span));
        }
        Ok(())
    }
}
