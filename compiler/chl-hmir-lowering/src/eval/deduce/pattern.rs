use rustc_hash::FxHashSet;

use chl_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRDefKind, HMIRFunction, HMIRUnitID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRExprTypeKind, aggregate::HMIRDeclaredMove},
};
use chl_log::{
    CHLResult,
    catalogue::{mir, typecheck},
};
use chl_tokens::TokenRange;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{eval_type, expr::name, flow},
    log::hmir_error,
    lower::declaration,
};

#[derive(Clone, Copy)]
pub(super) struct Form<'a> {
    pub(super) body: &'a HMIRBodyContext,
    pub(super) expr: HMIRExprID,
    pub(super) scope: Option<&'a AliasScope<'a>>,
    pub(super) source: Option<HMIRUnitID>,
    pub(super) argument: bool,
}

pub(super) struct AliasScope<'a> {
    pub(super) def: HMIRDefID,
    pub(super) function: &'a HMIRFunction,
    pub(super) arguments: Vec<Option<Form<'a>>>,
    pub(super) parent: Option<&'a AliasScope<'a>>,
}

impl<'a> Form<'a> {
    pub(super) fn new(body: &'a HMIRBodyContext, expr: HMIRExprID) -> Self {
        Self {
            body,
            expr,
            scope: None,
            source: None,
            argument: false,
        }
    }

    pub(super) fn at(self, expr: HMIRExprID) -> Self {
        Self {
            expr,
            argument: false,
            ..self
        }
    }

    pub(super) fn substitute(mut self) -> Option<Self> {
        let argument = self.argument;
        while let Some(scope) = self.scope
            && let HMIRExprKind::Local(local) = self.body.expr(self.expr).kind()
            && let Some(index) = scope
                .function
                .params()
                .iter()
                .position(|param| param == local)
        {
            self = scope.arguments[index]?;
        }
        self.argument = argument;
        Some(self)
    }
}

impl AliasScope<'_> {
    pub(super) fn contains(&self, def: HMIRDefID) -> bool {
        let mut scope = Some(self);
        while let Some(current) = scope {
            if current.def == def {
                return true;
            }
            scope = current.parent;
        }
        false
    }
}

pub(crate) fn validate(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    span: &TokenRange,
) -> CHLResult<()> {
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(id.brand());
    Validation::default().alias(&mut context, id, span)
}

#[derive(Default)]
struct Validation {
    active: FxHashSet<HMIRDefID>,
    checked: FxHashSet<HMIRDefID>,
}

impl Validation {
    fn alias(
        &mut self,
        context: &mut LocalLoweringContext,
        def: HMIRDefID,
        span: &TokenRange,
    ) -> CHLResult<()> {
        if self.checked.contains(&def) {
            return Ok(());
        }
        let unit = context.unit(def.brand());
        let source = unit
            .resolve_def(def)
            .expect("alias owner contains its definition");
        let Some(HMIRDefKind::Function(function)) = declaration::canonical(source).kind() else {
            return Err(invalid(span, "a call to a non-transparent function"));
        };
        let Some(root) = function.transparent_body() else {
            return Err(invalid(span, "a call to a non-transparent function"));
        };
        if !self.active.insert(def) {
            return Err(hmir_error(
                span,
                &mir::DEPENDENCY_CYCLE,
                source.name().to_string(),
            ));
        }
        context.relocated(def.brand(), |context| {
            self.expr(context, function, root, true)
        })?;
        self.active.remove(&def);
        self.checked.insert(def);
        Ok(())
    }

    fn expr(
        &mut self,
        context: &mut LocalLoweringContext,
        function: &HMIRFunction,
        expr: HMIRExprID,
        type_position: bool,
    ) -> CHLResult<()> {
        let body = function.body();
        let expression = body.expr(expr);
        match expression.kind() {
            HMIRExprKind::Local(local)
                if !type_position || body.is_type_kind(body.local(*local).ty()) =>
            {
                Ok(())
            }
            HMIRExprKind::Literal(_) if !type_position => Ok(()),
            HMIRExprKind::Def(_) | HMIRExprKind::Name { .. } if type_position => {
                flow::top(eval_type(context, body, expr).and_then(|ty| ty.description())).map(drop)
            }
            HMIRExprKind::Def(_) | HMIRExprKind::Name { .. } => Ok(()),
            HMIRExprKind::Type(ty)
                if !matches!(
                    ty.kind(),
                    HMIRExprTypeKind::Struct { .. }
                        | HMIRExprTypeKind::Union { .. }
                        | HMIRExprTypeKind::TaggedUnion { .. }
                        | HMIRExprTypeKind::Dependent
                ) && !matches!(
                    ty.semantics().move_semantics,
                    HMIRDeclaredMove::CopyTraits(_)
                ) =>
            {
                ty.kind().map(
                    &mut (self, context),
                    |(this, context), expr| this.expr(context, function, *expr, true),
                    |(this, context), expr| this.expr(context, function, *expr, false),
                )?;
                Ok(())
            }
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => {
                let callee = body.expr(*callee);
                let def = match callee.kind() {
                    HMIRExprKind::Def(def) => Some(*def),
                    HMIRExprKind::Name { name, tag } => {
                        name::resolve(context, callee.span(), name, *tag)?
                    }
                    _ => None,
                };
                let Some(def) = def else {
                    return Err(invalid(
                        callee.span(),
                        "a call to a non-transparent function",
                    ));
                };
                self.alias(context, def, callee.span())?;
                for arg in args.iter().chain(named.iter().map(|(_, arg)| arg)) {
                    self.expr(context, function, *arg, false)?;
                }
                Ok(())
            }
            _ => Err(invalid(expression.span(), "this expression")),
        }
    }
}

fn invalid(span: &TokenRange, feature: &str) -> chl_log::error::CHLError {
    let args = (feature.into(), "a transparent type alias".into());
    hmir_error(span, &typecheck::INVALID_CONTEXT, args)
}
