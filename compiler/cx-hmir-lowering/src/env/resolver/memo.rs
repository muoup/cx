use cx_hmir::{
    HMIRDefID, HMIRUnitID,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::HMIRTypeID,
    unit::def::HMIRDeclarationPoint,
};
use cx_log::catalogue::mir;
use cx_tokens::TokenRange;
use cx_util::dense_id;

use crate::{
    env::function::{ComptimeContext, Expectations, FnContext, LocalLoweringContext},
    eval::{self, coerce, flow},
    log::hmir_error,
    symbol,
    value::HMIRValue,
};

use super::HMIRResolver;

dense_id!(MemoID, "memo");
dense_id!(MemoContextID, "context");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MemoContext {
    owner: HMIRUnitID,
    visible_through: Option<HMIRDeclarationPoint>,
    bindings: Vec<(HMIRLocalID, HMIRValue)>,
    expected: Option<HMIRTypeID>,
    expectations: Expectations,
    initializer: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum MemoKey {
    Definition(HMIRDefID, MemoContextID),
    Expression(HMIRExprID, MemoContextID),
}

#[derive(Debug)]
pub(crate) struct MemoEntry {
    key: Option<MemoKey>,
    state: MemoState,
}

#[derive(Debug)]
enum MemoState {
    Reserved,
    Evaluating(Option<HMIRValue>),
    Ready(HMIRValue),
}

impl MemoEntry {
    pub fn reserved(key: MemoKey) -> Self {
        Self {
            key: Some(key),
            state: MemoState::Reserved,
        }
    }
    pub fn ready(value: HMIRValue) -> Self {
        Self {
            key: None,
            state: MemoState::Ready(value),
        }
    }
}

impl HMIRResolver {
    pub fn context(&self, id: MemoContextID) -> &MemoContext {
        &self.contexts[id]
    }
    pub fn publish(&mut self, id: MemoID, value: HMIRValue) {
        assert!(matches!(self.entries[id].state, MemoState::Evaluating(_)));
        self.entries[id].state = MemoState::Evaluating(Some(value));
    }
    pub fn pending(&self, id: MemoID) -> bool {
        matches!(self.entries[id].state, MemoState::Reserved)
    }
    pub fn source_point(&self, ty: HMIRTypeID) -> Option<HMIRDeclarationPoint> {
        match &self.entries[Self::type_memo(ty)].key {
            Some(MemoKey::Expression(_, scope)) => self.context(*scope).visible_through,
            _ => None,
        }
    }
    pub fn origin(&self, ty: HMIRTypeID) -> Option<(HMIRExprID, &[(HMIRLocalID, HMIRValue)])> {
        match &self.entries[Self::type_memo(ty)].key {
            Some(MemoKey::Expression(expr, scope)) => {
                Some((*expr, self.context(*scope).bindings.as_slice()))
            }
            _ => None,
        }
    }
    pub fn cycle(&self, id: MemoID, span: &TokenRange) -> cx_log::error::CXError {
        let start = self
            .active
            .iter()
            .position(|active| *active == id)
            .unwrap_or(0);
        let path = self.active[start..]
            .iter()
            .chain(std::iter::once(&id))
            .map(|id| match &self.entries[*id].key {
                Some(MemoKey::Definition(def, _)) => def.to_string(),
                Some(MemoKey::Expression(expr, _)) => format!("expression {expr}"),
                None => id.to_string(),
            })
            .collect::<Vec<_>>()
            .join(" -> ");
        hmir_error(span, &mir::DEPENDENCY_CYCLE, path)
    }
}

impl LocalLoweringContext<'_, '_, '_> {
    pub fn memo_context(
        &mut self,
        visible_through: Option<HMIRDeclarationPoint>,
        expected: Option<HMIRTypeID>,
        initializer: bool,
    ) -> MemoContextID {
        let mut bindings = eval::comptime::frozen_inputs(self);
        for (_, value) in &mut bindings {
            *value = self.env_mut().canonical_value(value.clone());
        }
        bindings.sort_by_key(|(local, _)| *local);
        let context = MemoContext {
            owner: self.source(),
            visible_through,
            bindings,
            expected,
            expectations: self.expectations(),
            initializer,
        };
        self.env_mut().resolver_mut().intern_context(context)
    }

    pub fn evaluate(&mut self, id: MemoID, span: &TokenRange) -> eval::EvalResult<HMIRValue> {
        let resolver = self.env().resolver();
        match &resolver.entries[id].state {
            MemoState::Ready(value) | MemoState::Evaluating(Some(value)) => {
                return Ok(flow::Evaluation::Ready(value.clone()));
            }
            MemoState::Evaluating(None) => return Err(resolver.cycle(id, span)),
            MemoState::Reserved => {}
        }
        let key = resolver.entries[id]
            .key
            .clone()
            .expect("a reserved entry has a source");
        let (owner, scope) = match key {
            MemoKey::Definition(def, scope) => (def.brand(), scope),
            MemoKey::Expression(_, scope) => (resolver.context(scope).owner, scope),
        };
        let captured = resolver.context(scope).clone();
        {
            let resolver = self.env_mut().resolver_mut();
            resolver.entries[id].state = MemoState::Evaluating(None);
            resolver.active.push(id);
        }
        let result = (|| {
            let evaluation = self.evaluation();
            let mut child = self
                .child(
                    None,
                    FnContext::Comptime(ComptimeContext::new()),
                    Some(evaluation),
                )
                .sourced(owner)
                .at(captured.visible_through);
            for (local, value) in captured.bindings {
                crate::emit::storage::bind_local(&mut child, local, value);
            }
            let expectations = match key {
                MemoKey::Definition(..) => Expectations::new(captured.expectations.permission()),
                MemoKey::Expression(..) => captured.expectations,
            };
            child.with_expectations(expectations, |child| match key {
                MemoKey::Definition(def, _) => {
                    symbol::definition(child, def, scope, span).map(flow::Evaluation::Ready)
                }
                MemoKey::Expression(expr, _) => expression(
                    child,
                    id,
                    expr,
                    captured.expected,
                    captured.initializer,
                    span,
                ),
            })
        })();
        let resolver = self.env_mut().resolver_mut();
        assert_eq!(resolver.active.pop(), Some(id));
        match result {
            Ok(flow::Evaluation::Ready(value)) => {
                resolver.entries[id].state = MemoState::Ready(value.clone());
                Ok(flow::Evaluation::Ready(value))
            }
            result => {
                resolver.entries[id].state = MemoState::Reserved;
                result
            }
        }
    }
}

fn expression(
    context: &mut LocalLoweringContext,
    id: MemoID,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
    initializer: bool,
    span: &TokenRange,
) -> eval::EvalResult<HMIRValue> {
    let owner = context.source();
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(owner)
        .expect("memo source owner exists");
    let body = unit
        .body(expr.body())
        .expect("memo expression belongs to its owner");
    if let HMIRExprKind::Type(ty) = body.expr(expr).kind() {
        let ty = eval::op::construct_type(context, body, ty, id)?;
        return eval::type_value(context, ty, span).map(flow::Evaluation::Ready);
    }
    let result = eval::eval(context, body, expr)?.complete(context, body, expected)?;
    let partial = result
        .known()
        .filter(|value| eval::type_constant(value).is_some())
        .cloned();
    let span = span.clone();
    result.and_then(context, body, partial, move |context, _, value, _| {
        let value = match expected {
            Some(expected) => coerce::implicit(context, value, expected, &span)?,
            None if initializer && eval::type_constant(&value).is_none() => {
                coerce::std_rvalue(context, value, &span)?
            }
            None => value,
        };
        Ok(flow::Evaluation::Ready(value))
    })
}
