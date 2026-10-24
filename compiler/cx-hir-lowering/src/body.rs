use std::{collections::HashMap, mem, rc::Rc};

use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    types::{HIRTagKind, HIRType, HIRTypeKind},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBody, HMIRCoerceMode, HMIRContract, HMIRControlOp, HMIRDefID, HMIRDefKind,
    HMIRFnSignature, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIROp, HMIROwnershipOp,
    body::{HMIRHole, HMIRLocal, HMIRLocalID},
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind, literal::HMIRLiteral, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRTypeSemantics},
        intrinsic::HMIRIntType,
    },
    unit::function::{HMIRFnDefinition, HMIRFnParam, HMIRFnPrototype},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    def::declared_params,
    expr::{lower_expr, lower_initial_value},
    log::log_hir_error,
    ty::lower_type,
    unit::UnitContext,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct Binding {
    local: HMIRLocalID,
    quoted: bool,
}

#[derive(Debug, Clone, Copy)]
enum ScopeEntry {
    Local(Binding),
    Static(HMIRDefID),
}

pub(crate) enum Symbol {
    Local(Binding),
    Def(HMIRDefID),
    Name,
}

pub(crate) enum ScopeKind {
    Plain,
    // Names bound by the arguments of a call stay in it; a 'then' among them sees past it
    Arguments,
    Staged,
    Block {
        statements: Rc<[HIRExpression]>,
        next: usize,
    },
    Switch {
        has_default: bool,
    },
}

struct Scope {
    symbols: HashMap<CXIdent, ScopeEntry>,
    kind: ScopeKind,
}

pub(crate) struct BodyLowering<'a> {
    unit: &'a UnitContext,
    namespace: NamespacePath,
    body: HMIRBody,
    scopes: Vec<Scope>,
    comptime: bool,
    // The def whose labels are in scope
    owner: Option<HMIRDefID>,
    address_labels: Vec<CXIdent>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        Self {
            symbols: HashMap::new(),
            kind,
        }
    }
}

impl Binding {
    pub(crate) fn local(self) -> HMIRLocalID {
        self.local
    }

    pub(crate) fn is_quoted(self) -> bool {
        self.quoted
    }
}

impl<'a> BodyLowering<'a> {
    pub(crate) fn new(unit: &'a UnitContext, namespace: NamespacePath, comptime: bool) -> Self {
        Self {
            unit,
            namespace,
            body: HMIRBody::new(),
            scopes: vec![Scope::new(ScopeKind::Plain)],
            comptime,
            owner: None,
            address_labels: Vec::new(),
        }
    }

    pub(crate) fn at(mut self, point: Option<cx_hmir::unit::def::HMIRDeclarationPoint>) -> Self {
        self.body = self.body.at(point);
        self
    }

    pub(crate) fn owned_by(mut self, owner: HMIRDefID) -> Self {
        self.owner = Some(owner);
        self
    }

    pub(crate) fn label_address(&mut self, name: &CXIdent, span: &TokenRange) -> HMIRExprID {
        if !self.address_labels.contains(name) {
            self.address_labels.push(name.clone());
        }
        let function = self.owner;
        let name = name.clone();
        self.control(HMIRControlOp::LabelAddress { function, name }, span)
    }

    pub(crate) fn unit(&self) -> &'a UnitContext {
        self.unit
    }

    fn bind_static(&mut self, name: &CXIdent, def: HMIRDefID) {
        self.scopes
            .last_mut()
            .expect("body lowering always has a root scope")
            .symbols
            .insert(name.clone(), ScopeEntry::Static(def));
    }

    pub(crate) fn finish(self) -> HMIRBody {
        self.body
    }

    // A lowering for a body of its own, which sees none of the locals of this one
    pub(crate) fn detached(&mut self, comptime: bool) -> BodyLowering<'_> {
        BodyLowering::new(self.unit, self.namespace.clone(), comptime)
            .at(self.body.visible_through())
    }

    // The signature parameters that the locals of a definition were declared from
    pub(crate) fn fn_params(&self, locals: &[HMIRLocalID]) -> Vec<HMIRFnParam<HMIRExprID>> {
        locals
            .iter()
            .map(|local| {
                let local = self.body.local(*local);
                HMIRFnParam::new(local.comptime(), local.ty())
            })
            .collect()
    }

    pub(crate) fn finish_definition(&mut self, root: HMIRExprID) -> HMIRFnDefinition {
        let labels = mem::take(&mut self.address_labels);
        HMIRFnDefinition::new(root, labels)
    }

    pub(crate) fn push(&mut self, kind: HMIRExprKind, span: &TokenRange) -> HMIRExprID {
        self.body.push_expr(HMIRExpr::new(kind, span.clone()))
    }

    pub(crate) fn op(&mut self, op: HMIROp, span: &TokenRange) -> HMIRExprID {
        self.push(HMIRExprKind::Op(op), span)
    }

    pub(crate) fn hole(&mut self, span: &TokenRange) -> HMIRExprID {
        let hole = self.body.declare_hole(HMIRHole::new(span.clone()));
        self.push(HMIRExprKind::Hole(hole), span)
    }

    pub(crate) fn block(
        &mut self,
        statements: Vec<HMIRExprID>,
        scoped: bool,
        yields: bool,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.push(
            HMIRExprKind::Block {
                statements,
                scoped,
                yields,
            },
            span,
        )
    }

    pub(crate) fn def_expr(&mut self, name: QualifiedName, span: &TokenRange) -> HMIRExprID {
        self.push(HMIRExprKind::Name { name, tag: None }, span)
    }

    pub(crate) fn type_ref(&mut self, kind: HMIRExprTypeKind, span: &TokenRange) -> HMIRExprID {
        self.expr_type(kind, HMIRTypeSemantics::default(), None, span)
    }

    pub(crate) fn comptime(&self) -> bool {
        self.comptime
    }

    pub(crate) fn with_stage<T>(&mut self, comptime: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let previous = mem::replace(&mut self.comptime, comptime);
        let result = f(self);
        self.comptime = previous;
        result
    }

    pub(crate) fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scoped_as(ScopeKind::Plain, f)
    }

    pub(crate) fn scoped_as<T>(&mut self, kind: ScopeKind, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(Scope::new(kind));
        let result = f(self);
        self.scopes.pop();
        result
    }

    // The scope of the block a 'then' continues: the innermost one, looking through the staged
    // code and the call arguments the 'then' sits behind
    fn block_scope(&mut self) -> Option<&mut Scope> {
        self.scopes
            .iter_mut()
            .rev()
            .find(|scope| !matches!(scope.kind, ScopeKind::Staged | ScopeKind::Arguments))
            .filter(|scope| matches!(scope.kind, ScopeKind::Block { .. }))
    }

    pub(crate) fn in_block(&mut self) -> bool {
        self.block_scope().is_some()
    }

    // Lowers what remains of the current block. A 'then' calls this from within a statement of
    // that block, which leaves the block's own call with nothing further to lower.
    pub(crate) fn lower_block_statements(&mut self) -> CXResult<Vec<HMIRExprID>> {
        let mut lowered = Vec::new();
        loop {
            let Some(Scope {
                kind: ScopeKind::Block { statements, next },
                ..
            }) = self.block_scope()
            else {
                break;
            };
            if *next == statements.len() {
                break;
            }
            let statements = statements.clone();
            *next += 1;
            let index = *next - 1;
            lowered.push(lower_expr(self, &statements[index])?);
        }
        Ok(lowered)
    }

    fn enclosing_switch(&mut self) -> Option<&mut bool> {
        self.scopes
            .iter_mut()
            .rev()
            .take_while(|scope| !matches!(scope.kind, ScopeKind::Staged))
            .find_map(|scope| match &mut scope.kind {
                ScopeKind::Switch { has_default } => Some(has_default),
                _ => None,
            })
    }

    pub(crate) fn in_switch(&mut self) -> bool {
        self.enclosing_switch().is_some()
    }

    // Records a 'default' label in the enclosing switch, returning whether it is the first
    pub(crate) fn declare_switch_default(&mut self) -> bool {
        self.enclosing_switch()
            .is_some_and(|has_default| !mem::replace(has_default, true))
    }

    pub(crate) fn declare(
        &mut self,
        name: Option<&CXIdent>,
        ty: HMIRExprID,
        comptime: bool,
        quoted: bool,
        span: &TokenRange,
    ) -> HMIRLocalID {
        let local =
            self.body
                .declare_local(HMIRLocal::new(name.cloned(), ty, span.clone(), comptime));
        if let Some(name) = name {
            self.scopes
                .last_mut()
                .expect("body lowering always has a root scope")
                .symbols
                .insert(name.clone(), ScopeEntry::Local(Binding { local, quoted }));
        }
        local
    }

    pub(crate) fn declare_local(
        &mut self,
        name: Option<&CXIdent>,
        ty: HMIRExprID,
        span: &TokenRange,
    ) -> HMIRLocalID {
        self.declare(name, ty, self.comptime, false, span)
    }

    pub(crate) fn lookup(&self, name: &QualifiedName, tag: Option<HIRTagKind>) -> Symbol {
        if tag.is_none()
            && let Some(root) = name.root_name_ref()
            && let Some(entry) = self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.symbols.get(root))
        {
            return match *entry {
                ScopeEntry::Local(binding) => Symbol::Local(binding),
                ScopeEntry::Static(def) => Symbol::Def(def),
            };
        }

        Symbol::Name
    }

    pub(crate) fn returning_block(&mut self, value: HMIRExprID, span: &TokenRange) -> HMIRExprID {
        let ret = self.control(HMIRControlOp::Return(Some(value)), span);
        self.block(vec![ret], true, false, span)
    }

    pub(crate) fn int_literal(
        &mut self,
        ty: HMIRIntType,
        value: i128,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.push(HMIRExprKind::Literal(HMIRLiteral::Int { value, ty }), span)
    }

    pub(crate) fn control(&mut self, op: HMIRControlOp, span: &TokenRange) -> HMIRExprID {
        self.op(HMIROp::Control(op), span)
    }

    pub(crate) fn ownership(&mut self, op: HMIROwnershipOp, span: &TokenRange) -> HMIRExprID {
        self.op(HMIROp::OwnershipOp(op), span)
    }

    pub(crate) fn aggregate_op(&mut self, op: HMIRAggregateOp, span: &TokenRange) -> HMIRExprID {
        self.op(HMIROp::AggregateOp(op), span)
    }

    pub(crate) fn type_of_types(&mut self, span: &TokenRange) -> HMIRExprID {
        self.type_ref(HMIRExprTypeKind::Type, span)
    }

    pub(crate) fn type_op(&mut self, op: HMIRTypeOp, span: &TokenRange) -> HMIRExprID {
        self.op(HMIROp::Type(op), span)
    }

    pub(crate) fn expr_type(
        &mut self,
        kind: HMIRExprTypeKind,
        semantics: HMIRTypeSemantics<HMIRDeclaredMove>,
        name: Option<&CXIdent>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let name = name.map(ToString::to_string);
        let ty = HMIRExprType::new(kind, semantics, span.clone(), name);
        self.push(HMIRExprKind::Type(Box::new(ty)), span)
    }

    pub(crate) fn coerce(
        &mut self,
        mode: HMIRCoerceMode,
        value: HMIRExprID,
        target: HMIRExprID,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.op(
            HMIROp::Coerce {
                mode,
                value,
                target,
            },
            span,
        )
    }
}

// '@reify(signature, value)' is a function of its own: its parameters are handed to the staged
// value, whose code becomes the body. It is lowered outside the enclosing function, so the code
// can only name what a function at file scope could.
pub(crate) fn lower_reify(
    context: &mut BodyLowering<'_>,
    signature: &HIRExpression,
    value: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let prototype = match &signature.kind {
        HIRExprKind::Type(ty) => match &ty.kind {
            HIRTypeKind::PointerTo { inner_type } => match &inner_type.kind {
                HIRTypeKind::FunctionPointer { prototype } => Some(prototype),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    };
    let Some(prototype) = prototype else {
        return log_hir_error(
            &signature.range,
            &typecheck::TYPE_REQUIREMENT,
            ("@reify".into(), "a function pointer type".into(), None),
        );
    };

    let id = context.unit.next_static();
    let mut anon = BodyLowering::new(context.unit, context.namespace.clone(), false).owned_by(id);

    let mut params = Vec::new();
    for param in declared_params(prototype) {
        let ty = lower_type(&mut anon, &param.ty)?;
        params.push(anon.declare(param.name.as_ref(), ty, false, false, &param.ty.range));
    }
    let return_type = lower_type(&mut anon, &prototype.return_type)?;

    let quote = anon.with_stage(true, |anon| lower_expr(anon, value))?;
    let quote = anon.push(HMIRExprKind::Comptime(quote), &value.range);
    let args = params
        .iter()
        .map(|param| anon.push(HMIRExprKind::Local(*param), span))
        .collect();
    let body = anon.push(HMIRExprKind::Splice { quote, args }, span);
    let root = anon.returning_block(body, span);

    let name = CXIdent::from(format!("reify.{}", id.index()).as_str());
    let signature = HMIRFnSignature::new(
        HMIRFunctionStage::Runtime,
        anon.fn_params(&params),
        return_type,
        prototype.var_args,
        HMIRContract::default(),
    );
    let prototype = HMIRFnPrototype::new(name.clone(), LinkageMode::Static, signature);
    let def = anon.finish_definition(root);
    let function = HMIRFunction::new(anon.finish(), prototype, params.into(), Some(def));

    context.unit.push_static(
        QualifiedName::new(context.namespace.clone(), name),
        span,
        HMIRDefKind::Function(Box::new(function)),
        context.body.visible_through(),
    );
    Ok(context.push(HMIRExprKind::Def(id), span))
}

// Lowers a function-level static into its own global def visible from this scope
pub(crate) fn lower_static(
    context: &mut BodyLowering<'_>,
    name: &CXIdent,
    ty: &HIRType,
    initializer: Option<&HIRExpression>,
    linkage: LinkageMode,
    span: &TokenRange,
) -> CXResult<()> {
    let id = context.unit.next_static();
    let mut global = BodyLowering::new(context.unit, context.namespace.clone(), false)
        .at(context.body.visible_through());
    global.owner = context.owner;

    let global_ty = lower_type(&mut global, ty)?;
    let initializer = match initializer {
        Some(initializer) => Some(lower_initial_value(&mut global, global_ty, initializer)?),
        None => None,
    };
    let labels = mem::take(&mut global.address_labels);
    let link_name = match linkage {
        LinkageMode::Extern => name.clone(),
        _ => CXIdent::from(format!("{name}.{}", id.index()).as_str()),
    };
    let global = HMIRGlobal::new(global.finish(), global_ty, initializer, linkage, link_name);

    let qualified = QualifiedName::new(context.namespace.clone(), name.clone());
    let kind = HMIRDefKind::Global(Box::new(global));
    let id = if linkage == LinkageMode::Extern {
        context.unit.push_extern(qualified, span, kind, context.body.visible_through())
    } else {
        context.unit.push_static(qualified, span, kind, context.body.visible_through());
        id
    };
    for label in labels {
        if !context.address_labels.contains(&label) {
            context.address_labels.push(label);
        }
    }
    context.bind_static(name, id);

    Ok(())
}
