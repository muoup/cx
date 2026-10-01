use std::collections::HashMap;

use cx_hir::ast::{
    expression::HIRExpression,
    types::{HIRTagKind, HIRType},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBlockKind, HMIRBody, HMIRCoerceMode, HMIRConstant, HMIRControlOp, HMIRDef,
    HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRExpr, HMIRExprID, HMIRExprKind, HMIRGlobal, HMIRHole,
    HMIRLocal, HMIRLocalID, HMIRNativeOp, HMIROwnershipOp, HMIRTypeDesc, HMIRTypeID,
    HMIRTypeInterner, HMIRTypeOp,
};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    expr::lower_initial_value,
    resolve::{GlobalSymbol, Resolver},
    ty::lower_type,
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
    Global(GlobalSymbol),
}

pub(crate) struct BodyLowering<'a> {
    resolver: &'a Resolver<'a>,
    types: &'a mut HMIRTypeInterner,
    namespace: NamespacePath,
    body: HMIRBody,
    scopes: Vec<HashMap<CXIdent, ScopeEntry>>,
    comptime: bool,
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
    pub(crate) fn new(
        resolver: &'a Resolver<'a>,
        types: &'a mut HMIRTypeInterner,
        namespace: NamespacePath,
        comptime: bool,
    ) -> Self {
        Self {
            resolver,
            types,
            namespace,
            body: HMIRBody::new(),
            scopes: vec![HashMap::new()],
            comptime,
        }
    }

    pub(crate) fn resolver(&self) -> &'a Resolver<'a> {
        self.resolver
    }

    pub(crate) fn finish(self) -> HMIRBody {
        self.body
    }

    pub(crate) fn push(&mut self, kind: HMIRExprKind, span: &TokenRange) -> HMIRExprID {
        self.body.push_expr(HMIRExpr::new(kind, span.clone()))
    }

    pub(crate) fn native(&mut self, op: HMIRNativeOp, span: &TokenRange) -> HMIRExprID {
        self.push(HMIRExprKind::Native(op), span)
    }

    pub(crate) fn error(&mut self, span: &TokenRange) -> HMIRExprID {
        self.push(HMIRExprKind::Error, span)
    }

    pub(crate) fn hole(&mut self, span: &TokenRange) -> HMIRExprID {
        let hole = self.body.declare_hole(HMIRHole::new(span.clone()));
        self.push(HMIRExprKind::Hole(hole), span)
    }

    pub(crate) fn block(
        &mut self,
        kind: HMIRBlockKind,
        statements: Vec<HMIRExprID>,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.push(
            HMIRExprKind::Block {
                kind,
                statements,
                tail: None,
            },
            span,
        )
    }

    pub(crate) fn def_expr(&mut self, name: QualifiedName, span: &TokenRange) -> HMIRExprID {
        let def = self.resolver.def_ref(name);
        self.push(HMIRExprKind::Def(def), span)
    }

    pub(crate) fn intern(&mut self, desc: HMIRTypeDesc) -> HMIRTypeID {
        self.types.intern(desc)
    }

    pub(crate) fn type_constant(&mut self, desc: HMIRTypeDesc, span: &TokenRange) -> HMIRExprID {
        let ty = self.intern(desc);
        self.push(HMIRExprKind::Constant(HMIRConstant::Type(ty)), span)
    }

    pub(crate) fn is_comptime(&self) -> bool {
        self.comptime
    }

    pub(crate) fn with_stage<T>(&mut self, comptime: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let previous = std::mem::replace(&mut self.comptime, comptime);
        let result = f(self);
        self.comptime = previous;
        result
    }

    pub(crate) fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(HashMap::new());
        let result = f(self);
        self.scopes.pop();
        result
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
                .declare_local(HMIRLocal::new(name.cloned(), ty, comptime, span.clone()));
        if let Some(name) = name {
            self.scopes
                .last_mut()
                .expect("body lowering always has a root scope")
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
            && let Some(entry) = self.scopes.iter().rev().find_map(|scope| scope.get(root))
        {
            return match *entry {
                ScopeEntry::Local(binding) => Symbol::Local(binding),
                ScopeEntry::Static(def) => {
                    Symbol::Global(GlobalSymbol::Def(HMIRDefRef::Local(def)))
                }
            };
        }
        Symbol::Global(self.resolver.resolve(&self.namespace, name, tag))
    }
    pub(crate) fn returning_block(&mut self, value: HMIRExprID, span: &TokenRange) -> HMIRExprID {
        let ret = self.control(HMIRControlOp::Return(Some(value)), span);
        self.block(HMIRBlockKind::Scope, vec![ret], span)
    }
    pub(crate) fn int_constant(
        &mut self,
        desc: HMIRTypeDesc,
        value: i128,
        span: &TokenRange,
    ) -> HMIRExprID {
        let ty = self.intern(desc);
        self.push(
            HMIRExprKind::Constant(HMIRConstant::Int { value, ty }),
            span,
        )
    }
    pub(crate) fn control(&mut self, op: HMIRControlOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::Control(op), span)
    }
    pub(crate) fn ownership(&mut self, op: HMIROwnershipOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::OwnershipOp(op), span)
    }
    pub(crate) fn aggregate_op(&mut self, op: HMIRAggregateOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::AggregateOp(op), span)
    }
    pub(crate) fn type_of_types(&mut self, span: &TokenRange) -> HMIRExprID {
        self.type_constant(HMIRTypeDesc::Type, span)
    }
    pub(crate) fn type_op(&mut self, op: HMIRTypeOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::Type(op), span)
    }
    pub(crate) fn coerce(
        &mut self,
        mode: HMIRCoerceMode,
        value: HMIRExprID,
        target: HMIRExprID,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.native(
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            },
            span,
        )
    }
}

// Lowers a function-level static into its own global def visible from this scope
pub(crate) fn lower_static(
    cx: &mut BodyLowering<'_>,
    name: &CXIdent,
    ty: &HIRType,
    initializer: Option<&HIRExpression>,
    span: &TokenRange,
) {
    let id = cx.resolver.next_static();
    let mut global_cx = BodyLowering::new(cx.resolver, cx.types, cx.namespace.clone(), false);
    let global_ty = lower_type(&mut global_cx, ty);
    let initializer =
        initializer.map(|initializer| lower_initial_value(&mut global_cx, ty, initializer));
    let global = HMIRGlobal::new(
        global_cx.finish(),
        global_ty,
        initializer,
        true,
        LinkageMode::Static,
        CXIdent::from(format!("{name}.{}", id.index()).as_str()),
    );
    let qualified = QualifiedName::new(cx.namespace.clone(), name.clone());
    cx.resolver.push_static(HMIRDef::new(
        qualified,
        span.clone(),
        HMIRDefKind::Global(Box::new(global)),
    ));
    cx.scopes
        .last_mut()
        .expect("body lowering always has a root scope")
        .insert(name.clone(), ScopeEntry::Static(id));
}
