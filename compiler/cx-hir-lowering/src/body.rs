use std::collections::HashMap;

use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    types::{HIRTagKind, HIRType, HIRTypeKind},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBlockKind, HMIRBody, HMIRCoerceMode, HMIRConstant, HMIRContract,
    HMIRControlOp, HMIRDef, HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRError, HMIRExpr, HMIRExprID,
    HMIRExprKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRHole, HMIRLocal, HMIRLocalID, HMIRNativeOp,
    HMIROwnershipOp, HMIRSignature, HMIRTypeDesc, HMIRTypeID, HMIRTypeInterner, HMIRTypeOp,
};
use cx_log::catalogue::{ErrorDefinition, typecheck};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    def::is_void,
    expr::{lower_expr, lower_initial_value},
    resolve::{GlobalSymbol, Resolver},
    ty::lower_type,
};

pub(crate) fn hmir_error<A>(definition: &ErrorDefinition<A>, args: A) -> HMIRError {
    HMIRError {
        code: definition.code,
        message: (definition.message)(args),
    }
}

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

    pub(crate) fn error<A>(
        &mut self,
        span: &TokenRange,
        definition: &ErrorDefinition<A>,
        args: A,
    ) -> HMIRExprID {
        self.push(HMIRExprKind::Error(hmir_error(definition, args)), span)
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

// A variant of a computed sum named as a value, 'opt(int)::some': a function over the sum type,
// bound to the sum it is named on
pub(crate) fn lower_variant_constructor(
    cx: &mut BodyLowering<'_>,
    sum: HMIRExprID,
    variant: &CXIdent,
    span: &TokenRange,
) -> HMIRExprID {
    let id = cx.resolver.next_static();
    let mut ctor = BodyLowering::new(cx.resolver, cx.types, cx.namespace.clone(), false);
    let universe = ctor.type_of_types(span);
    let sum_param = ctor.declare(None, universe, true, false, span);
    let sum_ty = ctor.push(HMIRExprKind::Local(sum_param), span);
    let payload = ctor.type_op(
        HMIRTypeOp::Member {
            ty: sum_ty,
            name: variant.clone(),
        },
        span,
    );
    let value = ctor.declare(None, payload, false, false, span);
    let return_type = ctor.push(HMIRExprKind::Local(sum_param), span);

    let ty = ctor.push(HMIRExprKind::Local(sum_param), span);
    let moved = ctor.push(HMIRExprKind::Local(value), span);
    let moved = ctor.ownership(HMIROwnershipOp::Move(moved), span);
    let built = ctor.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty,
            fields: vec![(Some(variant.clone()), moved)],
        },
        span,
    );
    let root = ctor.returning_block(built, span);
    let name = CXIdent::from(format!("{variant}.{}", id.index()).as_str());
    let signature = HMIRSignature::new(
        vec![sum_param, value],
        return_type,
        false,
        LinkageMode::Static,
        name.clone(),
        HMIRContract::default(),
    );
    let function = HMIRFunction::new(
        HMIRFunctionStage::Runtime,
        ctor.finish(),
        signature,
        Some(root),
    );
    cx.resolver.push_static(HMIRDef::new(
        QualifiedName::new(cx.namespace.clone(), name),
        span.clone(),
        HMIRDefKind::Function(Box::new(function)),
    ));
    let callee = cx.push(HMIRExprKind::Def(HMIRDefRef::Local(id)), span);
    cx.push(
        HMIRExprKind::Call {
            callee,
            args: vec![sum],
        },
        span,
    )
}

// '@reify(signature, value)' is a function of its own: its parameters are handed to the staged
// value, whose code becomes the body. It is lowered outside the enclosing function, so the code
// can only name what a function at file scope could.
pub(crate) fn lower_reify(
    cx: &mut BodyLowering<'_>,
    signature: &HIRExpression,
    value: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
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
        return cx.error(
            &signature.range,
            &typecheck::TYPE_REQUIREMENT,
            ("@reify".into(), "a function pointer type".into(), None),
        );
    };

    let id = cx.resolver.next_static();
    let mut anon = BodyLowering::new(cx.resolver, cx.types, cx.namespace.clone(), false);
    let declared = match prototype.params.as_slice() {
        [param] if param.name.is_none() && is_void(&anon, &param.ty) => &[],
        params => params,
    };
    let params = declared
        .iter()
        .map(|param| {
            let ty = lower_type(&mut anon, &param.ty);
            anon.declare(param.name.as_ref(), ty, false, false, &param.ty.range)
        })
        .collect::<Vec<_>>();
    let return_type = lower_type(&mut anon, &prototype.return_type);

    let quote = anon.with_stage(true, |anon| lower_expr(anon, value));
    let quote = anon.push(HMIRExprKind::Comptime(quote), &value.range);
    let args = params
        .iter()
        .map(|param| anon.push(HMIRExprKind::Local(*param), span))
        .collect();
    let body = anon.push(HMIRExprKind::Splice { quote, args }, span);
    let root = anon.returning_block(body, span);

    let name = CXIdent::from(format!("reify.{}", id.index()).as_str());
    let signature = HMIRSignature::new(
        params,
        return_type,
        prototype.var_args,
        LinkageMode::Static,
        name.clone(),
        HMIRContract::default(),
    );
    let function = HMIRFunction::new(
        HMIRFunctionStage::Runtime,
        anon.finish(),
        signature,
        Some(root),
    );
    cx.resolver.push_static(HMIRDef::new(
        QualifiedName::new(cx.namespace.clone(), name),
        span.clone(),
        HMIRDefKind::Function(Box::new(function)),
    ));
    cx.push(HMIRExprKind::Def(HMIRDefRef::Local(id)), span)
}

// Lowers a function-level static into its own global def visible from this scope
pub(crate) fn lower_static(
    cx: &mut BodyLowering<'_>,
    name: &CXIdent,
    ty: &HIRType,
    initializer: Option<&HIRExpression>,
    linkage: LinkageMode,
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
        linkage,
        match linkage {
            LinkageMode::Extern => name.clone(),
            _ => CXIdent::from(format!("{name}.{}", id.index()).as_str()),
        },
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
