use cx_hir::ast::{
    function::{HIRComptimeValueType, HIRFunctionPrototype},
    template::HIRTemplateInput,
    types::{HIRField, HIRMoveSemantics, HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRAggregateKind, HMIRDefRef, HMIRExprID, HMIRExprKind, HMIRFieldDef, HMIRMoveSemantics,
    HMIRTypeDesc, HMIRTypeOp,
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    resolve::GlobalSymbol,
};

impl BodyLowering<'_> {
    pub(crate) fn lower_type(&mut self, ty: &HIRType) -> HMIRExprID {
        let span = &ty.range;
        match &ty.kind {
            HIRTypeKind::Identifier {
                name,
                lookup,
                template_input,
            } => {
                let tag = match lookup {
                    HIRTypeLookup::Standard => None,
                    HIRTypeLookup::Tag(tag) => Some(*tag),
                };
                let callee = match self.lookup(name, tag) {
                    Symbol::Local(binding) => {
                        self.push(HMIRExprKind::Local(binding.local()), span)
                    }
                    Symbol::Global(GlobalSymbol::Primitive(desc)) => {
                        self.type_constant(desc, span)
                    }
                    Symbol::Global(
                        GlobalSymbol::Def(def) | GlobalSymbol::ComptimeFunction(def, _),
                    ) => self.push(HMIRExprKind::Def(def), span),
                    Symbol::Global(GlobalSymbol::Constructor(..)) => self.error(span),
                };
                self.instantiate(callee, template_input.as_ref(), span)
            }
            HIRTypeKind::ExplicitSizedArray(element, length) => {
                let element = self.lower_type(element);
                let length = self.lower_expr(length);
                self.type_op(
                    HMIRTypeOp::Array {
                        element,
                        length: Some(length),
                    },
                    span,
                )
            }
            HIRTypeKind::ImplicitSizedArray(element) => {
                let element = self.lower_type(element);
                self.type_op(
                    HMIRTypeOp::Array {
                        element,
                        length: None,
                    },
                    span,
                )
            }
            HIRTypeKind::MemoryReference { inner_type, .. } => {
                let inner = self.lower_type(inner_type);
                self.type_op(HMIRTypeOp::Reference(inner), span)
            }
            HIRTypeKind::PointerTo { inner_type } => {
                let inner = self.lower_type(inner_type);
                self.type_op(HMIRTypeOp::Pointer(inner), span)
            }
            HIRTypeKind::Structured {
                attributes, fields, ..
            } => self.aggregate(HMIRAggregateKind::Struct, &attributes.semantics, fields, span),
            HIRTypeKind::Union { fields, .. } => {
                self.aggregate(HMIRAggregateKind::Union, &HIRMoveSemantics::POD, fields, span)
            }
            HIRTypeKind::TaggedUnion {
                attributes,
                variants,
                ..
            } => self.aggregate(
                HMIRAggregateKind::TaggedUnion,
                &attributes.semantics,
                variants,
                span,
            ),
            HIRTypeKind::FunctionPointer { prototype } => {
                let function = self.function_type(prototype);
                self.type_op(HMIRTypeOp::Pointer(function), span)
            }
        }
    }

    pub(crate) fn lower_comptime_value_type(
        &mut self,
        value_type: &HIRComptimeValueType,
    ) -> HMIRExprID {
        let result = self.lower_type(&value_type.ty);
        if !value_type.expr {
            return result;
        }
        let params = value_type
            .params
            .iter()
            .map(|param| self.lower_type(param))
            .collect();
        self.type_op(HMIRTypeOp::Expr { params, result }, &value_type.ty.range)
    }

    pub(crate) fn type_of_types(&mut self, span: &TokenRange) -> HMIRExprID {
        self.type_constant(HMIRTypeDesc::Type, span)
    }

    pub(crate) fn instantiate(
        &mut self,
        callee: HMIRExprID,
        template_input: Option<&HIRTemplateInput>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let Some(input) = template_input else {
            return callee;
        };
        let args = self.template_args(Some(input));
        self.push(HMIRExprKind::Call { callee, args }, span)
    }

    pub(crate) fn template_args(&mut self, input: Option<&HIRTemplateInput>) -> Vec<HMIRExprID> {
        input
            .map(|input| input.params.iter().map(|ty| self.lower_type(ty)).collect())
            .unwrap_or_default()
    }

    pub(crate) fn constructor_sum(
        &mut self,
        union_type: &HIRType,
        template_input: Option<&HIRTemplateInput>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let HIRTypeKind::Identifier { name, lookup, .. } = &union_type.kind else {
            return self.lower_type(union_type);
        };
        let tag = match lookup {
            HIRTypeLookup::Standard => None,
            HIRTypeLookup::Tag(tag) => Some(*tag),
        };
        let def = match self.lookup(name, tag) {
            Symbol::Global(GlobalSymbol::Def(def)) => def,
            _ => HMIRDefRef::External(QualifiedName::clone(name)),
        };
        let callee = self.push(HMIRExprKind::Def(def), span);
        self.instantiate(callee, template_input, span)
    }

    fn function_type(&mut self, prototype: &HIRFunctionPrototype) -> HMIRExprID {
        let params = prototype
            .params
            .iter()
            .map(|param| self.lower_type(&param.ty))
            .collect();
        let ret = self.lower_type(&prototype.return_type);
        self.type_op(
            HMIRTypeOp::Function {
                params,
                ret,
                variadic: prototype.var_args,
            },
            &prototype.range,
        )
    }

    fn aggregate(
        &mut self,
        kind: HMIRAggregateKind,
        semantics: &HIRMoveSemantics,
        fields: &[HIRField],
        span: &TokenRange,
    ) -> HMIRExprID {
        let fields = fields
            .iter()
            .map(|field| match field {
                HIRField::Standard { name, ty } => {
                    HMIRFieldDef::new(Some(CXIdent::from(name.as_str())), self.lower_type(ty), None)
                }
                HIRField::Bitfield {
                    name,
                    integer_type,
                    width,
                } => HMIRFieldDef::new(
                    name.as_deref().map(CXIdent::from),
                    self.lower_type(integer_type),
                    Some(*width),
                ),
            })
            .collect();
        let semantics = match semantics {
            HIRMoveSemantics::POD => HMIRMoveSemantics::POD,
            HIRMoveSemantics::Nocopy => HMIRMoveSemantics::Nocopy,
            HIRMoveSemantics::Nodrop => HMIRMoveSemantics::Nodrop,
        };
        self.type_op(
            HMIRTypeOp::Aggregate {
                kind,
                semantics,
                fields,
            },
            span,
        )
    }

    fn type_op(&mut self, op: HMIRTypeOp, span: &TokenRange) -> HMIRExprID {
        self.native(cx_hmir::HMIRNativeOp::Type(op), span)
    }
}
