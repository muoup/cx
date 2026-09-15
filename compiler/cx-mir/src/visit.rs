use crate::*;
use std::convert::Infallible;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRVisitRole {
    Read,
    Copy,
    Move,
    Define,
    Write,
    Address,
    Invalidate,
}

macro_rules! traversal {
    ($visitor:ident, $core:ident, $comptime:ident, $staged:ident, $value:ident, $constant:ident, [$($mutable:tt)*]) => {
        pub trait $visitor<'ir> {
            type Error;
            fn register(&mut self, _: &'ir $($mutable)* MIRRegister, _: MIRVisitRole) -> Result<(), Self::Error> { Ok(()) }
            fn place(&mut self, place: &'ir $($mutable)* MIRPlace, _: MIRVisitRole) -> Result<(), Self::Error> {
                if let MIRPlace::Global(global) = place { self.global(global)?; }
                Ok(())
            }
            fn block(&mut self, _: &'ir $($mutable)* MIRBasicBlockID) -> Result<(), Self::Error> { Ok(()) }
            fn continuation(&mut self, _: &'ir $($mutable)* MIRBasicBlockID) -> Result<(), Self::Error> { Ok(()) }
            fn scope(&mut self, _: &'ir $($mutable)* MIRScopeID) -> Result<(), Self::Error> { Ok(()) }
            fn ty(&mut self, _: &'ir $($mutable)* MIRTypeID) -> Result<(), Self::Error> { Ok(()) }
            fn global(&mut self, _: &'ir $($mutable)* MIRGlobalID) -> Result<(), Self::Error> { Ok(()) }
            fn function(&mut self, _: &'ir $($mutable)* MIRFunctionID) -> Result<(), Self::Error> { Ok(()) }
            fn template(&mut self, _: &'ir $($mutable)* std::sync::Arc<MIRStagedTemplate>) -> Result<(), Self::Error> { Ok(()) }
            fn value(&mut self, value: &'ir $($mutable)* MIRValue) -> Result<(), Self::Error> { $value(self, value) }
            fn constant(&mut self, constant: &'ir $($mutable)* MIRConstant) -> Result<(), Self::Error> { $constant(self, constant) }
            fn target(&mut self, target: &'ir $($mutable)* MIRBlockTarget) -> Result<(), Self::Error> {
                self.block(& $($mutable)* target.block)?;
                for value in & $($mutable)* target.args { self.value(value)?; }
                Ok(())
            }
            fn targets(&mut self, targets: &'ir $($mutable)* MIRStagedTargets) -> Result<(), Self::Error> {
                for target in [& $($mutable)* targets.return_target, & $($mutable)* targets.break_target,
                    & $($mutable)* targets.continue_target, & $($mutable)* targets.yield_target] {
                    if let Some(target) = target { self.continuation(target)?; }
                }
                Ok(())
            }
        }

        pub fn $value<'ir, V: $visitor<'ir> + ?Sized>(visitor: &mut V, value: &'ir $($mutable)* MIRValue) -> Result<(), V::Error> {
            match value {
                MIRValue::Register(register) => visitor.register(register, MIRVisitRole::Read),
                MIRValue::PlaceRef(place) => visitor.place(place, MIRVisitRole::Address),
                MIRValue::Copy(place) => visitor.place(place, MIRVisitRole::Copy),
                MIRValue::Move(place) => visitor.place(place, MIRVisitRole::Move),
                MIRValue::Constant(constant) => visitor.constant(constant),
            }
        }

        pub fn $constant<'ir, V: $visitor<'ir> + ?Sized>(visitor: &mut V, constant: &'ir $($mutable)* MIRConstant) -> Result<(), V::Error> {
            match constant {
                MIRConstant::Unit | MIRConstant::Undefined | MIRConstant::Integer { .. } | MIRConstant::Float { .. } => {}
                MIRConstant::Nullptr { ty } => visitor.ty(ty)?,
                MIRConstant::Function(function) => visitor.function(function)?,
                MIRConstant::Global { global, ty, .. } => { visitor.global(global)?; visitor.ty(ty)?; }
                MIRConstant::Aggregate { ty, fields } => {
                    visitor.ty(ty)?;
                    for (_, value) in fields { visitor.constant(value)?; }
                }
            }
            Ok(())
        }

        pub fn $core<'ir, V: $visitor<'ir> + ?Sized>(visitor: &mut V, instruction: &'ir $($mutable)* MIRInstrKind) -> Result<(), V::Error> {
            use MIRVisitRole::*;
            match instruction {
                MIRInstrKind::ScopeEnter { scope } | MIRInstrKind::ScopeExit { scope } => visitor.scope(scope)?,
                MIRInstrKind::Initialize { place } => visitor.place(place, Define)?,
                MIRInstrKind::Bind { place, to } => { visitor.place(to, Address)?; visitor.place(place, Define)?; }
                MIRInstrKind::Invalidate { place, .. } => visitor.place(place, Invalidate)?,
                MIRInstrKind::Create { out, ty } => { visitor.place(out, Define)?; visitor.ty(ty)?; }
                MIRInstrKind::Assign { target, value, ty } => {
                    visitor.value(value)?;
                    match target {
                        MIRTarget::Place(place) => visitor.place(place, Write)?,
                        MIRTarget::Register(register) => visitor.register(register, Define)?,
                    }
                    visitor.ty(ty)?;
                }
                MIRInstrKind::AddressOf { out, place } => { visitor.place(place, Address)?; visitor.register(out, Define)?; }
                MIRInstrKind::Dereference { out, pointer, pointee_type } => {
                    visitor.value(pointer)?; visitor.place(out, Define)?; visitor.ty(pointee_type)?;
                }
                MIRInstrKind::AggregateOp(operation) => match operation {
                    MIRAggregateOp::Place { out, op } => {
                        match op {
                            MIRPlaceAggregateOp::Field { base, aggregate_type, .. } => { visitor.place(base, Address)?; visitor.ty(aggregate_type)?; }
                            MIRPlaceAggregateOp::Variant { base, sum_type, .. } => { visitor.place(base, Address)?; visitor.ty(sum_type)?; }
                            MIRPlaceAggregateOp::Index { base, index, element_type } => {
                                visitor.place(base, Address)?; visitor.value(index)?; visitor.ty(element_type)?;
                            }
                        }
                        visitor.place(out, Define)?;
                    }
                    MIRAggregateOp::Value { out, op } => {
                        match op {
                            MIRValueAggregateOp::Discriminant { value, sum_type } |
                            MIRValueAggregateOp::Variant { value, sum_type, .. } |
                            MIRValueAggregateOp::ProjectVariant { value, sum_type, .. } => { visitor.value(value)?; visitor.ty(sum_type)?; }
                            MIRValueAggregateOp::Construct { ty, fields } => {
                                visitor.ty(ty)?;
                                for (_, value) in fields { visitor.value(value)?; }
                            }
                        }
                        visitor.register(out, Define)?;
                    }
                }
                MIRInstrKind::Call { out, callee, args } => {
                    visitor.value(callee)?;
                    for arg in args { visitor.value(arg)?; }
                    if let Some(out) = out { visitor.register(out, Define)?; }
                }
                MIRInstrKind::VaStart { list, last } => { visitor.value(list)?; visitor.value(last)?; }
                MIRInstrKind::VaEnd { list } => visitor.value(list)?,
                MIRInstrKind::VaArg { out, list, ty } => { visitor.value(list)?; visitor.register(out, Define)?; visitor.ty(ty)?; }
                MIRInstrKind::BinOp { out, op, lhs, rhs } => {
                    visitor.value(lhs)?; visitor.value(rhs)?; visitor.register(out, Define)?;
                    if let MIRBinaryOp::PointerOffset { pointee, .. } = op { visitor.ty(pointee)?; }
                }
                MIRInstrKind::UnOp { out, operand, .. } => { visitor.value(operand)?; visitor.register(out, Define)?; }
                MIRInstrKind::Coerce { out, operand, to_type, .. } => { visitor.value(operand)?; visitor.register(out, Define)?; visitor.ty(to_type)?; }
                MIRInstrKind::Assert { condition, .. } | MIRInstrKind::Assume { condition } => visitor.value(condition)?,
                MIRInstrKind::Return { value } => { if let Some(value) = value { visitor.value(value)?; } }
                MIRInstrKind::Jump { target } => visitor.target(target)?,
                MIRInstrKind::Branch { cond, true_target, false_target } => {
                    visitor.value(cond)?; visitor.target(true_target)?; visitor.target(false_target)?;
                }
                MIRInstrKind::IntSwitch { value, cases, default } => {
                    visitor.value(value)?;
                    for (value, target) in cases { visitor.constant(value)?; visitor.target(target)?; }
                    if let Some(target) = default { visitor.target(target)?; }
                }
                MIRInstrKind::VariantSwitch { subject, sum_type, cases, default } => {
                    visitor.value(subject)?; visitor.ty(sum_type)?;
                    for (_, target) in cases { visitor.target(target)?; }
                    if let Some(target) = default { visitor.target(target)?; }
                }
                MIRInstrKind::Unreachable => {}
            }
            Ok(())
        }

        pub fn $comptime<'ir, V: $visitor<'ir> + ?Sized>(visitor: &mut V, instruction: &'ir $($mutable)* MIRComptimeOp) -> Result<(), V::Error> {
            match instruction {
                MIRComptimeOp::Call { out, callee, args } => {
                    visitor.value(callee)?;
                    for value in args { visitor.value(value)?; }
                    if let Some(out) = out { visitor.register(out, MIRVisitRole::Define)?; }
                }
                MIRComptimeOp::MakeStaged { out, template, captures } => {
                    visitor.template(template)?;
                    for value in captures { visitor.value(value)?; }
                    visitor.register(out, MIRVisitRole::Define)?;
                }
                MIRComptimeOp::ApplyStaged { out, staged, args, targets } => {
                    visitor.value(staged)?;
                    for value in args { visitor.value(value)?; }
                    visitor.targets(targets)?;
                    if let Some(out) = out { visitor.register(out, MIRVisitRole::Define)?; }
                }
            }
            Ok(())
        }

        pub fn $staged<'ir, V: $visitor<'ir> + ?Sized>(visitor: &mut V, instruction: &'ir $($mutable)* MIRStagedInstrKind) -> Result<(), V::Error> {
            match instruction {
                MIRStagedInstrKind::Standard(kind) => $core(visitor, kind)?,
                MIRStagedInstrKind::Comptime(kind) => $comptime(visitor, kind)?,
                MIRStagedInstrKind::Complete { value } => visitor.value(value)?,
                MIRStagedInstrKind::CallerReturn { value } => { if let Some(value) = value { visitor.value(value)?; } }
                MIRStagedInstrKind::Move { out, value } => { visitor.value(value)?; visitor.register(out, MIRVisitRole::Define)?; }
                MIRStagedInstrKind::ScopeExit { .. } => {}
                MIRStagedInstrKind::Yield { value, ty } => {
                    if let Some(value) = value { visitor.value(value)?; }
                    if let Some(ty) = ty { visitor.ty(ty)?; }
                }
                MIRStagedInstrKind::Use { value, targets } => { visitor.value(value)?; visitor.targets(targets)?; }
            }
            Ok(())
        }
    };
}

traversal!(
    MIRVisitor,
    walk_instruction,
    walk_comptime,
    walk_staged,
    walk_value,
    walk_constant,
    []
);
traversal!(MIRVisitorMut, walk_instruction_mut, walk_comptime_mut, walk_staged_mut, walk_value_mut, walk_constant_mut, [mut]);

pub trait MIRWalk {
    fn visit<'ir, V: MIRVisitor<'ir>>(&'ir self, visitor: &mut V) -> Result<(), V::Error>;
    fn visit_mut<'ir, V: MIRVisitorMut<'ir>>(
        &'ir mut self,
        visitor: &mut V,
    ) -> Result<(), V::Error>;
}

macro_rules! walk_impl {
    ($ty:ty, $walk:ident, $walk_mut:ident) => {
        impl MIRWalk for $ty {
            fn visit<'ir, V: MIRVisitor<'ir>>(&'ir self, visitor: &mut V) -> Result<(), V::Error> {
                $walk(visitor, self)
            }
            fn visit_mut<'ir, V: MIRVisitorMut<'ir>>(
                &'ir mut self,
                visitor: &mut V,
            ) -> Result<(), V::Error> {
                $walk_mut(visitor, self)
            }
        }
    };
}
walk_impl!(MIRInstrKind, walk_instruction, walk_instruction_mut);
walk_impl!(MIRComptimeOp, walk_comptime, walk_comptime_mut);
walk_impl!(MIRStagedInstrKind, walk_staged, walk_staged_mut);

impl MIRWalk for MIRComptimeInstrKind {
    fn visit<'ir, V: MIRVisitor<'ir>>(&'ir self, visitor: &mut V) -> Result<(), V::Error> {
        match self {
            Self::Standard(kind) => kind.visit(visitor),
            Self::Comptime(kind) => kind.visit(visitor),
        }
    }
    fn visit_mut<'ir, V: MIRVisitorMut<'ir>>(
        &'ir mut self,
        visitor: &mut V,
    ) -> Result<(), V::Error> {
        match self {
            Self::Standard(kind) => kind.visit_mut(visitor),
            Self::Comptime(kind) => kind.visit_mut(visitor),
        }
    }
}

impl<K: MIRWalk> MIRInstr<K> {
    pub fn visit<'ir, V: MIRVisitor<'ir>>(&'ir self, visitor: &mut V) -> Result<(), V::Error> {
        self.kind.visit(visitor)
    }
    pub fn visit_mut<'ir, V: MIRVisitorMut<'ir>>(
        &'ir mut self,
        visitor: &mut V,
    ) -> Result<(), V::Error> {
        self.kind.visit_mut(visitor)
    }
    pub fn successors(&self) -> impl Iterator<Item = MIRBasicBlockID> {
        struct Edges(Vec<MIRBasicBlockID>);
        impl<'ir> MIRVisitor<'ir> for Edges {
            type Error = Infallible;
            fn block(&mut self, block: &MIRBasicBlockID) -> Result<(), Infallible> {
                self.0.push(*block);
                Ok(())
            }
        }
        let mut edges = Edges(Vec::new());
        let Ok(()) = self.visit(&mut edges);
        edges.0.into_iter()
    }
}
