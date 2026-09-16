use std::convert::Infallible;

use crate::visit::{MIRVisitRole, MIRVisitor, MIRWalk};
use crate::{MIRBlockTarget, MIRPlaceID, MIRRegister, MIRTarget, MIRValue};

pub(crate) fn operands(instruction: &impl MIRWalk) -> Vec<&MIRValue> {
    struct Operands<'ir>(Vec<&'ir MIRValue>);
    impl<'ir> MIRVisitor<'ir> for Operands<'ir> {
        type Error = Infallible;

        fn value(&mut self, value: &'ir MIRValue) -> Result<(), Infallible> {
            self.0.push(value);
            Ok(())
        }

        fn target(&mut self, _: &'ir MIRBlockTarget) -> Result<(), Infallible> {
            Ok(())
        }
    }
    let mut visitor = Operands(Vec::new());
    let Ok(()) = instruction.visit(&mut visitor);
    visitor.0
}

pub(crate) fn successors(instruction: &impl MIRWalk) -> Vec<&MIRBlockTarget> {
    struct Successors<'ir>(Vec<&'ir MIRBlockTarget>);
    impl<'ir> MIRVisitor<'ir> for Successors<'ir> {
        type Error = Infallible;

        fn target(&mut self, target: &'ir MIRBlockTarget) -> Result<(), Infallible> {
            self.0.push(target);
            Ok(())
        }
    }
    let mut visitor = Successors(Vec::new());
    let Ok(()) = instruction.visit(&mut visitor);
    visitor.0
}

pub(crate) fn places(instruction: &impl MIRWalk) -> Vec<(MIRPlaceID, MIRVisitRole)> {
    struct Places(Vec<(MIRPlaceID, MIRVisitRole)>);
    impl<'ir> MIRVisitor<'ir> for Places {
        type Error = Infallible;

        fn place(&mut self, place: &'ir MIRPlaceID, role: MIRVisitRole) -> Result<(), Infallible> {
            self.0.push((*place, role));
            Ok(())
        }

        fn target(&mut self, _: &'ir MIRBlockTarget) -> Result<(), Infallible> {
            Ok(())
        }
    }
    let mut visitor = Places(Vec::new());
    let Ok(()) = instruction.visit(&mut visitor);
    visitor.0
}

pub(crate) fn registers(instruction: &impl MIRWalk) -> Vec<(MIRRegister, MIRVisitRole)> {
    struct Registers(Vec<(MIRRegister, MIRVisitRole)>);
    impl<'ir> MIRVisitor<'ir> for Registers {
        type Error = Infallible;

        fn register(
            &mut self,
            register: &'ir MIRRegister,
            role: MIRVisitRole,
        ) -> Result<(), Infallible> {
            self.0.push((*register, role));
            Ok(())
        }

        fn target(&mut self, _: &'ir MIRBlockTarget) -> Result<(), Infallible> {
            Ok(())
        }
    }
    let mut visitor = Registers(Vec::new());
    let Ok(()) = instruction.visit(&mut visitor);
    visitor.0
}

pub(crate) fn targets(instruction: &impl MIRWalk) -> Vec<(MIRTarget, MIRVisitRole)> {
    struct Targets(Vec<(MIRTarget, MIRVisitRole)>);
    impl<'ir> MIRVisitor<'ir> for Targets {
        type Error = Infallible;

        fn storage(
            &mut self,
            target: &'ir MIRTarget,
            role: MIRVisitRole,
        ) -> Result<(), Infallible> {
            self.0.push((*target, role));
            Ok(())
        }

        fn target(&mut self, _: &'ir MIRBlockTarget) -> Result<(), Infallible> {
            Ok(())
        }
    }
    let mut visitor = Targets(Vec::new());
    let Ok(()) = instruction.visit(&mut visitor);
    visitor.0
}
