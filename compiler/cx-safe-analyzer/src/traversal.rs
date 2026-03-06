use cx_safe_ir::ast::{FMIRNode, FMIRNodeBody};

#[allow(dead_code)]
pub(crate) enum VisitControl {
    Continue,
    SkipChildren,
    Stop,
}

pub(crate) fn child_nodes(node: &FMIRNode) -> Vec<&FMIRNode> {
    match &node.body {
        FMIRNodeBody::Application { function, argument } => vec![function, argument],
        FMIRNodeBody::CompilerAssert { condition, .. } => vec![condition],
        FMIRNodeBody::Bind {
            monad, function, ..
        } => vec![monad, function],
        FMIRNodeBody::Then { first, second } => vec![first, second],
        FMIRNodeBody::Load { pointer } => vec![pointer],
        FMIRNodeBody::Store { pointer, value } => vec![pointer, value],
        FMIRNodeBody::If {
            condition,
            then_branch,
            else_branch,
        } => vec![condition, then_branch, else_branch],
        FMIRNodeBody::CLoop { condition, body } => vec![condition, body],
        FMIRNodeBody::CReturn { value } => vec![value],
        FMIRNodeBody::IntrinsicFunction(_)
        | FMIRNodeBody::DeclareAccess { .. }
        | FMIRNodeBody::Pure
        | FMIRNodeBody::UnsafeBlock
        | FMIRNodeBody::Alloca
        | FMIRNodeBody::VariableAlias { .. }
        | FMIRNodeBody::IntegerLiteral(_)
        | FMIRNodeBody::FloatLiteral(_)
        | FMIRNodeBody::BooleanLiteral(_)
        | FMIRNodeBody::Unit => vec![],
    }
}

pub(crate) fn walk_pre_order<E, F>(root: &FMIRNode, visit: &mut F) -> Result<(), E>
where
    F: FnMut(&FMIRNode) -> Result<VisitControl, E>,
{
    let mut stack = vec![root];

    while let Some(node) = stack.pop() {
        match visit(node)? {
            VisitControl::Stop => return Ok(()),
            VisitControl::SkipChildren => continue,
            VisitControl::Continue => {
                let mut children = child_nodes(node);
                children.reverse();
                stack.extend(children);
            }
        }
    }

    Ok(())
}
