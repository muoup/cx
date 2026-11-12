use crate::index::FileIndex;
use cx_hmir::unit::def::HMIRDeclarationRole;
use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position};

#[derive(Clone, Copy)]
pub enum Goto {
    Definition,
    Declaration,
}

impl Goto {
    fn accepts(self, role: HMIRDeclarationRole) -> bool {
        match self {
            Goto::Definition => role == HMIRDeclarationRole::Definition,
            Goto::Declaration => role != HMIRDeclarationRole::Definition,
        }
    }
}

pub fn locate(index: &FileIndex, position: Position, goto: Goto) -> Option<GotoDefinitionResponse> {
    let reference = index.reference_at(position)?;
    let preferred = reference
        .targets
        .iter()
        .filter(|target| goto.accepts(target.role))
        .collect::<Vec<_>>();
    let targets = match preferred.is_empty() {
        true => reference.targets.iter().collect(),
        false => preferred,
    };

    let mut locations = Vec::<Location>::new();
    for target in targets {
        if !locations.contains(&target.location) {
            locations.push(target.location.clone());
        }
    }

    match locations.len() {
        0 => None,
        1 => locations.pop().map(GotoDefinitionResponse::Scalar),
        _ => Some(GotoDefinitionResponse::Array(locations)),
    }
}
