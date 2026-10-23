pub(crate) mod composite;
mod interface;
pub(crate) mod recursive;
mod structure;

use std::rc::Rc;

use cx_hmir::{HMIRDef, HMIRDefID, HMIRFnSignature, ty::HMIRTypeID};
use cx_log::{CXResult, catalogue::mir};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{env::HMIREnvironment, log::hmir_error, value::HMIRValue};

pub(crate) use structure::Shape;

#[derive(Debug, Clone)]
pub(crate) enum CompletedSymbol {
    Function(Rc<CompletedFunction>),
    StagedFunction,
    Global(Rc<CompletedGlobal>),
    Constant(HMIRValue),
    Type(HMIRTypeID),
}

#[derive(Debug)]
pub(crate) struct CompletedFunction {
    pub(crate) declaration: usize,
    pub(crate) symbol_name: CXIdent,
    pub(crate) linkage: LinkageMode,
    pub(crate) signature: HMIRFnSignature<HMIRTypeID>,
    pub(crate) ty: HMIRTypeID,
}

#[derive(Debug)]
pub(crate) struct CompletedGlobal {
    pub(crate) declaration: usize,
    pub(crate) symbol_name: CXIdent,
    pub(crate) linkage: LinkageMode,
    pub(crate) ty: HMIRTypeID,
}

#[derive(Debug)]
pub(crate) enum SymbolState {
    InProgress(Option<recursive::ProvisionalType>),
    Complete(CompletedSymbol),
}

pub(crate) fn complete(env: &mut HMIREnvironment, id: HMIRDefID) -> CXResult<CompletedSymbol> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let def = unit.resolve_def(id).expect("symbol exists in its unit");
    match env.resolver().symbol_state(id) {
        Some(SymbolState::Complete(symbol)) => return Ok(symbol.clone()),
        Some(SymbolState::InProgress(Some(provisional))) => {
            return if provisional.available(env, id) {
                Ok(CompletedSymbol::Type(provisional.ty))
            } else {
                Err(cycle(def))
            };
        }
        Some(SymbolState::InProgress(None)) => return Err(cycle(def)),
        None => {}
    }
    let shape = structure::check(def)?;
    env.resolver_mut()
        .set_symbol_state(id, SymbolState::InProgress(None));
    let type_like = matches!(shape, Shape::Alias | Shape::Type(_));
    if type_like {
        env.resolver_mut()
            .defining_types
            .push(recursive::TypeFrame {
                def: id,
                constructors: Vec::new(),
            });
    }
    env.resolver_mut().active_symbols.push(id);
    let result = interface::complete(env, id, def, &shape);
    env.resolver_mut().active_symbols.pop();
    if type_like {
        env.resolver_mut().defining_types.pop();
    }
    match result {
        Ok(symbol) => {
            env.resolver_mut()
                .set_symbol_state(id, SymbolState::Complete(symbol.clone()));
            Ok(symbol)
        }
        Err(error) => {
            env.resolver_mut().forget_symbol(id);
            Err(error)
        }
    }
}

pub(crate) fn validate(env: &mut HMIREnvironment, id: HMIRDefID) -> CXResult<Shape> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let def = unit.resolve_def(id).expect("symbol exists in its unit");
    let shape = structure::check(def)?;
    let declared = def
        .declarations()
        .iter()
        .filter(|declaration| declaration.kind().is_some());
    if shape.reconciles() && declared.count() > 1 {
        complete(env, id)?;
    }
    Ok(shape)
}

pub(crate) fn completed<'e>(env: &'e HMIREnvironment, id: HMIRDefID) -> &'e CompletedSymbol {
    match env.resolver().symbol_state(id) {
        Some(SymbolState::Complete(symbol)) => symbol,
        _ => unreachable!("a value refers to a symbol that was never completed"),
    }
}

fn cycle(def: &HMIRDef) -> cx_log::error::CXError {
    let span = def.declarations()[0].span();
    hmir_error(span, &mir::DEPENDENCY_CYCLE, def.name().to_string())
}
