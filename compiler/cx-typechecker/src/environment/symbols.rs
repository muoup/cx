pub use cx_mir::mir::expression::SymbolValueOrigin;
pub use cx_mir::registry::{
    MIRSymbolRegistry as SymbolRegistry, ResolvedValueSymbol, TemplateBindingFrame,
};

#[path = "../../../cx-mir/src/symbol/completion.rs"]
pub(crate) mod completion;
#[path = "../../../cx-mir/src/symbol/templates.rs"]
pub(crate) mod templates;
