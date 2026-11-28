use chl_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext},
};
use rustc_hash::FxHashSet;

use crate::{
    env::HMIREnvironment,
    value::{HMIRValue, format::format_value},
};

pub fn name(env: &HMIREnvironment, ty: HMIRTypeID) -> String {
    Names::new(env, false).name(ty)
}

pub fn generated_name(
    env: &HMIREnvironment,
    def: HMIRDefID,
    args: &[HMIRValue],
    defaults: &[usize],
) -> String {
    Names::new(env, false).generated_name(def, args, defaults)
}

pub fn conversion_names(
    env: &HMIREnvironment,
    source: HMIRTypeID,
    target: HMIRTypeID,
) -> (String, String) {
    let mut compact = Names::new(env, false);
    let names = (compact.name(source), compact.name(target));
    if source == target || names.0 != names.1 {
        return names;
    }
    let mut expanded = Names::new(env, true);
    let names = (expanded.name(source), expanded.name(target));
    if names.0 == names.1 {
        (
            format!("{} [type {source}]", names.0),
            format!("{} [type {target}]", names.1),
        )
    } else {
        names
    }
}

struct Names<'a, 'global, 'hmir> {
    env: &'a HMIREnvironment<'global, 'hmir>,
    expanded: bool,
    active: FxHashSet<HMIRTypeID>,
}

impl<'a, 'global, 'hmir> Names<'a, 'global, 'hmir> {
    fn new(env: &'a HMIREnvironment<'global, 'hmir>, expanded: bool) -> Self {
        Self {
            env,
            expanded,
            active: FxHashSet::default(),
        }
    }

    fn generated_name(&mut self, def: HMIRDefID, args: &[HMIRValue], defaults: &[usize]) -> String {
        let env = self.env;
        let name = env.symbols().resolve_definition(def).name();
        let name = if self.expanded {
            name.to_string()
        } else {
            name.name.to_string()
        };
        let args = args.iter().enumerate().map(|(index, arg)| {
            if defaults.contains(&index) {
                "..".into()
            } else {
                format_value(env, arg, &mut |ty| self.name(ty))
            }
        });
        format!("{name}({})", args.collect::<Vec<_>>().join(", "))
    }

    fn name(&mut self, id: HMIRTypeID) -> String {
        let env = self.env;
        let constant = env.semantics(id).constant;
        let id = env.underlying(id);
        let ty = env.resolve_type_id(id);
        let base = match ty.kind() {
            _ if self.expanded
                && let Some((def, args)) = env.resolver().origin(id)
                && self.active.insert(id) =>
            {
                let name = self.generated_name(def, args, &[]);
                self.active.remove(&id);
                name
            }
            _ if let Some(name) = ty.debug_name() => name.to_owned(),
            HMIRTypeKind::Nominal {
                inner,
                strong_identifier: Some(id),
            } => {
                format!("@strong#{} {}", id.0, self.name(*inner))
            }
            HMIRTypeKind::Nominal { .. } | HMIRTypeKind::Strong { .. } => {
                unreachable!("not an underlying type")
            }
            HMIRTypeKind::Void => "void".into(),
            HMIRTypeKind::Unreachable => "unreachable".into(),
            HMIRTypeKind::Type => "type".into(),
            HMIRTypeKind::Str(Some(length)) => format!("str<{length}>"),
            HMIRTypeKind::Str(None) => "str".into(),
            HMIRTypeKind::Int(int) => format!(
                "{}{}",
                if int.signed() { "i" } else { "u" },
                int.width().bytes() * 8
            ),
            HMIRTypeKind::Float(float) => format!("f{}", float.width().bytes() * 8),
            HMIRTypeKind::PointerTo(inner) => format!("{}*", self.name(*inner)),
            HMIRTypeKind::ReferenceTo {
                inner,
                bitfield: Some(bitfield),
            } => {
                format!(
                    "{}&[{}:{}]",
                    self.name(*inner),
                    bitfield.offset,
                    bitfield.width
                )
            }
            HMIRTypeKind::ReferenceTo {
                inner,
                bitfield: None,
            } => format!("{}&", self.name(*inner)),
            HMIRTypeKind::Array { element, length } => format!("{}[{length}]", self.name(*element)),
            HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
                format!("{}[]", self.name(*element))
            }
            HMIRTypeKind::Undefined(HMIRUndefined::Forward(Some(tag))) => tag.prefix().into(),
            HMIRTypeKind::Undefined(HMIRUndefined::Forward(None)) => "undefined".into(),

            HMIRTypeKind::Staged(ty) => format!("^{}", self.name(*ty)),

            HMIRTypeKind::Builtin { name, .. } => name.clone(),
            HMIRTypeKind::Struct { .. } => "struct".into(),
            HMIRTypeKind::Union { .. } => "union".into(),
            HMIRTypeKind::TaggedUnion { .. } => "enum union".into(),
            HMIRTypeKind::Function { signature, .. } => {
                let mut s = String::new();

                if signature.params().is_empty() {
                    s.push_str("() -> ");
                }

                for a in signature.params().iter() {
                    s.push_str(format!("{} -> ", self.name(*a.ty())).as_str());
                }

                s.push_str(self.name(*signature.return_type()).as_str());
                s
            }
            HMIRTypeKind::Dependent => "_".into(),
        };
        match constant {
            true => format!("const {base}"),
            false => base,
        }
    }
}
