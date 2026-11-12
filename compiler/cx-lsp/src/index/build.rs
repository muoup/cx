use crate::index::{
    Declaration, DeclarationSite, FileIndex, IndexedSymbol, Reference, SymbolClass, TypedSpan,
};
use crate::position::SourceCache;
use cx_hmir::body::{HMIRBodyContext, HMIRLocalID};
use cx_hmir::expr::HMIRExprKind;
use cx_hmir::ty::HMIRGenericTypeKind;
use cx_hmir::unit::def::{HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind};
use cx_hmir::unit::global::HMIRComptimeGlobal;
use cx_hmir::unit::HMIRUnit;
use cx_hmir_lowering::env::symbols::HMIRSymbolEnv;
use cx_pipeline_data::db::ModuleData;
use cx_pipeline_data::hmir::facts::HMIRTypeFacts;
use cx_pipeline_data::hmir::HMIRUnitRegistry;
use cx_pipeline_data::GlobalCompilationContext;
use cx_tokens::token::TokenKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use rustc_hash::FxHashMap;
use std::path::PathBuf;
use tower_lsp::lsp_types::{Location, Range, Url};

struct Identifier {
    start: usize,
    end: usize,
    name: String,
}

// Declarations read from their start through the name they declare, so the name is the last mention in them; the
// spans of statement locals also cover their initializer, so there it is the first
#[derive(Clone, Copy)]
enum Occurrence {
    First,
    Last,
}

struct Site {
    file: PathBuf,
    range: Range,
    name_end: usize,
}

struct Indexer<'a> {
    identifiers: FxHashMap<PathBuf, Vec<Identifier>>,
    sources: &'a mut SourceCache,
    facts: HMIRTypeFacts,
    sites: FxHashMap<HMIRDefID, Vec<DeclarationSite>>,
    files: FxHashMap<PathBuf, FileIndex>,
}

pub fn from_context(
    context: &GlobalCompilationContext,
    sources: &mut SourceCache,
) -> FxHashMap<Url, FileIndex> {
    let db = &context.module_db;
    let namespaces = db.lex_tokens.lock().keys().cloned().collect::<Vec<_>>();
    let mut facts = HMIRTypeFacts::default();
    for namespace in &namespaces {
        if let Some(unit) = db.type_facts.remove(namespace) {
            facts.exprs.extend(unit.exprs);
            facts.locals.extend(unit.locals);
        }
    }

    let mut indexer = Indexer {
        identifiers: identifiers(db),
        sources,
        facts,
        sites: FxHashMap::default(),
        files: FxHashMap::default(),
    };

    for namespace in &namespaces {
        if let Some(unit) = db.hmir.get(namespace) {
            indexer.unit(&db.hmir, &unit);
        }
    }

    indexer.finish()
}

fn identifiers(db: &ModuleData) -> FxHashMap<PathBuf, Vec<Identifier>> {
    let mut identifiers = FxHashMap::<PathBuf, Vec<Identifier>>::default();
    for tokens in db.lex_tokens.lock().values() {
        for token in tokens.iter() {
            if let TokenKind::Identifier(name) = &token.kind {
                identifiers
                    .entry(token.file_origin.to_path_buf())
                    .or_default()
                    .push(Identifier {
                        start: token.byte_start_index,
                        end: token.byte_end_index,
                        name: name.clone(),
                    });
            }
        }
    }
    for file in identifiers.values_mut() {
        file.sort_by_key(|identifier| (identifier.start, identifier.end));
        file.dedup_by_key(|identifier| (identifier.start, identifier.end));
    }
    identifiers
}

impl Indexer<'_> {
    fn unit(&mut self, registry: &HMIRUnitRegistry, unit: &HMIRUnit) {
        let mut env = HMIRSymbolEnv::new(registry, unit);

        for (id, def) in unit.defs() {
            let class = def_class(def);
            let name = source_name(&def.name().name);
            let targets = self.declaration_sites(*id, def);

            for declaration in def.declarations() {
                self.declaration(declaration, name, class, &targets);
                for (body, params) in declaration_bodies(declaration) {
                    self.body(&mut env, unit, body, params);
                }
            }
        }
    }

    fn declaration(
        &mut self,
        declaration: &HMIRDeclaration,
        name: &str,
        class: SymbolClass,
        targets: &[DeclarationSite],
    ) {
        let Some(site) = self.name_site(declaration.span(), name, Occurrence::Last) else {
            return;
        };
        let Some((_, start, end)) = declaration.span().source_bounds() else {
            return;
        };
        let Some(source) = self.sources.get(&site.file) else {
            return;
        };
        let range = source.range(start, end);

        let file = self.files.entry(site.file).or_default();
        file.symbols.push(IndexedSymbol {
            name: name.to_string(),
            class,
            range,
            selection: site.range,
        });
        file.references.push(Reference {
            range: site.range,
            class,
            targets: targets.to_vec(),
            ty: None,
        });
    }

    fn body(
        &mut self,
        env: &mut HMIRSymbolEnv,
        unit: &HMIRUnit,
        body: &HMIRBodyContext,
        params: &[HMIRLocalID],
    ) {
        let local_class = |id: HMIRLocalID| match params.contains(&id) {
            true => SymbolClass::Parameter,
            false => SymbolClass::Local,
        };

        let mut locals = FxHashMap::default();
        for (id, local) in body.locals() {
            let Some(name) = local.name() else {
                continue;
            };
            let occurrence = match params.contains(&id) {
                true => Occurrence::Last,
                false => Occurrence::First,
            };
            let Some(site) = self.name_site(local.span(), name.as_str(), occurrence) else {
                continue;
            };
            let declaration = self.declaration_text(local.span(), site.name_end, Some(site.name_end));
            let Some(target) = declaration_site(&site, HMIRDeclarationRole::Definition, declaration)
            else {
                continue;
            };
            let ty = self.facts.locals.get(&id).cloned();
            self.reference(site, local_class(id), vec![target.clone()], ty);
            locals.insert(id, target);
        }

        for (id, expr) in body.exprs() {
            let ty = self.facts.exprs.get(&id).cloned();
            let (name, class, targets, ty) = match expr.kind() {
                HMIRExprKind::Local(local) if local.body() == body.id() => {
                    let (Some(name), Some(target)) = (body.local(*local).name(), locals.get(local))
                    else {
                        continue;
                    };
                    let ty = self.facts.locals.get(local).cloned().or(ty);
                    (name.as_str().to_string(), local_class(*local), vec![target.clone()], ty)
                }
                HMIRExprKind::Def(def_id) => {
                    let Some(owner) = env.owner(def_id.brand()) else {
                        continue;
                    };
                    let Some(def) = owner.resolve_def(*def_id) else {
                        continue;
                    };
                    let name = source_name(&def.name().name).to_string();
                    let class = def_class(def);
                    (name, class, self.declaration_sites(*def_id, def), value_type(class, ty))
                }
                HMIRExprKind::Name { name, tag } => {
                    let mut class = None;
                    let mut targets = Vec::new();
                    for def_id in env.lookup(unit.id(), name, *tag) {
                        let Some(owner) = env.owner(def_id.brand()) else {
                            continue;
                        };
                        let Some(def) = owner.resolve_def(def_id) else {
                            continue;
                        };
                        class.get_or_insert(def_class(def));
                        targets.extend(self.declaration_sites(def_id, def));
                    }
                    let Some(class) = class else {
                        continue;
                    };
                    let name = name.name.as_str().to_string();
                    (name, class, targets, value_type(class, ty))
                }
                _ => {
                    if let Some(ty) = &ty {
                        self.typed_span(expr.span(), ty);
                    }
                    continue;
                }
            };

            if let Some(site) = self.name_site(expr.span(), &name, Occurrence::Last) {
                self.reference(site, class, targets, ty);
            }
        }
    }

    fn reference(
        &mut self,
        site: Site,
        class: SymbolClass,
        targets: Vec<DeclarationSite>,
        ty: Option<String>,
    ) {
        self.files
            .entry(site.file)
            .or_default()
            .references
            .push(Reference {
                range: site.range,
                class,
                targets,
                ty,
            });
    }

    fn typed_span(&mut self, span: &TokenRange, ty: &str) {
        if matches!(ty, "void" | "unreachable") {
            return;
        }
        let Some((file, start, end)) = span.source_bounds() else {
            return;
        };
        let Some(source) = self.sources.get(file) else {
            return;
        };
        let range = source.range(start, end);
        self.files
            .entry(file.to_path_buf())
            .or_default()
            .types
            .push(TypedSpan {
                range,
                ty: ty.to_string(),
            });
    }

    fn declaration_sites(&mut self, id: HMIRDefID, def: &HMIRDef) -> Vec<DeclarationSite> {
        if let Some(sites) = self.sites.get(&id) {
            return sites.clone();
        }
        let name = source_name(&def.name().name);
        let sites = def
            .declarations()
            .iter()
            .filter_map(|declaration| {
                let site = self.name_site(declaration.span(), name, Occurrence::Last)?;
                let text = self.declaration_text(declaration.span(), site.name_end, None);
                declaration_site(&site, declaration.role(), text)
            })
            .collect::<Vec<_>>();
        self.sites.insert(id, sites.clone());
        sites
    }

    fn declaration_text(&mut self, span: &TokenRange, name_end: usize, until: Option<usize>) -> Declaration {
        let collapse = |text: &str| text.split_whitespace().collect::<Vec<_>>().join(" ");
        let Some((source, start, end)) = span
            .source_bounds()
            .and_then(|(file, start, end)| Some((self.sources.get(file)?, start, end)))
        else {
            return Declaration {
                text: String::new(),
                name_end: 0,
            };
        };
        Declaration {
            text: collapse(source.slice(start, until.unwrap_or(end))),
            name_end: collapse(source.slice(start, name_end)).len(),
        }
    }

    fn name_site(&mut self, span: &TokenRange, name: &str, occurrence: Occurrence) -> Option<Site> {
        let (file, start, end) = span.source_bounds()?;
        let identifiers = self.identifiers.get(file)?;
        let from = identifiers.partition_point(|identifier| identifier.start < start);
        let mut matches = identifiers[from..]
            .iter()
            .take_while(|identifier| identifier.end <= end)
            .filter(|identifier| identifier.name == name);
        let found = match occurrence {
            Occurrence::First => matches.next(),
            Occurrence::Last => matches.last(),
        }?;
        let name_end = found.end;
        let range = self.sources.get(file)?.range(found.start, found.end);
        Some(Site {
            file: file.to_path_buf(),
            range,
            name_end,
        })
    }

    fn finish(self) -> FxHashMap<Url, FileIndex> {
        self.files
            .into_iter()
            .filter_map(|(file, mut index)| {
                let uri = Url::from_file_path(&file).ok()?;
                index.references.sort_by_key(|reference| reference.range.start);
                index.references.dedup_by_key(|reference| reference.range);
                index.symbols.sort_by_key(|symbol| symbol.selection.start);
                index.symbols.dedup_by_key(|symbol| symbol.selection);
                Some((uri, index))
            })
            .collect()
    }
}

fn declaration_site(
    site: &Site,
    role: HMIRDeclarationRole,
    declaration: Declaration,
) -> Option<DeclarationSite> {
    Some(DeclarationSite {
        location: Location {
            uri: Url::from_file_path(&site.file).ok()?,
            range: site.range,
        },
        role,
        declaration,
    })
}

fn value_type(class: SymbolClass, ty: Option<String>) -> Option<String> {
    match class {
        SymbolClass::Global | SymbolClass::Local | SymbolClass::Parameter => ty,
        SymbolClass::Function | SymbolClass::Type => None,
    }
}

fn declaration_bodies(
    declaration: &HMIRDeclaration,
) -> Vec<(&HMIRBodyContext, &[HMIRLocalID])> {
    match declaration.kind() {
        Some(HMIRDefKind::Function(function)) => {
            let contract = function.signature().contract();
            vec![
                (function.body(), function.params()),
                (contract.body(), contract.params()),
                (function.prototype().defaults(), &[]),
            ]
        }
        Some(HMIRDefKind::Global(global)) => vec![(global.body(), &[])],
        Some(HMIRDefKind::ComptimeGlobal(global)) => vec![(global.body(), &[])],
        None => vec![],
    }
}

fn def_class(def: &HMIRDef) -> SymbolClass {
    for declaration in def.declarations() {
        if declaration.tag().is_some() {
            return SymbolClass::Type;
        }
        match declaration.kind() {
            Some(HMIRDefKind::Function(_)) => return SymbolClass::Function,
            Some(HMIRDefKind::Global(_)) => return SymbolClass::Global,
            Some(HMIRDefKind::ComptimeGlobal(global)) if declares_type(global) => {
                return SymbolClass::Type;
            }
            Some(HMIRDefKind::ComptimeGlobal(_)) => return SymbolClass::Global,
            None => {}
        }
    }
    SymbolClass::Global
}

fn declares_type(global: &HMIRComptimeGlobal) -> bool {
    matches!(
        global.body().expr(global.ty()).kind(),
        HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRGenericTypeKind::Type)
    )
}

fn source_name(name: &CXIdent) -> &str {
    name.as_str().rsplit(' ').next().unwrap_or_default()
}
