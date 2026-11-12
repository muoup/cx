use crate::index::{DeclarationSite, FileIndex, IndexedSymbol, Reference, SymbolClass};
use crate::position::SourceCache;
use cx_hmir::body::{HMIRBodyContext, HMIRLocalID};
use cx_hmir::expr::HMIRExprKind;
use cx_hmir::ty::HMIRGenericTypeKind;
use cx_hmir::unit::def::{HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefKind};
use cx_hmir::unit::global::HMIRComptimeGlobal;
use cx_hmir::unit::HMIRUnit;
use cx_hmir_lowering::env::symbols::HMIRSymbolEnv;
use cx_pipeline_data::db::ModuleData;
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

#[derive(Clone, Copy)]
enum Occurrence {
    First,
    Last,
}

struct Site {
    file: PathBuf,
    range: Range,
}

struct Indexer<'a> {
    identifiers: FxHashMap<PathBuf, Vec<Identifier>>,
    sources: &'a mut SourceCache,
    files: FxHashMap<PathBuf, FileIndex>,
}

pub fn from_context(
    context: &GlobalCompilationContext,
    sources: &mut SourceCache,
) -> FxHashMap<Url, FileIndex> {
    let db = &context.module_db;
    let namespaces = db.lex_tokens.lock().keys().cloned().collect::<Vec<_>>();
    let mut indexer = Indexer {
        identifiers: identifiers(db),
        sources,
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

        for (_, def) in unit.defs() {
            let class = def_class(def);
            let name = source_name(&def.name().name);
            let targets = self.declaration_sites(def);

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
        let Some(site) = self.name_site(declaration.span(), name, Occurrence::First) else {
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
            let Some(site) = self.name_site(local.span(), name.as_str(), Occurrence::First) else {
                continue;
            };
            let Some(target) = declaration_site(&site, HMIRDeclarationRole::Definition) else {
                continue;
            };
            self.reference(site, local_class(id), vec![target.clone()]);
            locals.insert(id, target);
        }

        for (_, expr) in body.exprs() {
            let (name, class, targets) = match expr.kind() {
                HMIRExprKind::Local(id) if id.body() == body.id() => {
                    let (Some(name), Some(target)) = (body.local(*id).name(), locals.get(id)) else {
                        continue;
                    };
                    (name.as_str().to_string(), local_class(*id), vec![target.clone()])
                }
                HMIRExprKind::Def(id) => {
                    let Some(owner) = env.owner(id.brand()) else {
                        continue;
                    };
                    let Some(def) = owner.resolve_def(*id) else {
                        continue;
                    };
                    let name = source_name(&def.name().name).to_string();
                    (name, def_class(def), self.declaration_sites(def))
                }
                HMIRExprKind::Name { name, tag } => {
                    let mut class = None;
                    let mut targets = Vec::new();
                    for id in env.lookup(unit.id(), name, *tag) {
                        let Some(owner) = env.owner(id.brand()) else {
                            continue;
                        };
                        let Some(def) = owner.resolve_def(id) else {
                            continue;
                        };
                        class.get_or_insert(def_class(def));
                        targets.extend(self.declaration_sites(def));
                    }
                    let Some(class) = class else {
                        continue;
                    };
                    (name.name.as_str().to_string(), class, targets)
                }
                _ => continue,
            };

            if let Some(site) = self.name_site(expr.span(), &name, Occurrence::Last) {
                self.reference(site, class, targets);
            }
        }
    }

    fn reference(&mut self, site: Site, class: SymbolClass, targets: Vec<DeclarationSite>) {
        self.files
            .entry(site.file)
            .or_default()
            .references
            .push(Reference {
                range: site.range,
                class,
                targets,
            });
    }

    fn declaration_sites(&mut self, def: &HMIRDef) -> Vec<DeclarationSite> {
        let name = source_name(&def.name().name);
        def.declarations()
            .iter()
            .filter_map(|declaration| {
                let site = self.name_site(declaration.span(), name, Occurrence::First)?;
                declaration_site(&site, declaration.role())
            })
            .collect()
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
        let range = self.sources.get(file)?.range(found.start, found.end);
        Some(Site {
            file: file.to_path_buf(),
            range,
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

fn declaration_site(site: &Site, role: HMIRDeclarationRole) -> Option<DeclarationSite> {
    Some(DeclarationSite {
        location: Location {
            uri: Url::from_file_path(&site.file).ok()?,
            range: site.range,
        },
        role,
    })
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
