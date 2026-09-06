use super::*;

struct Symbols {
    definitions: Vec<(QualifiedName, (bool, bool))>,
    aliases: Vec<(NamespacePath, NamespacePath)>,
}

impl QualifiedLookup for Symbols {
    type Output = (bool, bool);

    fn lookup_exact(&self, _: &NamespacePath, name: &QualifiedName) -> Option<Self::Output> {
        self.definitions.iter().find_map(|(candidate, rank)| (candidate == name).then_some(*rank))
    }

    fn resolve_aliases(&self, _: &NamespacePath, namespace: &NamespacePath) -> Vec<NamespacePath> {
        self.aliases.iter().filter_map(|(alias, target)| (alias == namespace).then_some(target.clone())).collect()
    }

    fn priority(&self, lexical_namespace: &NamespacePath, name: &QualifiedName, rank: &Self::Output) -> (u8, bool) {
        (if name.namespace == *lexical_namespace { 2 } else { u8::from(rank.0) }, rank.1)
    }
}

#[test]
fn namespace_priority_precedes_symbol_preference() {
    let local = NamespacePath::from_str("local");
    let remote = NamespacePath::from_str("remote");
    let friend = NamespacePath::from_str("friend");
    for (preferred, is_friend) in [(local.clone(), false), (friend, true)] {
        let symbols = Symbols {
            definitions: vec![
                (QualifiedName::new(remote.clone(), "Value".into()), (false, true)),
                (QualifiedName::new(preferred.clone(), "Value".into()), (is_friend, false)),
            ],
            aliases: vec![(NamespacePath::root(), remote.clone()), (NamespacePath::root(), preferred.clone())],
        };
        let QualifiedLookupResult::Found { resolved_name, .. } = symbols.qualified_lookup(&local, &QualifiedName::root("Value".into())) else { panic!("expected preferred namespace") };
        assert_eq!(resolved_name.namespace, preferred);
    }
}

#[test]
fn aliases_merge_deduplicate_and_expand_deep_prefixes() {
    let target = NamespacePath::from_str("library");
    let name = QualifiedName::new(NamespacePath::from_str("library::nested::Type"), "member".into());
    let symbols = Symbols {
        definitions: vec![(name.clone(), (false, false))],
        aliases: vec![(NamespacePath::from_str("alias"), target.clone()), (NamespacePath::from_str("alias"), target)],
    };
    let query = QualifiedName::new(NamespacePath::from_str("alias::nested::Type"), "member".into());
    let QualifiedLookupResult::Found { resolved_name, .. } = symbols.qualified_lookup(&NamespacePath::root(), &query) else { panic!("expected unique alias target") };
    assert_eq!(resolved_name, name);
}

#[test]
fn direct_names_do_not_hide_equally_ranked_aliases() {
    let direct = QualifiedName::new(NamespacePath::from_str("alias"), "Value".into());
    let indirect = QualifiedName::new(NamespacePath::from_str("library"), "Value".into());
    let symbols = Symbols {
        definitions: vec![(direct.clone(), (false, true)), (indirect.clone(), (false, true))],
        aliases: vec![(direct.namespace.clone(), indirect.namespace.clone())],
    };
    let QualifiedLookupResult::Ambiguous { candidates } = symbols.qualified_lookup(&NamespacePath::root(), &direct) else { panic!("expected ambiguity") };
    assert_eq!(candidates, vec![direct, indirect]);
}

#[test]
fn symbol_preference_breaks_only_namespace_ties() {
    let first = QualifiedName::new(NamespacePath::from_str("a"), "Value".into());
    let second = QualifiedName::new(NamespacePath::from_str("b"), "Value".into());
    let symbols = Symbols {
        definitions: vec![(first.clone(), (false, false)), (second.clone(), (false, true))],
        aliases: vec![(NamespacePath::root(), first.namespace), (NamespacePath::root(), second.namespace.clone())],
    };
    let QualifiedLookupResult::Found { resolved_name, .. } = symbols.qualified_lookup(&NamespacePath::root(), &QualifiedName::root("Value".into())) else { panic!("expected preferred symbol") };
    assert_eq!(resolved_name, second);
}
