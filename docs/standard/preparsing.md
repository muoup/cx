# Preparsing

Lexed tokens of one unit to its preparse contents: type-like names, imports, namespace aliases. Lets the [parser](parsing.md) tell types from values before any unit is parsed. No declaration is parsed; recognition is by token shape.

## Terms

*verified 425c5ff1*

- **registry**: compilation-wide map, namespace to symbols, filled by every preparsed unit. Symbol kinds: *type*, *tag*, *generator*.
- **module symbols**: the names one unit contributes, under its namespace, each with a visibility.
- **alias map**: namespace to the namespaces it may stand for; see [Lookup](hmir-lowering.md#lookup) and [Type start](parsing.md#type-start).

## Unit

*verified 425c5ff1*

The file is lexed with include directories and predefined macros; included text sits between include markers. Tokens scanned one at a time, nothing skipped by nesting, so names inside function bodies count. Yields the module path, imports, module symbols, alias map.

- alias map starts with root standing for the unit's own namespace.
- `rule/intrinsic-imports` units outside the standard library also import the intrinsic assertion module.
- module symbols inserted into the registry right after; the unit parses once its imports have preparsed.
- `inv/preparse-before-parse` every import of a unit is in the registry when the unit parses.

### Visibility

*verified 425c5ff1*

Recorded per symbol; starts *private*.
- `rule/quirk-initial-visibility` the parser starts at *package*; preparse at private.
- `public :` and `private :` set it. `rule/preparse-access-colon` colon missing: P0001, though the parser would accept it.
- `[public | private] extern "C" :` sets it, private unless `public`. Other ABI: P0011; missing `:`: P0001.
- include markers save and restore it. Unmatched end: P0003.
- `rule/quirk-registry-ignores-visibility` the registry keeps no visibility; every symbol is found from every unit.

### Tags

*verified 425c5ff1*

`struct n`, `union n`, `enum n` anywhere: n is a tag.
- `rule/quirk-tagged-union-tag` `enum union n` registers n through its `union` token.
- forward references, uses and definitions all count, `struct S *p` included.

### Typedef

*verified 425c5ff1*

From `typedef` to the first `;` outside brackets: the name is the last identifier outside all brackets, or an identifier directly after `(*` inside one level. Registered as a type. None found: nothing.
- `typedef struct { int x; } S;` gives S; `typedef int (*f)(int);` gives f.
- `rule/quirk-typedef-last-identifier` any identifier after the declarator name and outside brackets (`typedef int T __wur;`) is taken instead.

### Comptime types

*verified 425c5ff1*

After `comptime`:
- `@type n =` : type n.
- `@type n (` : generator n.
- `auto n : @type =` : type n.
- `auto n ( ... ) ... -> @type` : generator n; the `->` must come at depth zero before any `;`, `{` or `=>`.
- other shapes: nothing. `rule/quirk-comptime-type-shapes` a type-valued comptime name declared any other way (`comptime @type n;`, a typedef of a generator) is not a type to the parser.

### Imports

*verified 425c5ff1*

```ebnf
import = "import" tree ["as" (qualified-name | "_")] ";" ;
tree   = path | path "::" "{" tree { "," tree } [","] "}" ;
path   = name { "::" name } ;
```

Each leaf yields a qualified name: its group prefixes, then its path. The imported module is that name as a namespace.
- top-level comma: P0010. Stray `}`: P0002. Empty path or group item: P0001. Other token: P0002.
- `rule/no-self-import` importing the unit's own namespace: P0019.
- `rule/import-alias` `as a::b` adds each imported module as a target of namespace `a::b`; `as _` adds them to root, so their names are found unqualified.

## Registry

*verified 425c5ff1*

Per namespace: symbol kinds by name, child namespace names (recorded, never queried). Inserting a unit registers every prefix of its namespace as a child of its parent.
- `rule/tag-yields-to-kind` a tag never replaces an existing kind; type or generator replaces any, later inserts winning.
- `rule/quirk-global-registry` lookups see every unit preparsed so far, imported or not; only imports are guaranteed present, so a reach into a non-imported namespace depends on scheduling.
- lookup ranking and ambiguity: [Type start](parsing.md#type-start).
