---
title: Control Flow
---

# Control Flow

CX keeps C-style control flow and adds tagged-union pattern dispatch through
`match` and `is`.

## `match`

`match` dispatches on patterns without fallthrough. Each case maps to its own
scope.

```c
match (out) {
    Output::integer(i) => printf("%d\n", i);
    Output::fp(d) => printf("%f\n", d);
    default => printf("unknown\n");
}
```

Tagged unions may be used in `match` statements so payload bindings are scoped
to the case that matched.

```c
void print_json(Json obj) {
    match (obj) {
        Json::string(str) => printf("%s", str);
        Json::number(num) => printf("%f", num);
        Json::null() => printf("null");
    }
}
```

## `is`

`is` checks whether a tagged union has a specific variant.

```c
if (out is Output::string(s)) {
    printf("%s\n", s);
}
```

The operator returns a boolean. The pattern binding is introduced
unconditionally, but using that binding outside a successful check is undefined
behavior.

```c
out is Output::string(s);

assert(out is Output::string(s));
printf("%s\n", s);
```
