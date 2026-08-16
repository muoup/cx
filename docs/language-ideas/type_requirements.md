# Type Requirements

> Historical design note. This file is exploratory and does not describe current compiler behavior.

This note predates the current ownership-attribute and `safe`-function work. It sketches a possible future feature in which aggregates carry semantic requirements that are checked at selected program boundaries rather than after every mutation.

## Original Idea

```c
struct Person {
  int age;
  birthdate_t* birthdate;
  char* name;
  char* address;

  requires(age == -1 || birthday != NULL, "If age is known, birthdate must be provided");
  requires(name != NULL || strlen(name) > 0, "Name must be provided");
  requires(address != NULL || strlen(address) > 0, "Address must be provided");
};
```

## Original Motivation

Continuous checking after every field mutation is too expensive and often ill-defined for transient invalid states. The motivating example was:

```c
void function() {
  Person p = (Person) {
    .age = -1,
    .birthdate = NULL,
    .name = "John Doe",
    .address = "123 Main St"
  };

  p.age = 30;
  p.birthdate = get_birthdate_for_person("John Doe");
}
```

Between those two assignments the aggregate is temporarily invalid, even though the enclosing operation is semantically reasonable.

## Likely Checkpoints

The original idea was to enforce such requirements only at boundary points such as:

- aggregate construction
- function entry
- function exit
- mutation through reference-like interfaces

## Status

Not implemented. Current CX ownership work is centered on:

- `@nocopy`
- `@nodrop`
- control-flow-aware move analysis
- `safe` functions and optional FMIR verification
