# Records

Records store values under string keys. Identifier keys are shorthand for
string keys; quoted keys support spaces and escapes.

```naijascript
make person get {name: "Ada", "home town": "Lagos"}
shout(person["name"])
person["age"] get 37
shout(typeof(person)) # "record"
```

Record indexing requires a string. Assignment updates an existing key or adds
a new key. Records preserve insertion order; `for each` visits their values in
that order.

## Comparisons

The canonical comparison spellings are:

- `na`: equal
- `no be`: not equal
- `small pass`: less than
- `no pass`: less than or equal
- `pass`: greater than
- `no small pass`: greater than or equal

## Modules and embedding

Core NaijaScript does not read files or resolve import paths because the parser
and runtime are embeddable on hosts without a filesystem. Embedders can provide
named source units through `modules::ModuleProvider` (or
`SliceModuleProvider`) and run each source through the normal pipeline.
Cross-module imports and automatic linking are not yet language features.
