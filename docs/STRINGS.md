# Strings

NaijaScript strings are UTF-8 encoded and support Unicode characters.

## Literals

Declare string values:

```naijascript
make foo get "Aisha"
make bar get " hello "
```

## Concatenation and interpolation

Concatenate with add and interpolate with braces:

```naijascript
make foo get "Hello, " add "Aisha" add "!"
make bar get "Hello, {foo}!"
```

## Methods

Available methods and return types:

| Method              | Description                                                                                                  | Returns         |
| ------------------- | ------------------------------------------------------------------------------------------------------------ | --------------- |
| `len()`             | Number of Unicode code points in the string                                                                  | `Number`        |
| `slice(start, end)` | Substring from start (inclusive) to end (exclusive). start/end are floored; negative indexes count from end. | `String`        |
| `to_uppercase()`    | Convert to uppercase                                                                                         | `String`        |
| `to_lowercase()`    | Convert to lowercase                                                                                         | `String`        |
| `find(needle)`      | Index of first occurrence (0-based). Returns -1 if not found.                                                | `Number`        |
| `replace(old, new)` | Replace all occurrences of old with new                                                                      | `String`        |
| `trim()`            | Remove leading and trailing whitespace                                                                       | `String`        |
| `to_number()`       | Parse the string as a number, or NaN on failure                                                              | `Number`        |
| `split(pattern)`    | Split the string by the given pattern                                                                        | `Array<String>` |
