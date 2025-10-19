# Numbers

NaijaScript numbers are IEEE-754 floating point values that can represent both integers and decimals.

## Literals

Declare numeric values:

```naijascript
make foo get 42
make bar get 3.14
make baz get minus 2.5
```

## Methods

Available methods and return types:

| Method    | Description                       | Returns  |
| --------- | --------------------------------- | -------- |
| `abs()`   | Absolute value                    | `Number` |
| `sqrt()`  | Square root                       | `Number` |
| `floor()` | Round down to the nearest integer | `Number` |
| `ceil()`  | Round up to the nearest integer   | `Number` |
| `round()` | Round to the nearest integer      | `Number` |

## Examples

Call methods on literals and variables:

```naijascript
make foo get (minus 3.5).abs()
make bar get 9.0.sqrt()
make baz get 2.7.floor()
```

Use methods on variables and chain calls:

```naijascript
make foo get minus 2.5
make bar get foo.abs()
make baz get (minus 2.5).abs().sqrt()
```

## Operators

Available arithmetic operators:

| Operator | Description    |
| -------- | -------------- |
| `add`    | Addition       |
| `minus`  | Subtraction    |
| `times`  | Multiplication |
| `divide` | Division       |
| `mod`    | Modulo         |
