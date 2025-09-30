# NaijaScript Language Reference Manual (LRM)

## 1. Introduction

Welcome to the official Language Reference Manual for NaijaScript. This document is the authoritative source for NaijaScript syntax, semantics, and runtime behavior. It explains how programs are structured, how types and values behave, and what each command does when executed.

NaijaScript is designed to feel natural to speakers of Nigerian Pidgin while remaining approachable to anyone learning to program. The language favors clarity, cultural relevance, and a progressive path for experimentation.

## 2. Lexical Structure

This section describes how NaijaScript code is tokenized.

### 2.1. Identifiers

Identifiers name variables, functions, and other bindings. They may contain letters (a–z, A–Z), digits (0–9), and underscores (`_`), but must start with a letter or underscore.

Examples: `myVar`, `age`, `totalAmount`, `my_var`, `_hiddenValue`

### 2.2. Keywords

Keywords carry special meaning in NaijaScript and cannot be used as identifiers.

- `make`: Declare a new variable.
- `get`: Assign a value to a variable.
- `if to say`: Conditional `if` keyword.
- `if not so`: Conditional `else` keyword.
- `jasi`: Loop keyword.
- `start`: Begin a block.
- `end`: End a block.
- `do`: Define a function.
- `return`: Return a value from a function.
- `add`, `minus`, `times`, `divide`, `mod`: Arithmetic operators.
- `na`, `pass`, `small pass`: Comparison operators.
- `and`, `or`, `not`: Logical operators.

### 2.3. Literals

Literals represent fixed values written directly in source code.

- **Numbers**: NaijaScript supports floating-point literals.
  - Examples: `10`, `3.142`, `0.5`
- **Strings**: Character sequences enclosed in double quotes (`"..."`) or single quotes (`'...'`). Supported escape sequences: `\\`, `\"`, `\'`, `\n`, `\t`, `\r`.
  - Examples: `"Hello, World!"`, `'Naija no dey carry last'`, `'foo\nbar'`
- **String Interpolation**: Strings can interpolate values within braces.
  - Example: `make name get "World" shout("Hello {name}")`
- **Booleans**: The keywords `true` and `false` represent truth values.
- **Arrays**: Ordered collections enclosed in square brackets.
  - Examples: `[1, 2, 3]`, `["naija", "4ever",]`, `[make_it(), other_value]`

### 2.4. Operators

- **Arithmetic**: `add` (+), `minus` (–), `times` (×), `divide` (/), `mod` (%)
- **Comparison**: `na` (==), `pass` (>), `small pass` (<)
- **Logical**: `and`, `or`, `not`
- **Unary**: `not` (logical negation), `minus` (arithmetic negation)

**Operator precedence (highest to lowest):**

1. Unary `not`, unary `minus`
2. `times`, `divide`, `mod`
3. Binary `add`, binary `minus`
4. `na`, `pass`, `small pass`
5. `and`
6. `or`

### 2.5. Punctuation

- `(` and `)`: Group expressions.
- `[` and `]`: Delimit array literals.

### 2.6. Whitespace and Comments

Whitespace (spaces, tabs, newlines) separates tokens and improves readability. NaijaScript ignores extra whitespace.

Comments begin with `#` and run to the end of the line. The interpreter discards comment text during execution.

## 3. Grammar and Syntax

The full grammar is defined in Extended Backus–Naur Form (EBNF). Reference the living grammar file here: [docs/grammar.ebnf](https://raw.githubusercontent.com/xosnrdev/naijascript/master/docs/grammar.ebnf).

## 4. Static Semantics

Static semantics describe checks performed before execution.

### 4.1. Type System

NaijaScript exposes four primary runtime types:

- **Number**: 64-bit floating-point values (`10`, `99.9`).
- **String**: Text data (`"Naija"`).
- **Boolean**: Truth values (`true`, `false`).
- **Array**: Ordered collections of values.

Types are inferred automatically. You do not declare types explicitly.

### 4.2. Name Binding and Scoping

- **Declaration**: Declare variables with `make`.
  - Example: `make myVar get 10`
- **Redeclaration**: Redeclaring a name within the same block emits a semantic error.
- **Scope**: Bindings introduced inside `start ... end` remain visible only within that block and its children. Once control leaves the block, the bindings go out of scope. This mirrors modern language scoping rules and prevents accidental leakage.

## 5. Dynamic Semantics

Dynamic semantics capture runtime behavior.

### 5.1. Statements

- **`make <variable> get <expression>`**: Declare a variable and initialize it with the expression value.
- **`<variable> get <expression>`**: Assign a new value to an existing variable.
- **`if to say (<condition>) <block>`**: Execute the block when the condition evaluates to `true`. Conditions may combine comparisons with `and`, `or`, and `not`.
- **`if to say (<condition>) <block> if not so <block>`**: Execute the first block when the condition is `true`; otherwise run the second block.
- **`jasi (<condition>) <block>`**: Loop while the condition remains `true`. Conditions can contain compound logical expressions.
- **`start ... end`**: Introduce a new block scope. Variables declared within the block expire when the block exits.
- **`do <name>(<parameters>) <block>`**: Define a function with the specified parameters and body.
- **`return <expression>`**: Exit a function and optionally provide a value. Without an expression, the runtime returns `0`.
- **Expression statement**: Evaluate an expression for its side effects (common for function calls).

### 5.2. Expressions

- **Addition and concatenation (`add`)**: Combines numbers or strings. Booleans cannot participate and cause a semantic error during analysis. Examples: `2 add 3`, `"foo" add "bar"`, `2 add "bar"` are valid; `true add 5` is rejected.
- **Arithmetic operators (`minus`, `times`, `divide`, `mod`)**: Require numeric operands. Mixing strings or booleans triggers a semantic error even when dynamic values are involved.
- **Dynamic operands**: Operations involving dynamic values succeed unless type rules are proven invalid (for example, boolean addition or string multiplication).
- **Comparisons** (`na`, `pass`, `small pass`): Compare operands and return a boolean.
- **Logical operators** (`and`, `or`, `not`): Operate exclusively on boolean values.
- **Function calls**: Invoke a function name followed by its arguments in parentheses and evaluate to the function’s return value.
- **Array literals**: Evaluate each element from left to right before creating the array.

## 6. Built-In Functions

NaijaScript exposes several built-ins that are always available.

### 6.1. Input/Output

- **`shout(expression)`**: Print the evaluated value to standard output.
- **`read_line()`**: Read a line from standard input and return it as a string.

### 6.2. Mathematics

Each math function accepts a number and returns a number:

- **`abs(number)`**: Absolute value.
- **`sqrt(number)`**: Square root computed via Newton’s method. Negative inputs yield `NaN`.
- **`floor(number)`**: Round down toward negative infinity.
- **`ceil(number)`**: Round up toward positive infinity.
- **`round(number)`**: Round to the nearest integer. Halfway cases round away from zero.

### 6.3. Strings

String helpers are Unicode-aware and handle multibyte characters:

- **`len(string)`**: Count Unicode scalar values.
- **`slice(string, start, end)`**: Extract a substring from `start` (inclusive) to `end` (exclusive). Indices are zero-based and support negative offsets from the end.
- **`upper(string)`** / **`lower(string)`**: Convert case.
- **`find(string, needle)`**: Return the index of the first occurrence of `needle` or `-1` if absent.
- **`replace(string, needle, replacement)`**: Replace all occurrences of `needle` with `replacement`.
- **`trim(string)`**: Strip leading and trailing whitespace.

### 6.4. Type Conversion

- **`to_string(value)`**: Convert any supported value to its string representation.
- **`to_number(string)`**: Parse a string into a number. Invalid input yields `NaN`.

### 6.5. Type Introspection

- **`typeof(value)`**: Return the runtime type name (`"number"`, `"string"`, `"bool"`, `"array"`).

## 7. Implementation-Defined Behavior

- **Numeric representation**: All numbers are 64-bit IEEE 754 floating-point values.
- **Diagnostics**: On error, the interpreter halts, prints a Pidgin-styled diagnostic message, and includes line and column context.
