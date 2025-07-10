# NaijaScript Language Reference Manual (LRM)

## 1. Introduction

Welcome to the official Language Reference Manual for NaijaScript. This document na the authoritative source for everything about the NaijaScript language. E go explain the full syntax, how variables and types dey work, and wetin each command dey do when you run your code.

NaijaScript dey designed to make programming feel natural and accessible for Pidgin English speakers. Our goal na to create a language wey dey simple to learn, expressive, and culturally relevant.

## 2. Lexical Structure

This section dey describe how NaijaScript code dey break down into individual tokens.

### 2.1. Identifiers

Identifiers na the names you give to your variables. Dem fit contain letters (a-z, A-Z) and numbers (0-9), but dem must to start with a letter.

Example: `myVar`, `age`, `totalAmount`

### 2.2. Keywords

Keywords na special words wey get meaning for NaijaScript. You no fit use them as variable names.

- `make`: Declare a new variable.
- `get`: Assign a value to a variable.
- `shout`: Print output to the console.
- `if to say`: Conditional `if` statement.
- `if not so`: Conditional `else` statement.
- `jasi`: Loop construct.
- `start`: Beginning of a code block.
- `end`: End of a code block.
- `do`: Define a function.
- `return`: Return a value from a function.
- `add`, `minus`, `times`, `divide`, `mod`: Arithmetic operators.
- `na`, `pass`, `small pass`: Comparison operators.
- `and`, `or`, `not`: Logical operators.

### 2.3. Literals

Literals na the fixed values wey you dey write directly for your code.

- **Numbers**: NaijaScript dey support floating-point numbers.
  - Example: `10`, `3.142`, `0.5`
- **Strings**: A sequence of characters inside double quotes.
  - Example: `"Hello, World!"`, `"Naija no dey carry last"`
- **Booleans**: Truth values wey be `true` and `false`.
  - `true` - mean say e correct or e happen.
  - `false` - mean say e no correct or e no happen.

### 2.4. Operators

- **Arithmetic**: `add` (+), `minus` (-), `times` (\*), `divide` (/), `mod` (%)
- **Comparison**: `na` (==), `pass` (>), `small pass` (<)
- **Logical**: `and` (logical AND), `or` (logical OR), `not` (logical NOT)

**Operator Precedence (from highest to lowest):**

1. `not`
2. `times`, `divide`, `mod`
3. `add`, `minus`
4. `na`, `pass`, `small pass`
5. `and`
6. `or`

### 2.5. Punctuation

- `(` and `)`: Used for grouping expressions and in `shout`, `if to say`, and `jasi` statements.

### 2.6. Whitespace and Comments

Whitespace (like space, tab, or new line) na wetin you fit use to separate code make e clear. NaijaScript no dey worry about how many space or new line you put.

If you wan write comment, just put `#` for anywhere for the line. Anything wey dey after `#` till the end of that line na comment, the interpreter no go run am. You fit put comment anywhere for your code, e no go affect how your program work.

## 3. Grammar and Syntax

NaijaScript syntax dey defined with a formal grammar using Backus-Naur Form (BNF). You fit check the full grammar for this [link](https://raw.githubusercontent.com/xosnrdev/naijascript/master/docs/grammar.bnf).

## 4. Static Semantics

This section dey cover the rules wey dey checked before your code run.

### 4.1. Type System

NaijaScript get four main data types:

- **Number**: Represents numeric values (e.g., `10`, `99.9`). All numbers na floating-point numbers.
- **String**: Represents text (e.g., `"Naija"`).
- **Boolean**: Represents truth values wey fit be `true` or `false`.
- **Function**: Represents callable code blocks with parameters and return values.

Type inference dey automatic. You no need to declare the type of a variable.

### 4.2. Name Binding and Scoping

- **Declaration**: You must declare a variable with the `make` keyword before you use am.
  - `make myVar get 10`
- **Redeclaration**: You no fit declare a variable wey you don already declare for the same block.
- **Scope**: Any variable wey you declare inside a `start ... end` block go only dey visible inside that block and any nested block. Variable wey you declare for outside block no go dey visible inside the block if you redeclare am inside. This help prevent accidental variable shadowing or leakage, and na the same way modern programming languages dey behave.
- **Function Scope**: Functions create their own scope. Parameters become local variables inside the function. Variables declared inside a function no go affect variables for outside scope with the same name.

## 5. Dynamic Semantics

This section dey explain wetin each part of the language dey do when e dey run.

### 5.1. Statements

- **`make <variable> get <expression>`**: This one go create a new variable and give am the value of the expression.
- **`<variable> get <expression>`**: This one go change the value of a variable wey you don already create.
- **`shout(<expression>)`**: This one go evaluate the expression and print the result to the console.
- **`if to say (<condition>) <block>`**: If the condition na true, the code inside the `start`...`end` block go run.
- **`if to say (<condition>) <block> if not so <block>`**: If the condition na true, the first block go run. Otherwise, the second block go run.
- **`jasi (<condition>) <block>`**: The code inside the block go run repeatedly as long as the condition dey true.
- **`start ... end`**: You fit use block anywhere as a statement. Any variable you declare inside the block go only dey visible inside that block and any nested block.
- **`do <name>(<parameters>) <block>`**: Define a function with the given name and parameters. The function body dey inside the block.
- **`return <expression>`**: Return a value from a function. If no expression provided, function go return 0. Only valid inside function definitions.

### 5.2. Expressions

- **Arithmetic**: Expressions with `add`, `minus`, `times`, `divide`, and `mod` go perform the calculation and return a `Number`. If you try to divide by zero or use mod with zero, e go cause a runtime error.
- **String Concatenation**: You fit use the `add` operator to join two strings together.
- **Comparison**: Expressions with `na`, `pass`, and `small pass` go compare two values and return a boolean result (true or false) for use in conditions.
- **Logical**: Expressions wey use `and`, `or`, and `not` dey join or change boolean values. Dem only work if the values na `true` or `false`. If you try use dem with number or string, e go cause a semantic error.
- **Function Call**: You fit call a function by writing its name followed by arguments in parentheses. The function go run and return a value wey you fit use for further expressions.

## 6. Built-in Intrinsics

NaijaScript no get a standard library for now. All functionality dey provided through the keywords.

## 7. Implementation-Defined Behavior

- **Number Precision**: All numbers na 64-bit floating-point numbers (IEEE 754 standard).
- **Error Reporting**: When error happen, the interpreter go stop execution and print a message for Pidgin English wey go explain the problem and show the line and column number where the error occur.
