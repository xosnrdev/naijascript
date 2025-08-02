# NaijaScript Language Reference Manual (LRM)

## 1. Introduction

Welcome to the official Language Reference Manual for NaijaScript. This document na the authoritative source for everything about the NaijaScript language. E go explain the full syntax, how variables and types dey work, and wetin each command dey do when you run your code.

NaijaScript dey designed to make programming feel natural and accessible for Pidgin English speakers. Our goal na to create a language wey dey simple to learn, expressive, and culturally relevant.

## 2. Lexical Structure

This section dey describe how NaijaScript code dey break down into individual tokens.

### 2.1. Identifiers

Identifiers na the names you give to your variables. Dem fit contain letters (a-z, A-Z), numbers (0-9), and underscore (\_), but dem must to start with a letter or underscore.

Example: `myVar`, `age`, `totalAmount`, `my_var`, `_hiddenValue`

### 2.2. Keywords

Keywords na special words wey get meaning for NaijaScript. You no fit use them as variable names.

- `make`: Declare a new variable.
- `get`: Assign a value to a variable.
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
- **Strings**: Sequence of characters inside double quotes (`"..."`) or single quotes (`'...'`). Supported escapes: `\\`, `\"`, `\'`, `\n`, `\t`, `\r`.
  - Example: `"Hello, World!"`, `'Naija no dey carry last'`, `'foo\nbar'`
- **Booleans**: Truth values wey be `true` and `false`.
  - `true` - mean say e correct or e happen.
  - `false` - mean say e no correct or e no happen.

### 2.4. Operators

- **Arithmetic**: `add` (+), `minus` (-), `times` (\*), `divide` (/), `mod` (%)
- **Comparison**: `na` (==), `pass` (>), `small pass` (<)
- **Logical**: `and` (logical AND), `or` (logical OR), `not` (logical NOT)
- **Unary**: `not` (logical negation), `minus` (arithmetic negation)

**Operator Precedence (from highest to lowest):**

1. `not`, `minus` (unary operators)
2. `times`, `divide`, `mod`
3. `add`, `minus` (binary operators)
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
- **Scope**: Any variable or function wey you declare inside a `start ... end` block go only dey visible inside that block and any nested block. This help prevent accidental variable or function shadowing or leakage, and na the same way modern programming languages dey behave. After the block ends, the function or variable no go dey visible outside the block again.

## 5. Dynamic Semantics

This section dey explain wetin each part of the language dey do when e dey run.

### 5.1. Statements

- **`make <variable> get <expression>`**: This one go create a new variable and give am the value of the expression.
- **`<variable> get <expression>`**: This one go change the value of a variable wey you don already create.
- **`if to say (<condition>) <block>`**: If the condition na true, the code inside the `start`...`end` block go run. The condition fit be simple comparison or complex compound condition using `and`, `or`, and `not` operators.
- **`if to say (<condition>) <block> if not so <block>`**: If the condition na true, the first block go run. Otherwise, the second block go run.
- **`jasi (<condition>) <block>`**: The code inside the block go run repeatedly as long as the condition dey true. Like `if to say`, the condition fit be compound expression with multiple logical operators.
- **`start ... end`**: You fit use block anywhere as a statement. Any variable you declare inside the block go only dey visible inside that block and any nested block.
- **`do <name>(<parameters>) <block>`**: Define a function with the given name and parameters. The function body dey inside the block.
- **`return <expression>`**: Return a value from a function. If no expression provided, function go return 0. Only valid inside function definitions.
- **`<expression>`**: You fit use any expression as a statement. This dey useful for function calls wey you wan execute for their side effects.

### 5.2. Expressions

**Addition and Concatenation (`add`)**: You fit use the `add` operator to join or add any combination of string, number, or dynamic types. The only thing wey no dey allowed be boolean: if any side be boolean, e go cause semantic error before your code run. For example, you fit do `2 add 3`, `"foo" add "bar"`, `2 add "bar"`, or join dynamic values, but `true add 5` or `"foo" add false` no go work.

**Arithmetic Operators (`minus`, `times`, `divide`, `mod`)**: These operators fit only work if both sides be number, or dynamic wey go be number at runtime. If you try use string or boolean with any arithmetic operator (even if the other side na dynamic), e go cause semantic error before your code run.

**Dynamic Types**: If any operand be dynamic, the operation go pass unless one side be boolean (for `add`) or string/boolean (for arithmetic). The analyzer only block operation if e fit prove say the types no go work.

**Comparison**: Expressions with `na`, `pass`, and `small pass` go compare two values and return a boolean result (true or false) for use in conditions.

**Logical**: Expressions wey use `and`, `or`, and `not` dey join or change boolean values. Dem only work if the values na `true` or `false`. If you try use dem with number or string, e go cause a semantic error.

**Function Call**: You fit call a function by writing its name followed by arguments in parentheses. The function go run and return a value wey you fit use for further expressions.

## 6. Built-in Functions

NaijaScript provides several built-in functions wey dey globally available:

### 6.1. Output Functions

- **`shout(expression)`**: Print the value of the expression to the console. This function take any value (number, string, or boolean) and display am. The function return 0 after printing.

Built-in functions dey take precedence over user-defined functions with the same name. You fit call dem anywhere expression dey expected.

## 7. Implementation-Defined Behavior

- **Number Precision**: All numbers na 64-bit floating-point numbers (IEEE 754 standard).
- **Error Reporting**: When error happen, the interpreter go stop execution and print a message for Pidgin English wey go explain the problem and show the line and column number where the error occur.
