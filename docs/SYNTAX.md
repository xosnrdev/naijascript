# Syntax Reference

This page provides a complete reference for NaijaScript syntax, organized by language construct.

> Use this as a quick lookup for language features and syntax.

## Operators

### Comparison

```naijascript
a na b           # Equal to
a pass b         # Greater than
a small pass b   # Less than
```

## Control Flow

### Conditional Statements

```naijascript
if to say (condition) start
    # code block
end

if to say (condition) start
    # if block
end
if not so start
    # else block
end
```

### Loops

```naijascript
jasi (condition) start
    # loop body
end
```

## Functions

### Function Declaration

```naijascript
do function_name(parameter1, parameter2) start
    # function body
    return value  # optional
end
```

### Function Call

```naijascript
function_name(argument1, argument2)
```

## Built-in Functions

### Output and Input

```naijascript
shout(value)                  # Print to console
make input get read_line()    # Read user input
```

### Math Functions

```naijascript
abs(number)         # Absolute value
sqrt(number)        # Square root
floor(number)       # Round down
ceil(number)        # Round up
round(number)       # Round to nearest integer
```

### Type Functions

```naijascript
typeof(value)        # Get type as string
to_string(value)     # Convert to string
to_number(string)    # Convert string to number
```

## Comments

```naijascript
# Single line comment
make value get 42  # Inline comment
```
