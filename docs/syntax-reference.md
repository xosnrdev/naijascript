# Syntax Reference

This page provides a complete reference for NaijaScript syntax, organized by language construct. Use this as a quick lookup for language features.

## Variables

### Declaration

```naijascript
make variable_name get value
```

### Assignment

```naijascript
variable_name get new_value
```

### Examples

```naijascript
make name get "Kemi"
make age get 25
make is_student get true
make scores get [85, 90, 92]

age get 26  # reassignment
```

## Data Types

### Numbers

```naijascript
make integer get 42
make decimal get 3.14159
make negative get 0 minus 10
```

### Strings

```naijascript
make greeting get "Hello!"
make name get "Adaeze"
make empty get ""

# String interpolation
make message get "My name is {name}"
```

### Booleans

```naijascript
make is_true get true
make is_false get false
```

### Arrays

```naijascript
make fruits get ["apple", "banana", "orange"]
make numbers get [1, 2, 3, 4, 5]
make mixed get ["text", 42, true]
make nested get [[1, 2], [3, 4]]
```

## Operators

### Arithmetic

```naijascript
a add b      # Addition
a minus b    # Subtraction
a times b    # Multiplication
a divide b   # Division
a mod b      # Modulus (remainder)
```

### Comparison

```naijascript
a na b           # Equal to
a pass b         # Greater than
a small pass b   # Less than
```

### Logical

```naijascript
a and b     # Logical AND
a or b      # Logical OR
not a       # Logical NOT
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

### String Functions

```naijascript
len(string)                      # String length
slice(string, start, length)     # Extract substring
upper(string)                    # Convert to uppercase
lower(string)                    # Convert to lowercase
trim(string)                     # Remove whitespace
find(string, substring)          # Find position of substring
replace(string, old, new)        # Replace text
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

## String Interpolation

```naijascript
make name get "Aisha"
make message get "Hello, {name}!"
make calculation get "Result: {5 add 3}"
```
