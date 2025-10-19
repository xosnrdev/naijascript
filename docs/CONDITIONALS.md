# Conditionals

Conditionals control flow by evaluating boolean expressions and running code blocks when those expressions are true.

## Syntax

```naijascript
if to say (condition) start
    # Executes block if condition is true
end
if not so start
    # Executes block if condition is false
end
```

## Operators

Available comparison operators:

| Operator     | Description  |
| ------------ | ------------ |
| `na`         | equal to     |
| `pass`       | greater than |
| `small pass` | less than    |

## Examples

If statement:

```naijascript
if to say (foo pass 10) start
    shout("foo is greater than 10")
end
```

Else statement:

```naijascript
if to say (foo na 0) start
    shout("foo is zero")
end

if not so start
    shout("foo is not zero")
end
```

Else-if statement:

```naijascript
make n get 3

if to say (n small pass 2) start
    shout("small")
end

if not so start
    if to say (n na 2) start
        shout("exactly two")
    end

    if not so start
        shout("greater than two")
    end
end
```
