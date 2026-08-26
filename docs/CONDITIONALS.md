# Conditionals

Conditionals control flow by evaluating boolean expressions and running code blocks when those expressions are true.

## Syntax

```naijascript
if na so (condition) start
    # Executes block if condition is true
end
if no be so start
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
if na so (foo pass 10) start
    shout("foo is greater than 10")
end
```

Else statement:

```naijascript
if na so (foo na 0) start
    shout("foo is zero")
end

if no be so start
    shout("foo is not zero")
end
```

Else-if statement:

```naijascript
make n get 3

if na so (n small pass 2) start
    shout("small")
end

if no be so start
    if na so (n na 2) start
        shout("exactly two")
    end

    if no be so start
        shout("greater than two")
    end
end
```
