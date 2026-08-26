# Loops

Loops repeat a block while a boolean condition holds. NaijaScript uses the `jaara` construct for pre-checked looping (while-style).

## Syntax

```naijascript
jaara (condition) start
    # Executes block continuously while condition is true
end
```

## Operators

Available comparison operators:

| Operator     | Description  |
| ------------ | ------------ |
| `na`         | equal to     |
| `pass`       | greater than |
| `small pass` | less than    |
| `no be`      | not equal to |
| `no pass`    | less than or equal |
| `no small pass` | greater than or equal |

## Collection iteration

Use `for each <name> inside <collection>` to visit array elements or record
values. The loop variable is scoped to the loop body.

```naijascript
for each name inside ["Ada", "Bola"] start
    shout(name)
end
```

## Examples

Simple counter loop (prints 0 through 4):

```naijascript
make foo get 0

jaara (foo small pass 5) start
    shout(foo)
    foo get foo add 1
end
```

FizzBuzz (1 to 100):

```naijascript
make foo get 1

jaara (foo small pass 101) start
    if na so (foo mod 15 na 0) start
        shout("FizzBuzz")
    end
    if no be so start
        if na so (foo mod 3 na 0) start
            shout("Fizz")
        end
        if no be so start
            if na so (foo mod 5 na 0) start
                shout("Buzz")
            end
            if no be so start
                shout(foo)
            end
        end
    end
    foo get foo add 1
end
```

## Loop Control Flow

### Break Statement (`comot`)

Exit the innermost loop immediately:

```naijascript
make i get 0
jaara (i small pass 10) start
    if na so (i na 5) start
        comot  # Exit loop when i reaches 5
    end
    shout(i)
    i get i add 1
end
# Prints: 0, 1, 2, 3, 4
```

### Continue Statement (`next`)

Skip to the next iteration of the innermost loop:

```naijascript
make i get 0
jaara (i small pass 5) start
    i get i add 1
    if na so (i mod 2 na 0) start
        next  # Skip even numbers
    end
    shout(i)
end
# Prints: 1, 3, 5
```

### Nested Loops

`comot` and `next` affect only the innermost loop:

```naijascript
make outer get 0
jaara (outer small pass 3) start
    make inner get 0
    jaara (inner small pass 3) start
        if na so (inner na 1) start
            comot  # Exits inner loop only
        end
        inner get inner add 1
    end
    outer get outer add 1
end
```
