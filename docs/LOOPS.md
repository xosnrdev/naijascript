# Loops

Loops repeat a block while a boolean condition holds. NaijaScript uses the `jasi` construct for pre-checked looping (while-style).

## Syntax

```naijascript
jasi (condition) start
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

## Examples

Simple counter loop (prints 0 through 4):

```naijascript
make foo get 0

jasi (foo small pass 5) start
    shout(foo)
    foo get foo add 1
end
```

FizzBuzz (1 to 100):

```naijascript
make foo get 1

jasi (foo small pass 101) start
    if to say (foo mod 15 na 0) start
        shout("FizzBuzz")
    end
    if not so start
        if to say (foo mod 3 na 0) start
            shout("Fizz")
        end
        if not so start
            if to say (foo mod 5 na 0) start
                shout("Buzz")
            end
            if not so start
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
jasi (i small pass 10) start
    if to say (i na 5) start
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
jasi (i small pass 5) start
    i get i add 1
    if to say (i mod 2 na 0) start
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
jasi (outer small pass 3) start
    make inner get 0
    jasi (inner small pass 3) start
        if to say (inner na 1) start
            comot  # Exits inner loop only
        end
        inner get inner add 1
    end
    outer get outer add 1
end
```
