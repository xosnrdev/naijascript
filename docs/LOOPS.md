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

## Limitations

The current loop construct `jasi` lacks support for common loop control flows such as `break` and `continue`, as well as higher-level loop constructs common in most programming languages, like `for` or `foreach`. These features are planned for a future release to make looping more expressive and concise.
