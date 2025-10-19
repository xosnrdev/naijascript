# Comments

Any text following a `#` symbol on a line is treated as a comment and ignored by the interpreter.

## Single-line

```naijascript
# This is a full-line comment
make foo get 1
# Comment between statements
make bar get 2
```

## Inline

```naijascript
make foo get 1    # initialize foo
make bar get foo  # copy foo to bar
make baz get 3    # set baz to 3
```
