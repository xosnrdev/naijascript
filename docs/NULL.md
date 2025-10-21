# Null

Null represents the absence of a value. It is a distinct runtime value, different from `true`, `false`, `0`, or the empty string. In boolean contexts, `null` is falsy.

## Literals

Declare null values:

```naijascript
make foo get null
```

A function with no return value implicitly returns `null`:

```naijascript
do do_nothing() start
    # no return statement
end

shout(typeof(do_nothing())) # prints "null"
```

## Comparisons

`null` can be compared using the `na` (equal to) operator:

```naijascript
make foo get null
make baz get null

shout(foo na baz)  # prints true
shout(foo na 0)  # prints false
shout(foo na "") # prints false
shout(foo na false) # prints false
```

## Boolean context

`null` behaves as falsy in conditional checks:

```naijascript
if to say(not null) start
    shout("null is falsy")
end
```
