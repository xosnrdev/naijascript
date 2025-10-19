# Functions

## Definition

Define a function with `do name(params) start ... end`:

```naijascript
do hello() start
    shout("hello")
end
```

Functions can accept zero or more parameters:

```naijascript
do add(foo, bar) start
    return foo add bar
end
```

## Parameters

Parameters are local names bound to the arguments passed at call time:

```naijascript
do greet(name) start
    shout("Hello {name}")
end

make foo get "Ada"
greet(foo)
```

## Return

Use `return` to send a value back to the caller:

```naijascript
do max(foo, bar) start
    if to say (foo small pass bar) start
        return bar
    end
    return foo
end

make result get max(3, 7)
```

If a function does not execute a `return`, it produces no value for the caller.

## Calling functions

Call a function by its name and argument list. You may use the result in expressions or assignments:

```naijascript
make x get add(2, 3)
shout("x = {x}")
```

## Scope and locals

Variables declared with `make` inside a function are local to that function. They do not affect the caller’s scope unless returned and assigned:

```naijascript
do example() start
    make foo get 1
    make bar get 2
    return foo add bar
end

make foo get 10
make result get example()
shout("outer foo = {foo}")    # outer foo still 10
```
