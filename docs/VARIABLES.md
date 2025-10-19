# Variables

## Declaration

Declare a variable with an initial value:

```naijascript
make foo get 42
make bar get "hello"
make baz get true
```

You can also declare a variable without an initial value. That creates an uninitialized variable with dynamic typing:

```naijascript
make foo
make bar
```

## Assignment

Assign or reassign a variable:

```naijascript
foo get 10
bar get foo
```

## Shadowing

NaijaScript allows redeclaring the same variable name in the same scope. The most recent declaration in that scope is used at runtime:

```naijascript
make foo get 1
make foo get 2  # This shadows the earlier foo in the same scope
foo             # evaluates to 2
```

Inner blocks can shadow outer variables. The inner declaration hides the outer one until the inner block ends:

```naijascript
make foo get 1

start
    make foo get 2
    foo          # inner foo is 2
end

foo              # outer foo is still 1
```
