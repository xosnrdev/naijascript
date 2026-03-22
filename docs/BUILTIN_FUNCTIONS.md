# Built-in Functions

## I/O functions

### shout(value)

Prints value to the console.

Example:

```naijascript
shout("Hello, World!")
```

### read_line(prompt)

Prompts the user and reads a single line from standard input. Returns a string with the line.

Example:

```naijascript
make name get read_line("Enter name: ")
shout("Hello {name}")
```

## Introspection

### typeof(value)

Returns the runtime type of value as a string: "number", "string", "boolean", "array", or "null".

Example:

```naijascript
make foo get 42
make t get typeof(foo)
shout("foo is a {t}")
```

### to_string(value)

Converts the given value to its string representation.

Example:

```naijascript
make x get 42
make s get to_string(x)
shout(s)
```

## Workflow

### command(program)

Creates a `process_command` builder for direct child-process execution. See
[Process Execution](PROCESS_EXECUTION.md) for the full method set and runtime
behavior.

Example:

```naijascript
make cmd get command("git")
cmd.arg("status")
cmd.stdout_capture()
make res get cmd.run()
shout(res.stdout())
```
