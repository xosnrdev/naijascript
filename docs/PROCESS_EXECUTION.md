# Process Execution

NaijaScript can run native child processes with `command(program)`.

This feature is for scripting and workflow automation. It is:

- direct execution, not shell execution
- synchronous
- available on native targets
- unsupported on wasm

## Quick start

```naijascript
make cmd get command("git")
cmd.arg("status")
cmd.stdout_capture()
make res get cmd.run()

if to say (res.success()) start
    shout(res.stdout())
end
```

`command(program)` returns a `process_command` builder. `run()` returns a
`process_result`.

## Builder methods

`process_command` supports:

- `arg(value)`
- `cwd(path)`
- `env(key, value)`
- `stdin_text(text)`
- `stdin_inherit()`
- `stdin_null()`
- `stdout_capture()`
- `stdout_inherit()`
- `stdout_null()`
- `stderr_capture()`
- `stderr_inherit()`
- `stderr_null()`
- `timeout_ms(value)`
- `run()`

Mutating builder methods update the command in place and return `null`.

## Result methods

`process_result` supports:

- `success()`
- `exit_code()`
- `stdout()`
- `stderr()`

Non-zero exit status is normal result data. It is not an interpreter error.

## Common patterns

### Capture output

```naijascript
make cmd get command("git")
cmd.arg("rev-parse")
cmd.arg("--short")
cmd.arg("HEAD")
cmd.stdout_capture()

make res get cmd.run()
if to say (res.success()) start
    shout("commit: {res.stdout()}")
end
```

### Set working directory and environment

```naijascript
make cmd get command("git")
cmd.arg("status")
cmd.cwd("/tmp/repo")
cmd.env("GIT_PAGER", "cat")
cmd.stdout_capture()

make res get cmd.run()
shout(res.stdout())
```

### Feed stdin

```naijascript
make cmd get command("sort")
cmd.stdin_text("b\na\n")
cmd.stdout_capture()

make res get cmd.run()
shout(res.stdout())
```

## Notes

- There is no shell interpolation.
- Argument boundaries are preserved exactly.
- Captured stdout and stderr are UTF-8 text.
- Oversized captured output fails with a runtime error instead of truncating.
