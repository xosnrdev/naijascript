# NaijaScript Interpreter Design Document

- Purpose and Scope

  - This document provides a technical overview of the NaijaScript interpreter’s architecture, design decisions, and implementation details. It is intended for developers and maintainers working on the codebase. It is not an official user manual or README, but an internal reference to guide development, maintenance, and future enhancements.

- System Overview

NaijaScript is a tree-walking interpreter for a simple scripting language. Its primary goal is to provide an approachable, expressive scripting environment for learning and automation, with a focus on Nigerian developers. The interpreter is implemented in Rust and uses Pidgin English for error messages.

- Architecture

```
Source Code → Lexer → Parser → AST → Interpreter → Output
```

    - **Lexer**: Tokenizes input, supports multi-word keywords, and provides error diagnostics.
    - **Parser**: Builds an AST using recursive descent and Pratt parsing for expressions.
    - **Interpreter**: Walks the AST, manages variable scopes, and executes statements.

    * Visual Overview

    ```mermaid
    graph TD;
        SourceCode[Source Code]
        Lexer[Lexer]
        Parser[Parser]
        AST[AST]
        Interpreter[Interpreter]
        Output[Output]
        SourceCode --> Lexer --> Parser --> AST --> Interpreter --> Output
    ```

- Design Considerations

  - **Simplicity**: Minimal features for easy onboarding and maintenance.
  - **Error Friendliness**: Pidgin English errors with line/column context.
  - **Extensibility**: AST and interpreter are designed for future data types and features.
  - **Startup Speed**: Prioritized over runtime performance.

- Data Models

  - **Variables**: Stack-based scoping (vector of hashmaps).
  - **Values**: Only `Number (f64)` for now; future support for `String`, `Boolean`, `Array`.
  - **AST Nodes**: Strongly-typed Rust enums and structs.

- Error Handling

  - **Contextual Messages**: All errors include line/column and are phrased in Pidgin English.
  - **Graceful Degradation**: Interpreter attempts to continue or provide actionable suggestions where possible.
  - **Edge Cases**: Handles division by zero, undefined variables, and invalid syntax robustly.

- Security Considerations

  - **Sandboxing**: No file or system access from scripts.
  - **Input Validation**: Lexer and parser reject invalid or malicious input early.

- Testing Approach

  - **Unit Tests**: For lexer, parser, and interpreter.
  - **Integration Tests**: End-to-end validation.
  - **Error Injection**: Ensures user-friendly error reporting.

- Design Rationale

  - **Stack-Based Scoping**: Simplicity and block-structured language compatibility.
  - **Reference Counting**: Planned for future shared values.
  - **Pidgin English Errors**: Lowers intimidation for new programmers.
  - **Extensible AST**: Enables future language growth.

- Limitations and Future Work

  - **Performance**: Not for compute-intensive workloads.
  - **Data Types**: Only numbers implemented; others planned.
  - **Diagnostics**: Advanced error reporting is a future goal.

- Versioning and Maintenance

  - This document is version-controlled with the codebase.
  - Contributors must update it with significant design/code changes.
