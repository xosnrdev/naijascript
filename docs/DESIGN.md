# NaijaScript Design Philosophy

## 1. Philosophy

NaijaScript is designed to make programming approachable and culturally resonant for Nigerian developers and for anyone who relates to the language patterns of Pidgin English. The language and its interpreter stay intentionally simple so learners can focus on core concepts without wrestling with unnecessary ceremony.

Our design principles:

- **Approachability**: Lower the barrier to entry. The language borrows familiar phrasing to help first-time programmers feel at home while they learn.
- **Clarity**: Keep syntax readable and direct. We choose a small, orthogonal set of features instead of sprawling surface area, so programs remain easy to follow.
- **Iterative Growth**: The project welcomes rapid experimentation and community feedback. We are comfortable shipping breaking changes when they lead to more expressive or more learnable features.

## 2. Interpreter Architecture

NaijaScript uses a **tree-walk interpreter**. This architecture keeps the implementation straightforward, emphasizes readability, and mirrors the pedagogical approach in [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/).

Key runtime characteristics:

- **Recursive evaluation** of Abstract Syntax Tree (AST) nodes keeps execution easy to reason about and aligns with the language’s procedural feel.
- **Context-rich diagnostics** surface line and column information and present error messages in Pidgin-styled prose so learners receive feedback that feels familiar.
- **Extensibility** remains a priority. The AST and runtime can expand with new value types and control structures without reworking the existing pipeline.

## 3. Language Design Rationale

- **Pidgin-inspired syntax** makes the language less intimidating for first-time programmers in Nigeria and across the diaspora.
- **Dynamic typing** avoids ceremony around type declarations and encourages experimentation, which is ideal for scripting tasks and learning environments.
- **Block scoping** (`start ... end`) prevents accidental variable leakage and is enforced by both the resolver and runtime.

## 4. Further Reading

- [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/)
- [Onyejelem, P. O. (2020). The influence of Pidgin English on educational outcomes among secondary school students in Nigeria. _International Journal of English and Literature, 11_(3), 46–52.](https://academicjournals.org/journal/IJEL/article-full-text/1F0EB5564993)
