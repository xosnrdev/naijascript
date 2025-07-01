# NaijaScript Design Philosophy

## 1. Philosophy

NaijaScript is designed to make programming more accessible and relatable for Nigerians and Pidgin English speakers. The language and its interpreter are intentionally simple, prioritizing learnability, expressiveness, and cultural relevance.

Our design philosophy is guided by these core principles:

- **Pikin-Friendly (Approachability)**: The primary goal is to lower the barrier to entry for programming. By using Nigerian Pidgin English, we connect with a vast audience in a language they understand intuitively. This is informed by research on the sociolinguistic status of Pidgin in Nigeria, which highlights its role as a lingua franca and its deep cultural roots.
- **Sharp-Sharp (Simplicity and Readability)**: The syntax is clean and maps directly to common Pidgin phrases. We avoid complex symbols and abstract jargon, focusing on a small, orthogonal set of features that can be combined powerfully. The language should be easy to read and write, allowing new programmers to build confidence quickly.
- **Learn as You Go (Experimentation and Iteration)**: The project is open to breaking changes and rapid development. We believe in learning by doing and evolving the language based on community feedback and practical experience.

## 2. Interpreter Architecture

NaijaScript uses a **tree-walk interpreter** architecture. This choice is deliberate and central to our design philosophy.

- **Why Tree-Walk?** Tree-walk interpreters are straightforward to implement, debug, and extend. This makes them ideal for an educational and experimental language, as they allow us to focus on language features rather than complex compilation techniques. This approach is heavily inspired by the book [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/).

Key components of the architecture include:

- **Recursive Evaluation**: Each node in the Abstract Syntax Tree (AST) is evaluated recursively. This model is easy to reason about and aligns with the simple, procedural nature of the language.
- **Localized Error Reporting**: All errors are surfaced in Pidgin English, with clear, contextual line and column information. This is inspired by the excellent diagnostics in the Rust compiler and is crucial for a positive learning experience.
- **Planned Extensibility**: The AST and interpreter are designed to be easily extended with new value types (like booleans or lists) and control flow constructs, following best practices from scripting language research.

## 3. Language Design Rationale

- **Why Pidgin English?** Pidgin is the most widely spoken language in Nigeria, acting as a bridge across diverse ethnic groups. Using it as the foundation for a programming language makes coding less intimidating and more inclusive.
- **Dynamic Typing**: NaijaScript uses dynamic typing, meaning you don't have to declare the type of a variable. This is common in scripting languages and allows for a more flexible and less verbose coding style, which is beneficial for beginners.
- **Block Scoping**: Any variable declared inside a `start ... end` block is only visible within that block and its nested children. This matches the behavior of modern programming languages and helps prevent accidental variable shadowing or leakage. Block scoping is enforced both at the semantic analysis and runtime levels, ensuring soundness and predictability.

## 4. Further Reading

- [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/)
- [Onyejelem, P. O. (2020). The influence of Pidgin English on educational outcomes among secondary school students in Nigeria. International Journal of English and Literature, 11(3), 46-52.](https://academicjournals.org/journal/IJEL/article-full-text/1F0EB5564993)
