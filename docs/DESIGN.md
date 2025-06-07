# NaijaScript Interpreter – Design Philosophy

> **Audience:** Contributors and language designers.
> **Purpose:** Explain the guiding philosophy and architectural principles behind NaijaScript.

## Philosophy

NaijaScript is designed to make programming more accessible and relatable for Nigerians and Pidgin English speakers. The language and its interpreter are intentionally simple, prioritizing learnability, expressiveness, and cultural relevance.

- **Approachability:** Informed by research on the sociolinguistic status and educational impact of Pidgin English in Nigeria (see [Onyejelem, 2020](https://academicjournals.org/journal/IJEL/article-full-text/1F0EB5564993)), which highlights Pidgin's role as a lingua franca and its deep cultural roots. While Pidgin English can lower barriers to communication, its habitual use in education can affect mastery of standard English, suggesting the value of localized programming for engagement but also the need for careful curriculum design.
- **Experimentation:** The project is open to breaking changes and rapid iteration, following the philosophy of “learning by doing.”

## Interpreter Architecture

NaijaScript uses a **tree-walk interpreter** architecture, where source code is parsed into an Abstract Syntax Tree (AST), and the interpreter recursively evaluates the AST nodes.

- **Recursive Evaluation:** Each AST node is visited and evaluated using a functional visitor pattern, as described in [Crafting Interpreters](https://craftinginterpreters.com/).
- **Block Scoping:** Variable environments are managed as a stack of hashmaps, supporting block-local variables and shadowing.
- **Error Reporting:** All errors are surfaced in Pidgin English, with contextual line/column information, inspired by user-first error design.
- **Extensibility:** The AST and interpreter are designed to be easily extended with new value types and control flow constructs, following best practices from scripting language research.

## Design Rationale

- **Why Tree-Walk?**  
  Tree-walk interpreters are easy to implement, debug, and extend, making them ideal for experimental and educational languages ([Crafting Interpreters](https://craftinginterpreters.com/)).
- **Why Pidgin English?**  
  Pidgin English is the most widely spoken language code in Nigeria, serving as a lingua franca and a marker of identity and solidarity ([Onyejelem, 2020](https://academicjournals.org/journal/IJEL/article-full-text/1F0EB5564993)). Using local language lowers the barrier to entry and makes programming more inclusive, but must be balanced with educational outcomes.
- **Why Block Scoping?**  
  Block scoping is familiar from modern languages and supports safe, predictable variable lifetimes.

## Further Reading

- [Crafting Interpreters by Bob Nystrom](https://craftinginterpreters.com/)
- [Onyejelem, P. O. (2020). The influence of Pidgin English on educational outcomes among secondary school students in Nigeria. International Journal of English and Literature, 11(3), 46-52.](https://academicjournals.org/journal/IJEL/article-full-text/1F0EB5564993)
