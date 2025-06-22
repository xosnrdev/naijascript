//! The semantic analyzer (or resolver) for NaijaScript.

use crate::diagnostics::{AsStr, Diagnostics, Severity};
use crate::syntax::parser::{Arena, Block, BlockId, Cond, CondId, Expr, ExprId, Stmt, StmtId};

/// Identifies any node in our AST for precise error reporting.
/// We use this instead of raw pointers because arena-based storage gives us
/// memory safety while still allowing efficient lookups by index.
#[derive(Copy, Clone, Debug)]
pub enum NodeId {
    Stmt(usize),
    Expr(usize),
    Cond(usize),
    Block(usize),
}

/// Represents the type of semantic errors that can occur
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticError {
    DuplicateDeclaration,
    AssignmentToUndeclared,
    UseOfUndeclared,
}

impl AsStr for SemanticError {
    fn as_str(&self) -> &'static str {
        match self {
            SemanticError::DuplicateDeclaration => "You don declare dis variable before",
            SemanticError::AssignmentToUndeclared => {
                "You dey try give value to variable wey I no sabi"
            }
            SemanticError::UseOfUndeclared => "You dey use variable wey I no sabi",
        }
    }
}

/// The heart of our semantic analysis - this walks through NaijaScript code
/// and catches logical errors that the parser can't detect.
///
/// Right now we're keeping things simple with a flat symbol table, meaning
/// variables declared anywhere are visible everywhere. This matches how the
/// grammar is structured but we could extend this later for block scoping.
pub struct SemAnalyzer<'src> {
    // These are borrowed references to the parser's AST arenas
    // We don't own this data, just analyze what the parser built
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,

    // Simple flat scope for now - stores variable names as they're declared
    // Using string slices from the source code avoids unnecessary allocations
    symbol_table: Vec<&'src str>,

    // Collect all errors instead of failing fast - gives better user experience
    pub errors: Diagnostics,
}

impl<'src> SemAnalyzer<'src> {
    /// Sets up a new analyzer with the AST arenas from parsing.
    /// We start with empty symbol table and error list - fresh slate for analysis.
    #[inline(always)]
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
    ) -> Self {
        SemAnalyzer {
            stmts,
            exprs,
            conds,
            blocks,
            symbol_table: Vec::new(),
            errors: Diagnostics::default(),
        }
    }

    /// Main entry point for semantic checking.
    /// Takes the root block (representing the whole program) and recursively
    /// validates everything inside it.
    #[inline(always)]
    pub fn analyze(&mut self, root: BlockId) {
        self.check_block(root);
    }

    /// Validates all statements within a block.
    /// Blocks in NaijaScript are wrapped with "start" and "end" keywords,
    /// but here we just process the list of statements inside.
    #[inline(always)]
    fn check_block(&mut self, bid: BlockId) {
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            self.check_stmt(sid);
        }
    }

    /// The main semantic validation logic - handles each type of statement.
    /// This is where we enforce the key rules of NaijaScript:
    /// 1. No redeclaring variables (each "make" creates a new variable)
    /// 2. Variables must be declared before use
    /// 3. All expressions and conditions must be semantically valid
    fn check_stmt(&mut self, sid: StmtId) {
        match &self.stmts.nodes[sid.0] {
            // Handle "make variable get expression" statements
            Stmt::Assign { var, expr, span } => {
                // Check for duplicate declarations first
                // In NaijaScript, once you "make" a variable, you can't "make" it again
                if self.symbol_table.contains(var) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic error",
                        SemanticError::DuplicateDeclaration.as_str(),
                        &[],
                    );
                } else {
                    // Add to our symbol table so future references know it exists
                    self.symbol_table.push(var);
                }
                // Always check the expression, even if variable was duplicate
                // This catches more errors in one pass
                self.check_expr(*expr);
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, expr, span } => {
                if !self.symbol_table.contains(var) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic error",
                        SemanticError::AssignmentToUndeclared.as_str(),
                        &[],
                    );
                }
                self.check_expr(*expr);
            }
            // Handle "shout(expression)" statements - just validate the expression
            Stmt::Shout { expr, .. } => {
                self.check_expr(*expr);
            }
            // Handle "if to say(condition) start...end" with optional "if not so"
            Stmt::If { cond, then_b, else_b, .. } => {
                self.check_cond(*cond);
                self.check_block(*then_b);
                // Else block is optional in the grammar
                if let Some(eb) = else_b {
                    self.check_block(*eb);
                }
            }
            // Handle "jasi(condition) start...end" loop statements
            Stmt::Loop { cond, body, .. } => {
                self.check_cond(*cond);
                self.check_block(*body);
            }
        }
    }

    /// Validates condition expressions used in if statements and loops.
    /// NaijaScript conditions are binary comparisons: "na" (equals),
    /// "pass" (greater), "small pass" (less than).
    /// Both sides must be valid expressions.
    fn check_cond(&mut self, cid: CondId) {
        let cond = &self.conds.nodes[cid.0];
        self.check_expr(cond.lhs);
        self.check_expr(cond.rhs);
    }

    /// Recursively validates expressions - the core of our semantic checking.
    /// This is where we catch the most common programming error: using variables
    /// before declaring them. Numbers are always valid, but variables need to
    /// exist in our symbol table.
    fn check_expr(&mut self, eid: ExprId) {
        match &self.exprs.nodes[eid.0] {
            // Numbers like 42 or 3.14 are always semantically valid
            Expr::Number(_, _) => {}
            // Variables must have been declared with "make" before use
            Expr::Var(v, span) => {
                if !self.symbol_table.contains(v) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic error",
                        SemanticError::UseOfUndeclared.as_str(),
                        &[],
                    );
                }
            }
            // Binary operations like "add", "minus", "times", "divide"
            // Both operands must be valid expressions (recursive validation)
            Expr::Binary { lhs, rhs, .. } => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::parser::Parser;

    #[test]
    fn test_semantic_duplicate_declaration() {
        let src = "make x get 1\nmake x get 2";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == SemanticError::DuplicateDeclaration.as_str()),
            "Expected duplicate declaration error"
        );
    }

    #[test]
    fn test_semantic_undeclared_variable() {
        let src = "shout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == SemanticError::UseOfUndeclared.as_str()),
            "Expected undeclared variable error"
        );
    }

    #[test]
    fn test_semantic_valid_program() {
        let src = "make x get 5\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer.errors.diagnostics.is_empty(),
            "Expected no semantic errors, got: {:#?}",
            analyzer.errors
        );
    }
}
