//! The semantic analyzer (or resolver) for NaijaScript.

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    Arena, BinOp, Block, BlockId, Cond, CondId, Expr, ExprId, Stmt, StmtId,
};

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
    TypeMismatch,
    InvalidStringOperation,
}

impl AsStr for SemanticError {
    fn as_str(&self) -> &'static str {
        match self {
            SemanticError::DuplicateDeclaration => "You don declare dis variable before",
            SemanticError::AssignmentToUndeclared => {
                "You dey try give value to variable wey I no sabi"
            }
            SemanticError::UseOfUndeclared => "You dey use variable wey I no sabi",
            SemanticError::TypeMismatch => "You dey try use wrong type for operation",
            SemanticError::InvalidStringOperation => "You no fit do dis operation for string",
        }
    }
}

/// Represents the type of a variable in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum VarType {
    Number,
    String,
    Bool,
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

    // This is our symbol table! For now, it's just a flat Vec that tracks all variables
    // declared so far, along with their type and where they were declared (the Span).
    // We use &'src str for variable names to avoid extra allocations, since these
    // string slices come straight from the source code. If we ever add block scoping,
    // we can turn this into a stack of Vecs or something fancier.
    symbol_table: Vec<(&'src str, VarType, &'src Span)>,

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
                // Find if variable already declared, and get its span if so
                if let Some((_, _, orig_span)) =
                    self.symbol_table.iter().find(|(name, _, _)| name == var)
                {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::DuplicateDeclaration.as_str(),
                        vec![
                            Label {
                                span: (*orig_span).clone(),
                                message: "First time you declare am here",
                            },
                            Label {
                                span: span.clone(),
                                message: "You try declare am again for here",
                            },
                        ],
                    );
                } else {
                    // Let's figure out the type of this variable from the expression,
                    // then add it to our symbol table so future code knows it's declared.
                    let typ = self.infer_expr_type(*expr).unwrap_or(VarType::Number); // fallback to Number if unknown
                    self.symbol_table.push((var, typ, span));
                }
                // Always check the expression, even if variable was duplicate
                // This catches more errors in one pass
                self.check_expr(*expr);
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, expr, span } => {
                if !self.symbol_table.iter().any(|(name, _, _)| name == var) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::AssignmentToUndeclared.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: "This variable never dey before",
                        }],
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
        let lhs_type = self.infer_expr_type(cond.lhs);
        let rhs_type = self.infer_expr_type(cond.rhs);
        match (lhs_type, rhs_type) {
            (Some(VarType::String), Some(VarType::String))
            | (Some(VarType::Number), Some(VarType::Number))
            | (Some(VarType::Bool), Some(VarType::Bool)) => {}
            (Some(VarType::String), Some(VarType::Number))
            | (Some(VarType::Number), Some(VarType::String))
            | (Some(VarType::Bool), Some(VarType::Number))
            | (Some(VarType::Number), Some(VarType::Bool))
            | (Some(VarType::Bool), Some(VarType::String))
            | (Some(VarType::String), Some(VarType::Bool)) => {
                self.errors.emit(
                    cond.span.clone(),
                    Severity::Error,
                    "semantic",
                    SemanticError::TypeMismatch.as_str(),
                    vec![Label {
                        span: cond.span.clone(),
                        message: "You no fit compare different types together",
                    }],
                );
            }
            _ => {}
        }
    }

    /// Recursively validates expressions - the core of our semantic checking.
    /// This is where we catch the most common programming error: using variables
    /// before declaring them. Numbers are always valid, but variables need to
    /// exist in our symbol table.
    fn check_expr(&mut self, eid: ExprId) {
        match &self.exprs.nodes[eid.0] {
            // Numbers like 42 or 3.14 are always semantically valid
            Expr::Number(_, _) | Expr::String(_, _) | Expr::Bool(_, _) => {}
            // Variables must have been declared with "make" before use
            Expr::Var(v, span) => {
                if !self.symbol_table.iter().any(|(name, _, _)| name == v) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::UseOfUndeclared.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: "This variable never dey before",
                        }],
                    );
                }
            }
            // Binary operations like "add", "minus", "times", "divide"
            // Both operands must be valid expressions (recursive validation)
            Expr::Binary { op, lhs, rhs, span } => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                let l = self.infer_expr_type(*lhs);
                let r = self.infer_expr_type(*rhs);
                match op {
                    BinOp::Add => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number))
                        | (Some(VarType::String), Some(VarType::String)) => {}
                        (Some(VarType::String), Some(VarType::Number))
                        | (Some(VarType::Number), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: "You no fit add string and number together",
                                }],
                            );
                        }
                        (Some(VarType::Bool), _) | (_, Some(VarType::Bool)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: "You no fit do arithmetic with boolean values",
                                }],
                            );
                        }
                        _ => {}
                    },
                    BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Remain => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number)) => {}
                        (Some(VarType::String), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::InvalidStringOperation.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: "You no fit minus/times/divide/remain string for here",
                                }],
                            );
                        }
                        (Some(VarType::String), Some(VarType::Number))
                        | (Some(VarType::Number), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: "You no fit do arithmetic with string and number together",
                                }],
                            );
                        }
                        (Some(VarType::Bool), _) | (_, Some(VarType::Bool)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: "You no fit do arithmetic with boolean values",
                                }],
                            );
                        }
                        _ => {}
                    },
                }
            }
        }
    }

    /// Infer the type of an expression, using the symbol table for variables
    fn infer_expr_type(&self, eid: ExprId) -> Option<VarType> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(_, _) => Some(VarType::Number),
            Expr::String(_, _) => Some(VarType::String),
            Expr::Bool(_, _) => Some(VarType::Bool),
            Expr::Var(v, _) => {
                self.symbol_table.iter().find(|(name, _, _)| name == v).map(|(_, t, _)| *t)
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                let l = self.infer_expr_type(*lhs)?;
                let r = self.infer_expr_type(*rhs)?;
                match op {
                    BinOp::Add | BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Remain => {
                        if l == VarType::Number && r == VarType::Number {
                            Some(VarType::Number)
                        } else if l == VarType::String
                            && r == VarType::String
                            && matches!(op, BinOp::Add)
                        {
                            Some(VarType::String)
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
}
