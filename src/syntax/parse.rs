//! The syntax parser for NaijaScript.

use crate::syntax::scanner::{Lexer, Token};

/// Arena allocator for AST nodes - our answer to memory management without garbage collection.
///
/// Traditional tree structures with Box<> pointers work fine, but arenas give us several
/// advantages: better cache locality (all nodes of same type are together), easier
/// serialization (just indices), and deterministic cleanup. Plus, no risk of cycles
/// or dangling pointers that can plague tree structures.
#[derive(Default)]
pub struct Arena<T> {
    pub nodes: Vec<T>,
}

impl<T> Arena<T> {
    /// Creates a fresh arena - nothing fancy here, just an empty Vec
    #[inline(always)]
    pub const fn new() -> Self {
        Arena { nodes: Vec::new() }
    }

    /// Stores a node and returns its ID for later reference.
    /// The inline(always) here is important - this gets called constantly during parsing
    /// and we want the optimizer to eliminate the function call overhead.
    #[inline(always)]
    pub fn alloc(&mut self, node: T) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }

    /// Retrieves a node by its ID - essentially just array indexing with a wrapper
    #[inline(always)]
    pub fn get(&self, id: NodeId) -> &T {
        &self.nodes[id.0]
    }
}

/// Wrapper around usize that prevents mixing up different node types.
/// Without this, it's too easy to accidentally use a statement ID where we need
/// an expression ID. The type system catches these mistakes at compile time.
#[derive(Copy, Clone, Debug)]
pub struct NodeId(pub usize);

// Type aliases make the code more self-documenting
// These all use the same underlying NodeId but different types prevent confusion
pub type StmtId = NodeId;
pub type ExprId = NodeId;
pub type CondId = NodeId;
pub type BlockId = NodeId;

/// Binary operators in NaijaScript expressions.
/// Maps directly to the arithmetic operators in our BNF grammar.
/// The names are more conventional than the Nigerian Pidgin keywords for easier code reading.
#[derive(Debug)]
pub enum BinOp {
    Add,    // "add" keyword
    Minus,  // "minus" keyword
    Times,  // "times" keyword
    Divide, // "divide" keyword
}

/// Comparison operators for conditions in if statements and loops.
#[derive(Debug)]
pub enum CmpOp {
    Eq, // "na" - equals comparison
    Gt, // "pass" - greater than
    Lt, // "small pass" - less than
}

/// All the different statement types NaijaScript supports.
/// This mirrors the grammar's statement alternatives exactly.
/// Using string slices from the source avoids unnecessary allocations.
#[derive(Debug)]
pub enum Stmt<'src> {
    Assign { var: &'src str, expr: ExprId }, // "make x get 5"
    AssignExisting { var: &'src str, expr: ExprId }, // "x get 5"
    Shout { expr: ExprId },                  // "shout(x)"
    If { cond: CondId, then_b: BlockId, else_b: Option<BlockId> }, // "if to say(...) start...end"
    Loop { cond: CondId, body: BlockId },    // "jasi(...) start...end"
}

/// Expression AST nodes - the building blocks of NaijaScript arithmetic.
/// We store numbers as string slices to preserve the original formatting
/// (useful for error messages and potential future features like hex literals).
#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str),                              // 42, 3.14, etc.
    Var(&'src str),                                 // variable references
    Binary { op: BinOp, lhs: ExprId, rhs: ExprId }, // arithmetic operations
}

/// Represents a condition in if statements or loops.
/// Always a binary comparison between two expressions.
/// The grammar only supports three comparison types right now.
#[derive(Debug)]
pub struct Cond {
    pub op: CmpOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

/// A block is just a collection of statements wrapped in "start"/"end".
/// This keeps the AST structure simple while matching the grammar exactly.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

/// Syntax error information for helpful error messages.
/// We track position in the source and use static strings for messages
/// to avoid allocation overhead during error reporting.
#[derive(Debug)]
pub struct SyntaxError {
    pub pos: usize,
    pub message: &'static str,
}

/// The heart of NaijaScript parsing - converts tokens into AST nodes.
///
/// This is a recursive descent parser with single token lookahead. Simple but effective
/// for our grammar. The key insight is that we use separate arenas for different node
/// types, which gives us type safety without runtime overhead.
///
/// Error recovery strategy: when we hit a syntax error, we try to synchronize to the
/// next statement boundary and continue parsing. This gives users multiple error
/// messages in one parse, which is much more helpful than stopping at the first error.
pub struct Parser<'src> {
    lexer: Lexer<'src>,
    cur: Token<'src>,         // Current token (one token lookahead)
    errors: Vec<SyntaxError>, // Collect all syntax errors, don't fail fast

    // Separate arenas for each AST node type - type safety without runtime cost
    pub stmt_arena: Arena<Stmt<'src>>,
    pub expr_arena: Arena<Expr<'src>>,
    pub cond_arena: Arena<Cond>,
    pub block_arena: Arena<Block>,
}

impl<'src> Parser<'src> {
    /// Sets up a new parser with the first token ready to go.
    /// The lexer is consumed immediately to get the first token - this simplifies
    /// the parsing logic since we always have a current token to examine.
    #[inline(always)]
    pub fn new(src: &'src str) -> Self {
        let mut lex = Lexer::new(src);
        let first = lex.next_token();
        Parser {
            lexer: lex,
            cur: first,
            errors: Vec::new(),
            stmt_arena: Arena::new(),
            expr_arena: Arena::new(),
            cond_arena: Arena::new(),
            block_arena: Arena::new(),
        }
    }

    /// Advances to the next token - the fundamental operation of parsing.
    /// Inlined because this gets called constantly and we want zero overhead.
    #[inline(always)]
    fn bump(&mut self) {
        self.cur = self.lexer.next_token();
    }

    /// Records a syntax error without stopping parsing.
    /// This "keep going" approach lets us find multiple errors in one pass,
    /// which gives users a much better experience than fixing one error at a time.
    #[inline(always)]
    fn error(&mut self, msg: &'static str) {
        let pos = self.lexer.pos;
        self.errors.push(SyntaxError { pos, message: msg });
    }

    /// Main entry point for parsing a complete NaijaScript program.
    /// Returns both the AST root and any syntax errors found.
    /// The grammar says: <program> ::= <statement_list>
    /// We treat the whole program as one big block of statements.
    #[inline(always)]
    pub fn parse_program(&mut self) -> (BlockId, Vec<SyntaxError>) {
        let block_id = self.parse_block_body();
        if self.cur != Token::EOF {
            self.error("trailing tokens after program end");
        }
        (block_id, std::mem::take(&mut self.errors))
    }

    /// Parses a sequence of statements until we hit a block terminator.
    /// The grammar's <statement_list> is recursive, but we use a simple loop instead.
    /// When statement parsing fails, we call synchronize() to skip to the next likely
    /// statement start - this is crucial for good error recovery.
    #[inline(always)]
    fn parse_block_body(&mut self) -> BlockId {
        let mut stmts = Vec::new();
        while !matches!(self.cur, Token::EOF | Token::End) {
            match self.parse_statement() {
                Some(sid) => stmts.push(sid),
                None => self.synchronize(), // Skip to next statement on error
            }
        }
        self.block_arena.alloc(Block { stmts })
    }

    /// Error recovery mechanism - skips tokens until we find a likely statement start.
    /// This is essential for parsing multiple statements when earlier ones have errors.
    /// We look for statement keywords (make, shout, if to say, jasi) or block boundaries.
    /// Without this, one syntax error would make the rest of the file unparseable.
    #[inline(always)]
    fn synchronize(&mut self) {
        while !matches!(
            self.cur,
            Token::EOF | Token::Make | Token::Shout | Token::IfToSay | Token::Jasi | Token::End
        ) {
            self.bump();
        }
    }

    /// Dispatches to the appropriate statement parser based on the current token.
    /// This directly implements: <statement> ::= <assignment> | <output_statement> | <if_statement> | <loop_statement>
    /// Returns None on error to trigger error recovery in the caller.
    #[inline(always)]
    fn parse_statement(&mut self) -> Option<StmtId> {
        match self.cur {
            Token::Make => self.parse_assignment(),
            Token::Shout => self.parse_shout(),
            Token::IfToSay => self.parse_if(),
            Token::Jasi => self.parse_loop(),
            Token::Identifier(var) => {
                // Peek ahead for reassignment: <identifier> 'get' <expression>
                self.bump();
                if self.cur == Token::Get {
                    self.bump();
                    let expr = self.parse_expression(0);
                    let sid = self.stmt_arena.alloc(Stmt::AssignExisting { var, expr });
                    Some(sid)
                } else {
                    // Not a reassignment, error and do not consume
                    self.error("expected statement (did you mean to use 'make' or 'get'?)");
                    None
                }
            }
            _ => {
                self.error("expected statement");
                None
            }
        }
    }

    /// Parses "make variable get expression" assignments.
    /// Grammar: <assignment> ::= "make" <variable> "get" <expression>
    /// This is the most common statement type, so we want it to be rock-solid.
    /// Note: we parse the expression even if the variable part failed - this helps
    /// with error recovery and might catch additional errors in the expression.
    #[inline(always)]
    fn parse_assignment(&mut self) -> Option<StmtId> {
        self.bump(); // consume 'make'
        let var = if let Token::Identifier(name) = &self.cur {
            *name
        } else {
            self.error("expected identifier after `make`");
            return None;
        };
        self.bump();
        if self.cur != Token::Get {
            self.error("expected `get` after identifier");
            return None;
        }
        self.bump();
        let expr = self.parse_expression(0);
        let sid = self.stmt_arena.alloc(Stmt::Assign { var, expr });
        Some(sid)
    }

    /// Parses "shout(expression)" output statements.
    /// Grammar: <output_statement> ::= "shout" "(" <expression> ")"
    /// Straightforward function-call syntax - the parentheses are mandatory.
    #[inline(always)]
    fn parse_shout(&mut self) -> Option<StmtId> {
        self.bump(); // consume 'shout'
        if self.cur != Token::LParen {
            self.error("expected `(` after `shout`");
            return None;
        }
        self.bump();
        let expr = self.parse_expression(0);
        if self.cur != Token::RParen {
            self.error("expected `)` after shout expression");
            return None;
        }
        self.bump();
        Some(self.stmt_arena.alloc(Stmt::Shout { expr }))
    }

    /// Parses if statements with optional else blocks.
    /// Grammar: <if_statement> ::= "if to say" "(" <condition> ")" <block> ("if not so" <block>)?
    /// This is the most complex statement type - lots of moving parts to coordinate.
    /// The error recovery here tries to parse as much as possible even if parts fail.
    #[inline(always)]
    fn parse_if(&mut self) -> Option<StmtId> {
        self.bump(); // consume 'if to say'
        if self.cur != Token::LParen {
            self.error("expected `(` after `if to say`");
            return None;
        }
        self.bump();
        let cond = self.parse_condition();
        if self.cur != Token::RParen {
            self.error("expected `)` after if condition");
            return None;
        }
        self.bump();

        // Parse the mandatory then-block
        if self.cur != Token::Start {
            self.error("expected `start` for then-block");
            return None;
        }
        self.bump();
        let then_b = self.parse_block_body();
        if self.cur == Token::End {
            self.bump();
        } else {
            self.error("unterminated then-block (missing `end`)");
        }

        // Parse optional else block - "if not so" is our else keyword
        let else_b = if self.cur == Token::IfNotSo {
            self.bump();
            if self.cur != Token::Start {
                self.error("expected `start` for else-block");
                return None;
            }
            self.bump();
            let b = self.parse_block_body();
            if self.cur == Token::End {
                self.bump();
            } else {
                self.error("unterminated else-block (missing `end`)");
            }
            Some(b)
        } else {
            None
        };
        let sid = self.stmt_arena.alloc(Stmt::If { cond, then_b, else_b });
        Some(sid)
    }

    /// Parses loop statements - simpler than if statements since there's no else clause.
    /// Grammar: <loop_statement> ::= "jasi" "(" <condition> ")" <block>
    /// "jasi" appears to be Nigerian Pidgin for some kind of loop construct.
    #[inline(always)]
    fn parse_loop(&mut self) -> Option<StmtId> {
        self.bump(); // consume 'jasi'
        if self.cur != Token::LParen {
            self.error("expected `(` after `jasi`");
            return None;
        }
        self.bump();
        let cond = self.parse_condition();
        if self.cur != Token::RParen {
            self.error("expected `)` after loop condition");
            return None;
        }
        self.bump();
        if self.cur != Token::Start {
            self.error("expected `start` for loop body");
            return None;
        }
        self.bump();
        let body = self.parse_block_body();
        if self.cur == Token::End {
            self.bump();
        } else {
            self.error("unterminated loop body (missing `end`)");
        }
        let sid = self.stmt_arena.alloc(Stmt::Loop { cond, body });
        Some(sid)
    }

    /// Parses condition expressions for if statements and loops.
    /// Grammar: <condition> ::= <expression> "na" <expression> | <expression> "pass" <expression> | <expression> "small pass" <expression>
    /// Always a binary comparison - no compound conditions yet.
    #[inline(always)]
    fn parse_condition(&mut self) -> CondId {
        let lhs = self.parse_expression(0);
        let op = match self.cur {
            Token::Na => CmpOp::Eq,        // "na" means equals
            Token::Pass => CmpOp::Gt,      // "pass" means greater than
            Token::SmallPass => CmpOp::Lt, // "small pass" means less than
            _ => {
                self.error("expected comparison operator (`na`/`pass`/`small pass`)");
                CmpOp::Eq // fallback to something reasonable
            }
        };
        self.bump();
        let rhs = self.parse_expression(0);
        self.cond_arena.alloc(Cond { op, lhs, rhs })
    }

    /// Expression parsing using Pratt parsing technique for operator precedence.
    /// This elegantly handles the grammar rules:
    /// <expression> ::= <term> | <expression> "add" <term> | <expression> "minus" <term>
    /// <term> ::= <factor> | <term> "times" <factor> | <term> "divide" <factor>
    ///
    /// The min_bp parameter controls precedence - higher numbers bind tighter.
    /// This avoids the traditional approach of separate methods for each precedence level.
    /// Inspiration: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    #[inline(always)]
    fn parse_expression(&mut self, min_bp: u8) -> ExprId {
        // Parse the left-hand side (primary expression)
        let mut lhs = match &self.cur {
            Token::Number(n) => {
                let id = self.expr_arena.alloc(Expr::Number(n));
                self.bump();
                id
            }
            Token::Identifier(v) => {
                let id = self.expr_arena.alloc(Expr::Var(v));
                self.bump();
                id
            }
            Token::LParen => {
                self.bump();
                let expr = self.parse_expression(0); // Reset precedence inside parentheses
                if self.cur != Token::RParen {
                    self.error("expected `)`");
                } else {
                    self.bump();
                }
                expr
            }
            _ => {
                self.error("expected number, variable, or `(`");
                // Error recovery: create a dummy number and continue
                let id = self.expr_arena.alloc(Expr::Number("0"));
                self.bump();
                id
            }
        };

        // Parse binary operators with precedence climbing
        loop {
            let op = match self.cur {
                Token::Times => BinOp::Times,
                Token::Divide => BinOp::Divide,
                Token::Add => BinOp::Add,
                Token::Minus => BinOp::Minus,
                _ => break, // No more operators
            };

            // Set precedence levels - multiplication/division bind tighter than addition/subtraction
            let (l_bp, r_bp) = match op {
                BinOp::Times | BinOp::Divide => (20, 21), // Higher precedence
                BinOp::Add | BinOp::Minus => (10, 11),    // Lower precedence
            };

            // If current operator has lower precedence than minimum, we're done
            if l_bp < min_bp {
                break;
            }

            self.bump(); // consume the operator
            let rhs = self.parse_expression(r_bp); // Parse right side with higher precedence
            lhs = self.expr_arena.alloc(Expr::Binary { op, lhs, rhs });
        }

        lhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_assignment() {
        let src = "make x get 42";
        let mut parser = Parser::new(src);
        let (_block_id, errors) = parser.parse_program();
        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_parse_shout() {
        let src = "shout(1 add 2)";
        let mut parser = Parser::new(src);
        let (_block_id, errors) = parser.parse_program();
        assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    }

    #[test]
    fn test_parse_invalid() {
        let src = "make get 5";
        let mut parser = Parser::new(src);
        let (_block_id, errors) = parser.parse_program();
        assert!(!errors.is_empty(), "Expected errors for invalid syntax");
    }
}
