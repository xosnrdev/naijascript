//! The syntax parser for NaijaScript.

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
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
    Assign { var: &'src str, expr: ExprId, span: Span }, // "make x get 5"
    AssignExisting { var: &'src str, expr: ExprId, span: Span }, // "x get 5"
    Shout { expr: ExprId, span: Span },                  // "shout(x)"
    If { cond: CondId, then_b: BlockId, else_b: Option<BlockId>, span: Span }, // "if to say(...) start...end"
    Loop { cond: CondId, body: BlockId, span: Span }, // "jasi(...) start...end"
}

/// Expression AST nodes for NaijaScript.
/// Numbers and string literals are stored as string slices to preserve original formatting.
#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str, Span),                                    // 42, 3.14, etc.
    String(&'src str, Span),                                    // "hello", etc.
    Var(&'src str, Span),                                       // variable references
    Binary { op: BinOp, lhs: ExprId, rhs: ExprId, span: Span }, // arithmetic operations
}

/// Represents a condition in if statements or loops.
/// Always a binary comparison between two expressions.
/// The grammar only supports three comparison types right now.
#[derive(Debug)]
pub struct Cond {
    pub op: CmpOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
    pub span: Span,
}

/// A block is just a collection of statements wrapped in "start"/"end".
/// This keeps the AST structure simple while matching the grammar exactly.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
    pub span: Span,
}

/// Represents the type of syntax errors that can occur during parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxError {
    ExpectedStatement,
    ExpectedIdentifierAfterMake,
    ExpectedGetAfterIdentifier,
    ExpectedLParenAfterShout,
    ExpectedRParenAfterShout,
    ExpectedLParenAfterIf,
    ExpectedRParenAfterIf,
    ExpectedStartForThenBlock,
    UnterminatedThenBlock,
    ExpectedStartForElseBlock,
    UnterminatedElseBlock,
    ExpectedLParenAfterJasi,
    ExpectedRParenAfterJasi,
    ExpectedStartForLoopBody,
    UnterminatedLoopBody,
    ExpectedComparisonOperator,
    ExpectedNumberOrVariableOrLParen,
    TrailingTokensAfterProgramEnd,
}

impl AsStr for SyntaxError {
    fn as_str(&self) -> &'static str {
        match self {
            SyntaxError::ExpectedStatement => "Statement syntax no correct",
            SyntaxError::ExpectedIdentifierAfterMake => "Assignment syntax no complete",
            SyntaxError::ExpectedGetAfterIdentifier => "Assignment syntax no correct",
            SyntaxError::ExpectedLParenAfterShout => "Shout syntax no complete",
            SyntaxError::ExpectedRParenAfterShout => "Shout syntax no complete",
            SyntaxError::ExpectedLParenAfterIf => "If statement syntax no complete",
            SyntaxError::ExpectedRParenAfterIf => "If statement syntax no complete",
            SyntaxError::ExpectedStartForThenBlock => "If block syntax no complete",
            SyntaxError::UnterminatedThenBlock => "If block syntax no complete",
            SyntaxError::ExpectedStartForElseBlock => "Else block syntax no complete",
            SyntaxError::UnterminatedElseBlock => "Else block syntax no complete",
            SyntaxError::ExpectedLParenAfterJasi => "Loop syntax no complete",
            SyntaxError::ExpectedRParenAfterJasi => "Loop syntax no complete",
            SyntaxError::ExpectedStartForLoopBody => "Loop block syntax no complete",
            SyntaxError::UnterminatedLoopBody => "Loop block syntax no complete",
            SyntaxError::ExpectedComparisonOperator => "Condition syntax no complete",
            SyntaxError::ExpectedNumberOrVariableOrLParen => "Expression syntax no complete",
            SyntaxError::TrailingTokensAfterProgramEnd => "Program syntax no complete",
        }
    }
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
    pub lexer: Lexer<'src>,
    cur: Token<'src>,    // Current token (one token lookahead)
    errors: Diagnostics, // Collect all syntax errors, don't fail fast

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
            errors: Diagnostics::default(),
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

    /// Main entry point for parsing a complete NaijaScript program.
    /// Returns both the AST root and any syntax errors found.
    /// The grammar says: <program> ::= <statement_list>
    /// We treat the whole program as one big block of statements.
    #[inline(always)]
    pub fn parse_program(&mut self) -> (BlockId, Diagnostics) {
        let block_id = self.parse_program_body();
        if self.cur != Token::EOF {
            self.errors.emit(
                self.lexer.pos..self.lexer.pos + 1,
                Severity::Error,
                "syntax",
                SyntaxError::TrailingTokensAfterProgramEnd.as_str(),
                Vec::new(),
            );
        }
        (block_id, std::mem::take(&mut self.errors))
    }

    /// Parses a sequence of statements until we hit a block terminator.
    /// The grammar's <statement_list> is recursive, but we use a simple loop instead.
    /// When statement parsing fails, we call synchronize() to skip to the next likely
    /// statement start - this is crucial for good error recovery.
    #[inline(always)]
    fn parse_block_body(&mut self) -> BlockId {
        let start = self.lexer.pos;
        let mut stmts = Vec::new();
        while !matches!(self.cur, Token::EOF | Token::End) {
            match self.parse_statement() {
                Some(sid) => stmts.push(sid),
                None => self.synchronize(), // Skip to next statement on error
            }
        }
        let end = self.lexer.pos;
        self.block_arena.alloc(Block { stmts, span: start..end })
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
        let start = self.lexer.pos;
        match self.cur {
            Token::Make => self.parse_assignment(start),
            Token::Shout => self.parse_shout(start),
            Token::IfToSay => self.parse_if(start),
            Token::Jasi => self.parse_loop(start),
            Token::Identifier(var) => {
                // Peek ahead for reassignment: <identifier> 'get' <expression>
                self.bump();
                if self.cur == Token::Get {
                    self.bump();
                    let expr = self.parse_expression(0);
                    let end = self.lexer.pos;
                    let sid =
                        self.stmt_arena.alloc(Stmt::AssignExisting { var, expr, span: start..end });
                    Some(sid)
                } else {
                    // Not a reassignment, error and do not consume
                    let mut message = "I dey expect statement for here";
                    // Suggest a keyword if close to a known one
                    if Self::suggest_keyword(var, "make").is_some() {
                        message = "You fit mean `make`?";
                    } else if Self::suggest_keyword(var, "shout").is_some() {
                        message = "You fit mean `shout`?";
                    } else if Self::suggest_keyword(var, "jasi").is_some() {
                        message = "You fit mean `jasi`?";
                    } else if Self::suggest_keyword(var, "if").is_some() {
                        message = "You fit mean `if to say`?";
                    }
                    self.errors.emit(
                        start..self.lexer.pos,
                        Severity::Error,
                        "syntax",
                        SyntaxError::ExpectedStatement.as_str(),
                        vec![Label { span: start..self.lexer.pos, message }],
                    );
                    None
                }
            }
            _ => {
                self.errors.emit(
                    start..self.lexer.pos,
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedStatement.as_str(),
                    vec![Label {
                        span: start..self.lexer.pos,
                        message: "I dey expect `make`, `shout`, `if to say`, `jasi`, or variable reassignment for here",
                    }],
                );
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
    fn parse_assignment(&mut self, start: usize) -> Option<StmtId> {
        self.bump(); // consume 'make'
        let var = if let Token::Identifier(name) = &self.cur {
            *name
        } else {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedIdentifierAfterMake.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Put variable name after `make` for here",
                }],
            );
            return None;
        };
        self.bump();
        if self.cur != Token::Get {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedGetAfterIdentifier.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Put `get` after variable name for here",
                }],
            );
            return None;
        }
        self.bump();
        let expr = self.parse_expression(0);
        let end = self.lexer.pos;
        let sid = self.stmt_arena.alloc(Stmt::Assign { var, expr, span: start..end });
        Some(sid)
    }

    /// Parses "shout(expression)" output statements.
    /// Grammar: <output_statement> ::= "shout" "(" <expression> ")"
    /// Straightforward function-call syntax - the parentheses are mandatory.
    #[inline(always)]
    fn parse_shout(&mut self, start: usize) -> Option<StmtId> {
        self.bump(); // consume 'shout'
        if self.cur != Token::LParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterShout.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Put `(` after `shout` for here",
                }],
            );
            return None;
        }
        self.bump();
        let expr = self.parse_expression(0);
        if self.cur != Token::RParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterShout.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Close shout with `)` for here",
                }],
            );
            return None;
        }
        self.bump();
        let end = self.lexer.pos;
        Some(self.stmt_arena.alloc(Stmt::Shout { expr, span: start..end }))
    }

    /// Parses if statements with optional else blocks.
    /// Grammar: <if_statement> ::= "if to say" "(" <condition> ")" <block> ("if not so" <block>)?
    /// This is the most complex statement type - lots of moving parts to coordinate.
    /// The error recovery here tries to parse as much as possible even if parts fail.
    #[inline(always)]
    fn parse_if(&mut self, start: usize) -> Option<StmtId> {
        self.bump(); // consume 'if to say'
        if self.cur != Token::LParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterIf.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Put `(` after `if to say` for here",
                }],
            );
            return None;
        }
        self.bump();
        let cond = self.parse_condition();
        if self.cur != Token::RParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterIf.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Close condition with `)` for here",
                }],
            );
            return None;
        }
        self.bump();

        // Parse the mandatory then-block
        if self.cur != Token::Start {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedStartForThenBlock.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Begin block with `start` for here",
                }],
            );
            return None;
        }
        self.bump();
        let then_b = self.parse_block_body();
        if self.cur == Token::End {
            self.bump();
        } else {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::UnterminatedThenBlock.as_str(),
                vec![Label {
                    span: start..start + 5, // highlight 'start'
                    message: "Dis block start here, but I no see `end`",
                }],
            );
        }

        // Parse optional else block - "if not so" is our else keyword
        let else_b = if self.cur == Token::IfNotSo {
            self.bump();
            if self.cur != Token::Start {
                self.errors.emit(
                    start..self.lexer.pos,
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedStartForElseBlock.as_str(),
                    vec![Label {
                        span: self.lexer.pos..self.lexer.pos + 1,
                        message: "Begin else block with `start` for here",
                    }],
                );
                return None;
            }
            self.bump();
            let b = self.parse_block_body();
            if self.cur == Token::End {
                self.bump();
            } else {
                self.errors.emit(
                    start..self.lexer.pos,
                    Severity::Error,
                    "syntax",
                    SyntaxError::UnterminatedElseBlock.as_str(),
                    vec![Label {
                        span: start..start + 5,
                        message: "Dis else block start here, but I no see `end`",
                    }],
                );
            }
            Some(b)
        } else {
            None
        };
        let end = self.lexer.pos;
        let sid = self.stmt_arena.alloc(Stmt::If { cond, then_b, else_b, span: start..end });
        Some(sid)
    }

    /// Parses loop statements - simpler than if statements since there's no else clause.
    /// Grammar: <loop_statement> ::= "jasi" "(" <condition> ")" <block>
    /// "jasi" appears to be Nigerian Pidgin for some kind of loop construct.
    #[inline(always)]
    fn parse_loop(&mut self, start: usize) -> Option<StmtId> {
        self.bump(); // consume 'jasi'
        if self.cur != Token::LParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterJasi.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Put `(` after `jasi` for here",
                }],
            );
            return None;
        }
        self.bump();
        let cond = self.parse_condition();
        if self.cur != Token::RParen {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterJasi.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Close loop condition with `)` for here",
                }],
            );
            return None;
        }
        self.bump();
        if self.cur != Token::Start {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedStartForLoopBody.as_str(),
                vec![Label {
                    span: self.lexer.pos..self.lexer.pos + 1,
                    message: "Begin loop body with `start` for here",
                }],
            );
            return None;
        }
        self.bump();
        let body = self.parse_block_body();
        if self.cur == Token::End {
            self.bump();
        } else {
            self.errors.emit(
                start..self.lexer.pos,
                Severity::Error,
                "syntax",
                SyntaxError::UnterminatedLoopBody.as_str(),
                vec![Label {
                    span: start..start + 5,
                    message: "Dis loop body start here, but I no see `end`",
                }],
            );
        }
        let end = self.lexer.pos;
        let sid = self.stmt_arena.alloc(Stmt::Loop { cond, body, span: start..end });
        Some(sid)
    }

    /// Parses condition expressions for if statements and loops.
    /// Grammar: <condition> ::= <expression> "na" <expression> | <expression> "pass" <expression> | <expression> "small pass" <expression>
    /// Always a binary comparison - no compound conditions yet.
    #[inline(always)]
    fn parse_condition(&mut self) -> CondId {
        let start = self.lexer.pos;
        let lhs = self.parse_expression(0);
        let op = match self.cur {
            Token::Na => CmpOp::Eq,        // "na" means equals
            Token::Pass => CmpOp::Gt,      // "pass" means greater than
            Token::SmallPass => CmpOp::Lt, // "small pass" means less than
            _ => {
                self.errors.emit(
                    start..self.lexer.pos,
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedComparisonOperator.as_str(),
                    vec![Label {
                        span: self.lexer.pos..self.lexer.pos + 1,
                        message: "Use `na`, `pass`, or `small pass` to compare for here",
                    }],
                );
                CmpOp::Eq // fallback to something reasonable
            }
        };
        self.bump();
        let rhs = self.parse_expression(0);
        let end = self.lexer.pos;
        self.cond_arena.alloc(Cond { op, lhs, rhs, span: start..end })
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
        let start = self.lexer.pos;
        // Parse the left-hand side (primary expression)
        let mut lhs = match &self.cur {
            Token::Number(n) => {
                let s = start..self.lexer.pos + n.len();
                let id = self.expr_arena.alloc(Expr::Number(n, s));
                self.bump();
                id
            }
            Token::String(sval) => {
                let s = start..self.lexer.pos + sval.len() + 2; // +2 for quotes
                let id = self.expr_arena.alloc(Expr::String(sval, s));
                self.bump();
                id
            }
            Token::Identifier(v) => {
                let s = start..self.lexer.pos + v.len();
                let id = self.expr_arena.alloc(Expr::Var(v, s));
                self.bump();
                id
            }
            Token::LParen => {
                self.bump();
                let expr = self.parse_expression(0); // Reset precedence inside parentheses
                if self.cur != Token::RParen {
                    self.errors.emit(
                        start..self.lexer.pos,
                        Severity::Error,
                        "syntax",
                        SyntaxError::ExpectedRParenAfterShout.as_str(),
                        vec![Label {
                            span: self.lexer.pos..self.lexer.pos + 1,
                            message: "Close expression with `)` for here",
                        }],
                    );
                } else {
                    self.bump();
                }
                expr
            }
            _ => {
                self.errors.emit(
                    start..self.lexer.pos,
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedNumberOrVariableOrLParen.as_str(),
                    vec![Label {
                        span: self.lexer.pos..self.lexer.pos + 1,
                        message: "Use number, variable name, string, or `(` for expression for here",
                    }],
                );
                let s = start..self.lexer.pos;
                // Error recovery: create a dummy number and continue
                let id = self.expr_arena.alloc(Expr::Number("0", s));
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
            let end = self.lexer.pos;
            lhs = self.expr_arena.alloc(Expr::Binary { op, lhs, rhs, span: start..end });
        }
        lhs
    }

    /// Suggests a statement keyword if the identifier is a likely typo, using a single-edit heuristic.
    #[inline(always)]
    fn suggest_keyword<'a>(ident: &str, expected: &'a str) -> Option<&'a str> {
        // Only check for single-char edit distance or prefix (very cheap, no alloc)
        if ident.len() == expected.len() {
            // One substitution: e.g., 'mak' vs 'make'
            let mut diff = 0;
            for (a, b) in ident.bytes().zip(expected.bytes()) {
                if a != b {
                    diff += 1;
                    if diff > 1 {
                        break;
                    }
                }
            }
            if diff == 1 {
                return Some(expected);
            }
        } else if ident.len() + 1 == expected.len() && expected.starts_with(ident) {
            // Missing last char: e.g., 'mak' vs 'make'
            return Some(expected);
        } else if ident.len() == expected.len() + 1 && ident.starts_with(expected) {
            // Extra last char: e.g., 'makee' vs 'make'
            return Some(expected);
        }
        None
    }

    /// Parses a program-level statement list that stops at invalid tokens.
    /// Unlike parse_block_body, this stops at any token that's not a valid statement starter.
    /// This implements: <program> ::= <statement_list> where statement_list ends at non-statements.
    #[inline(always)]
    fn parse_program_body(&mut self) -> BlockId {
        let start = self.lexer.pos;
        let mut stmts = Vec::new();

        // Keep parsing statements until we hit EOF or an invalid statement token
        while matches!(
            self.cur,
            Token::Make | Token::Shout | Token::IfToSay | Token::Jasi | Token::Identifier(_)
        ) {
            match self.parse_statement() {
                Some(sid) => stmts.push(sid),
                None => self.synchronize(), // Skip to next statement on error
            }
        }
        let end = self.lexer.pos;
        self.block_arena.alloc(Block { stmts, span: start..end })
    }
}
