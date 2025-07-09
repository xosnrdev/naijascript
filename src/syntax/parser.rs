//! The syntax parser for NaijaScript.

use std::borrow::Cow;

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::token::{SpannedToken, Token};

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
    // Creates a fresh arena
    #[inline]
    const fn new() -> Self {
        Arena { nodes: Vec::new() }
    }

    // Stores a node and returns its ID for later reference
    #[inline]
    fn alloc(&mut self, node: T) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
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
    Mod,    // "mod" keyword
    And,    // "and" keyword (logical)
    Or,     // "or" keyword (logical)
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
    Block { block: BlockId, span: Span },             // "start...end" - standalone or nested block
}

/// Expression AST nodes for NaijaScript.
/// Numbers and string literals are stored as string slices to preserve original formatting.
#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str, Span),                                    // 42, 3.14, etc.
    String(Cow<'src, str>, Span),                               // "hello", etc.
    Bool(bool, Span),                                           // "true", "false"
    Var(&'src str, Span),                                       // variable references
    Binary { op: BinOp, lhs: ExprId, rhs: ExprId, span: Span }, // arithmetic/logical operations
    Not { expr: ExprId, span: Span },                           // logical not
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
    UnterminatedBlock,
    ExpectedStartForElseBlock,
    UnterminatedElseBlock,
    ExpectedLParenAfterJasi,
    ExpectedRParenAfterJasi,
    ExpectedStartForLoopBody,
    UnterminatedLoopBody,
    ExpectedComparisonOperator,
    ExpectedNumberOrVariableOrLParen,
    TrailingTokensAfterProgramEnd,
    ReservedKeywordAsIdentifier,
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
            SyntaxError::UnterminatedBlock => "Block syntax no complete",
            SyntaxError::ExpectedStartForElseBlock => "Else block syntax no complete",
            SyntaxError::UnterminatedElseBlock => "Else block syntax no complete",
            SyntaxError::ExpectedLParenAfterJasi => "Loop syntax no complete",
            SyntaxError::ExpectedRParenAfterJasi => "Loop syntax no complete",
            SyntaxError::ExpectedStartForLoopBody => "Loop block syntax no complete",
            SyntaxError::UnterminatedLoopBody => "Loop block syntax no complete",
            SyntaxError::ExpectedComparisonOperator => "Condition syntax no complete",
            SyntaxError::ExpectedNumberOrVariableOrLParen => "Expression syntax no complete",
            SyntaxError::TrailingTokensAfterProgramEnd => "Program syntax no complete",
            SyntaxError::ReservedKeywordAsIdentifier => "Use of reserved keyword as identifier",
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
pub struct Parser<'src, I: Iterator<Item = SpannedToken<'src>>> {
    tokens: I,
    cur: SpannedToken<'src>, // Current spanned token (one token lookahead)
    errors: Diagnostics,     // Collect all syntax errors, don't fail fast

    // Separate arenas for each AST node type
    pub stmt_arena: Arena<Stmt<'src>>,
    pub expr_arena: Arena<Expr<'src>>,
    pub cond_arena: Arena<Cond>,
    pub block_arena: Arena<Block>,
}

impl<'src, I: Iterator<Item = SpannedToken<'src>>> Parser<'src, I> {
    /// Creates a new parser, primed and ready with the first token.
    ///
    /// This function takes an iterator of tokens, grabs the first one (so parsing can start immediately),
    /// and sets up all the arenas and error tracking you'll need. It's the standard entry point for
    /// turning a stream of tokens into a parser instance. If the token stream is empty, we just use a default token.
    #[inline]
    pub fn new(mut tokens: I) -> Self {
        let first = tokens.next().unwrap_or_default();
        Parser {
            tokens,
            cur: first,
            errors: Diagnostics::default(),
            stmt_arena: Arena::new(),
            expr_arena: Arena::new(),
            cond_arena: Arena::new(),
            block_arena: Arena::new(),
        }
    }

    // Move forward to the next token in our input stream.
    #[inline]
    fn bump(&mut self) {
        self.cur = self.tokens.next().unwrap_or(SpannedToken {
            token: Token::EOF,
            span: self.cur.span.end..self.cur.span.end,
        });
    }

    /// Main entry point for parsing a complete NaijaScript program.
    /// Returns both the AST root and any syntax errors found.
    /// The grammar says: <program> ::= <statement_list>
    /// We treat the whole program as one big block of statements.
    #[inline]
    pub fn parse_program(&mut self) -> (BlockId, Diagnostics) {
        let block_id = self.parse_program_body();
        if self.cur.token != Token::EOF {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::TrailingTokensAfterProgramEnd.as_str(),
                Vec::new(),
            );
        }
        (block_id, std::mem::take(&mut self.errors))
    }

    // Parses a sequence of statements until we hit a block terminator.
    // The grammar's <statement_list> is recursive, but we use a simple loop instead.
    // When statement parsing fails, we call synchronize() to skip to the next likely
    // statement start - this is crucial for good error recovery.
    #[inline]
    fn parse_block_body(&mut self) -> BlockId {
        let start = self.cur.span.start;
        let mut stmts = Vec::new();
        while !matches!(self.cur.token, Token::EOF | Token::End) {
            match self.parse_statement() {
                Some(sid) => stmts.push(sid),
                None => self.synchronize(), // Skip to next statement on error
            }
        }
        self.block_arena.alloc(Block { stmts, span: start..self.cur.span.end })
    }

    // Error recovery mechanism - skips tokens until we find a likely statement start.
    // This is essential for parsing multiple statements when earlier ones have errors.
    // We look for statement keywords (make, shout, if to say, jasi) or block boundaries.
    // Without this, one syntax error would make the rest of the file unparseable.
    #[inline]
    fn synchronize(&mut self) {
        while !matches!(
            self.cur.token,
            Token::EOF | Token::Make | Token::Shout | Token::IfToSay | Token::Jasi | Token::End
        ) {
            self.bump();
        }
    }

    // Dispatches to the appropriate statement parser based on the current token.
    // This directly implements: <statement> ::= <assignment> | <output_statement> | <if_statement> | <loop_statement>
    // Returns None on error to trigger error recovery in the caller.
    #[inline]
    fn parse_statement(&mut self) -> Option<StmtId> {
        let start = self.cur.span.start;
        match &self.cur.token {
            Token::Make => self.parse_assignment(start),
            Token::Shout => self.parse_shout(start),
            Token::IfToSay => self.parse_if(start),
            Token::Jasi => self.parse_loop(start),
            Token::Start => {
                // Parse a standalone or nested block as a statement
                self.bump(); // consume 'start'
                let block_id = self.parse_block_body();
                if let Token::End = self.cur.token {
                    self.bump();
                } else {
                    self.errors.emit(
                        self.cur.span.clone(),
                        Severity::Error,
                        "syntax",
                        SyntaxError::UnterminatedBlock.as_str(),
                        vec![Label {
                            span: self.cur.span.clone(),
                            message: Cow::Borrowed("Dis block start here, but I no see `end`"),
                        }],
                    );
                }
                let sid = self
                    .stmt_arena
                    .alloc(Stmt::Block { block: block_id, span: start..self.cur.span.end });
                Some(sid)
            }
            Token::Identifier(var) => {
                let var_name = *var;
                let var_span = self.cur.span.clone();
                // Peek ahead for reassignment: <identifier> 'get' <expression>
                self.bump();
                if let Token::Get = self.cur.token {
                    self.bump();
                    let expr = self.parse_expression(0);
                    let sid = self.stmt_arena.alloc(Stmt::AssignExisting {
                        var: var_name,
                        expr,
                        span: start..self.cur.span.end,
                    });
                    Some(sid)
                } else {
                    // Not a reassignment, error and do not consume
                    let message: Cow<'static, str> =
                        if let Some(suggestion) = Token::suggest_keyword(var_name) {
                            Cow::Owned(format!(
                                "I dey expect statement for here. Na `{suggestion}` you mean?",
                            ))
                        } else {
                            Cow::Borrowed("I dey expect statement for here")
                        };
                    self.errors.emit(
                        var_span.clone(),
                        Severity::Error,
                        "syntax",
                        SyntaxError::ExpectedStatement.as_str(),
                        vec![Label { span: var_span, message }],
                    );
                    None
                }
            }
            _ => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedStatement.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("I dey expect `make`, `shout`, `if to say`, `jasi`, or variable reassignment for here"),
                    }],
                );
                None
            }
        }
    }

    // Parses "make variable get expression" assignments.
    // Grammar: <assignment> ::= "make" <variable> "get" <expression>
    // This is the most common statement type, so we want it to be rock-solid.
    // Note: we parse the expression even if the variable part failed - this helps
    // with error recovery and might catch additional errors in the expression.
    #[inline]
    fn parse_assignment(&mut self, start: usize) -> Option<StmtId> {
        let make_span = self.cur.span.clone();
        self.bump(); // consume 'make'
        let (var, var_span) = match &self.cur.token {
            Token::Identifier(name) => (*name, self.cur.span.clone()),
            t if t.is_reserved_keyword() => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ReservedKeywordAsIdentifier.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Owned(format!(
                            "You no fit use reserved word `{t}` as identifier"
                        )),
                    }],
                );
                return None;
            }
            _ => {
                self.errors.emit(
                    make_span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedIdentifierAfterMake.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("Put variable name after `make` for here"),
                    }],
                );
                return None;
            }
        };
        self.bump();
        if let Token::Get = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                var_span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedGetAfterIdentifier.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `get` after variable name for here"),
                }],
            );
            return None;
        }
        let expr = self.parse_expression(0);
        let sid = self.stmt_arena.alloc(Stmt::Assign { var, expr, span: start..self.cur.span.end });
        Some(sid)
    }

    // Parses "shout(expression)" output statements.
    // Grammar: <output_statement> ::= "shout" "(" <expression> ")"
    // Straightforward function-call syntax - the parentheses are mandatory.
    #[inline]
    fn parse_shout(&mut self, start: usize) -> Option<StmtId> {
        let shout_span = self.cur.span.clone();
        self.bump(); // consume 'shout'
        if let Token::LParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                shout_span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterShout.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after `shout` for here"),
                }],
            );
            return None;
        }
        let expr = self.parse_expression(0);
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterShout.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Close shout with `)` for here"),
                }],
            );
            return None;
        }
        Some(self.stmt_arena.alloc(Stmt::Shout { expr, span: start..self.cur.span.end }))
    }

    // Parses if statements with optional else blocks.
    // Grammar: <if_statement> ::= "if to say" "(" <condition> ")" <block> ("if not so" <block>)?
    // This is the most complex statement type - lots of moving parts to coordinate.
    // The error recovery here tries to parse as much as possible even if parts fail.
    #[inline]
    fn parse_if(&mut self, start: usize) -> Option<StmtId> {
        let if_span = self.cur.span.clone();
        self.bump(); // consume 'if to say'
        if let Token::LParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                if_span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterIf.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after `if to say` for here"),
                }],
            );
            return None;
        }
        let cond = self.parse_condition();
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterIf.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Close condition with `)` for here"),
                }],
            );
            return None;
        }

        // Parser the mandatory then-block
        if let Token::Start = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedStartForThenBlock.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Begin block with `start` for here"),
                }],
            );
            return None;
        }
        let then_b = self.parse_block_body();
        if let Token::End = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::UnterminatedThenBlock.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Dis block start here, but I no see `end`"),
                }],
            );
        }

        // Parse optional else block - "if not so" is our else keyword
        let else_b = if let Token::IfNotSo = self.cur.token {
            let else_span = self.cur.span.clone();
            self.bump();
            if let Token::Start = self.cur.token {
                self.bump();
            } else {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedStartForElseBlock.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("Begin else block with `start` for here"),
                    }],
                );
                return None;
            }
            let b = self.parse_block_body();
            if let Token::End = self.cur.token {
                self.bump();
            } else {
                self.errors.emit(
                    else_span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::UnterminatedElseBlock.as_str(),
                    vec![Label {
                        span: else_span,
                        message: Cow::Borrowed("Dis else block start here, but I no see `end`"),
                    }],
                );
            }
            Some(b)
        } else {
            None
        };
        let sid = self.stmt_arena.alloc(Stmt::If {
            cond,
            then_b,
            else_b,
            span: start..self.cur.span.end,
        });
        Some(sid)
    }

    // Parses loop statements - simpler than if statements since there's no else clause.
    // Grammar: <loop_statement> ::= "jasi" "(" <condition> ")" <block>
    // "jasi" appears to be Nigerian Pidgin for some kind of loop construct.
    #[inline]
    fn parse_loop(&mut self, start: usize) -> Option<StmtId> {
        let loop_span = self.cur.span.clone();
        self.bump(); // consume 'jasi'
        if let Token::LParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                loop_span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParenAfterJasi.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after `jasi` for here"),
                }],
            );
            return None;
        }
        let cond = self.parse_condition();
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParenAfterJasi.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Close loop condition with `)` for here"),
                }],
            );
            return None;
        }
        if let Token::Start = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedStartForLoopBody.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Begin loop body with `start` for here"),
                }],
            );
            return None;
        }
        let body = self.parse_block_body();
        if let Token::End = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::UnterminatedLoopBody.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Dis loop body start here, but I no see `end`"),
                }],
            );
        }
        let sid = self.stmt_arena.alloc(Stmt::Loop { cond, body, span: start..self.cur.span.end });
        Some(sid)
    }

    // Parses condition expressions for if statements and loops.
    // Grammar: <condition> ::= <expression> "na" <expression> | <expression> "pass" <expression> | <expression> "small pass" <expression>
    // Always a binary comparison - no compound conditions yet.
    #[inline]
    fn parse_condition(&mut self) -> CondId {
        let start = self.cur.span.start;
        let lhs = self.parse_expression(0);
        let op = match &self.cur.token {
            Token::Na => CmpOp::Eq,        // "na" means equals
            Token::Pass => CmpOp::Gt,      // "pass" means greater than
            Token::SmallPass => CmpOp::Lt, // "small pass" means less than
            _ => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedComparisonOperator.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed(
                            "Use `na`, `pass`, or `small pass` to compare for here",
                        ),
                    }],
                );
                CmpOp::Eq // fallback to something reasonable
            }
        };
        self.bump();
        let rhs = self.parse_expression(0);
        self.cond_arena.alloc(Cond { op, lhs, rhs, span: start..self.cur.span.end })
    }

    // Expression parsing using Pratt parsing technique for operator precedence.
    // This elegantly handles the grammar rules:
    // <expression> ::= <logic_term> | <expression> "or" <logic_term> | <expression> "add" <term> | <expression> "minus" <term>
    // <logic_term> ::= <logic_factor> | <logic_term> "and" <logic_factor> | <term>
    // <logic_factor> ::= "not" <factor> | <factor>
    // <term> ::= <factor> | <term> "times" <factor> | <term> "divide" <factor> | <term> "mod" <factor>
    //
    // The min_bp parameter controls precedence - higher numbers bind tighter.
    // This avoids the traditional approach of separate methods for each precedence level.
    // Inspiration: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    #[inline]
    fn parse_expression(&mut self, min_bp: u8) -> ExprId {
        let start = self.cur.span.start;
        // Parse the left-hand side (primary expression)
        let mut lhs = match &self.cur.token {
            Token::Number(n) => {
                let s = self.cur.span.clone();
                let id = self.expr_arena.alloc(Expr::Number(n, s));
                self.bump();
                id
            }
            Token::String(sval) => {
                let s = self.cur.span.clone();
                let id = self.expr_arena.alloc(Expr::String(sval.clone(), s));
                self.bump();
                id
            }
            Token::True | Token::False => {
                let s = self.cur.span.clone();
                let value = matches!(&self.cur.token, Token::True);
                let id = self.expr_arena.alloc(Expr::Bool(value, s));
                self.bump();
                id
            }
            Token::Identifier(v) => {
                let s = self.cur.span.clone();
                let id = self.expr_arena.alloc(Expr::Var(v, s));
                self.bump();
                id
            }
            Token::Not => {
                self.bump();
                let expr = self.parse_expression(30); // Highest precedence for unary not
                self.expr_arena.alloc(Expr::Not { expr, span: start..self.cur.span.end })
            }
            Token::LParen => {
                self.bump();
                let expr = self.parse_expression(0); // Reset precedence inside parentheses
                if let Token::RParen = self.cur.token {
                    self.bump();
                } else {
                    self.errors.emit(
                        self.cur.span.clone(),
                        Severity::Error,
                        "syntax",
                        SyntaxError::ExpectedNumberOrVariableOrLParen.as_str(),
                        vec![Label {
                            span: self.cur.span.clone(),
                            message: Cow::Borrowed("Close expression with `)` for here"),
                        }],
                    );
                }
                expr
            }
            _ => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedNumberOrVariableOrLParen.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("I dey expect number, variable, or `(` for here"),
                    }],
                );
                let s = self.cur.span.clone();
                let id = self.expr_arena.alloc(Expr::Number("0", s));
                self.bump();
                id
            }
        };

        // Parse binary operators with precedence climbing
        loop {
            let (op, l_bp, r_bp) = match &self.cur.token {
                Token::Times => (BinOp::Times, 20, 21),
                Token::Divide => (BinOp::Divide, 20, 21),
                Token::Mod => (BinOp::Mod, 20, 21),
                Token::Add => (BinOp::Add, 10, 11),
                Token::Minus => (BinOp::Minus, 10, 11),
                Token::And => (BinOp::And, 5, 6),
                Token::Or => (BinOp::Or, 1, 2),
                _ => break, // No more operators
            };

            // If current operator has lower precedence than minimum, we're done
            if l_bp < min_bp {
                break;
            }
            self.bump(); // consume the operator
            let rhs = self.parse_expression(r_bp); // Parse right side with higher precedence
            lhs = self.expr_arena.alloc(Expr::Binary {
                op,
                lhs,
                rhs,
                span: start..self.cur.span.end,
            });
        }
        lhs
    }

    // Parses a program-level statement list that stops at invalid tokens.
    // Unlike parse_block_body, this stops at any token that's not a valid statement starter.
    // This implements: <program> ::= <statement_list> where statement_list ends at non-statements.
    #[inline]
    fn parse_program_body(&mut self) -> BlockId {
        let start = self.cur.span.start;
        let mut stmts = Vec::new();

        // Keep parsing statements until we hit EOF or an invalid statement token
        while matches!(
            &self.cur.token,
            Token::Make
                | Token::Shout
                | Token::IfToSay
                | Token::Jasi
                | Token::Identifier(_)
                | Token::Start
        ) {
            match self.parse_statement() {
                Some(sid) => stmts.push(sid),
                None => self.synchronize(), // Skip to next statement on error
            }
        }
        self.block_arena.alloc(Block { stmts, span: start..self.cur.span.end })
    }
}
