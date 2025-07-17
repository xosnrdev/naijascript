//! The syntax parser for NaijaScript.

use std::borrow::Cow;

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::token::{SpannedToken, Token};

/// A simple arena allocator for AST nodes.
#[derive(Default)]
pub struct Arena<T> {
    pub nodes: Vec<T>,
}

impl<T> Arena<T> {
    #[inline]
    const fn new() -> Self {
        Arena { nodes: Vec::new() }
    }

    #[inline]
    fn alloc(&mut self, node: T) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        id
    }
}

/// A unique identifier for AST nodes.
#[derive(Copy, Clone, Debug)]
pub struct NodeId(pub usize);

pub type StmtId = NodeId;
pub type ExprId = NodeId;
pub type BlockId = NodeId;
pub type ParamListId = NodeId;
pub type ArgListId = NodeId;

/// Represents a list of function parameters
#[derive(Debug)]
pub struct ParamList<'src> {
    pub params: Vec<&'src str>,
}

/// Represents a list of function call arguments
#[derive(Debug)]
pub struct ArgList {
    pub args: Vec<ExprId>,
}

/// Binary operators for arithmetic and logical expressions.
#[derive(Debug)]
pub enum BinOp {
    Add,    // "add" keyword
    Minus,  // "minus" keyword
    Times,  // "times" keyword
    Divide, // "divide" keyword
    Mod,    // "mod" keyword
    And,    // "and" keyword (logical)
    Or,     // "or" keyword (logical)
    Eq,     // "na" keyword (equals)
    Gt,     // "pass" keyword (greater than)
    Lt,     // "small pass" keyword (less than)
}

/// Represents a statement in NaijaScript.
#[derive(Debug)]
pub enum Stmt<'src> {
    // "make x get 5"
    Assign { var: &'src str, expr: ExprId, span: Span },
    // "x get 5"
    AssignExisting { var: &'src str, expr: ExprId, span: Span },
    // "if to say(...) start...end"
    If { cond: ExprId, then_b: BlockId, else_b: Option<BlockId>, span: Span },
    // "jasi(...) start...end"
    Loop { cond: ExprId, body: BlockId, span: Span },
    // "start...end" - standalone or nested block
    Block { block: BlockId, span: Span },
    // Function definition: do <name>(<params>?) start ... end
    FunctionDef { name: &'src str, params: ParamListId, body: BlockId, span: Span },
    // Return statement: return <expr>?
    Return { expr: Option<ExprId>, span: Span },
    // Expression statement (e.g., function calls)
    Expression { expr: ExprId, span: Span },
}

/// Represents an expression in NaijaScript.
#[derive(Debug)]
pub enum Expr<'src> {
    Number(&'src str, Span),      // 42, 3.14, etc.
    String(Cow<'src, str>, Span), // "hello", etc.
    Bool(bool, Span),             // "true", "false"
    Var(&'src str, Span),         // variable references
    // arithmetic/logical operations
    Binary { op: BinOp, lhs: ExprId, rhs: ExprId, span: Span },
    // logical not
    Not { expr: ExprId, span: Span },
    // Function call: <callee>(<args>?)
    Call { callee: ExprId, args: ArgListId, span: Span },
}

/// Represents a block of statements, which can be nested.
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
    pub span: Span,
}

/// Represents syntax errors that can occur during parsing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxError {
    ExpectedStatement,
    ExpectedIdentifier,
    ExpectedGetAfterIdentifier,
    ExpectedLParen,
    ExpectedRParen,
    ExpectedStartBlock,
    UnterminatedBlock,
    ExpectedComparisonOperator,
    ExpectedNumberOrVariableOrLParen,
    TrailingTokensAfterProgramEnd,
    ReservedKeywordAsIdentifier,
}

impl AsStr for SyntaxError {
    fn as_str(&self) -> &'static str {
        match self {
            SyntaxError::ExpectedStatement => "Missing statement",
            SyntaxError::ExpectedIdentifier => "Missing identifier",
            SyntaxError::ExpectedGetAfterIdentifier => "Missing `get` after identifier",
            SyntaxError::ExpectedLParen => "Missing left parenthesis",
            SyntaxError::ExpectedRParen => "Missing right parenthesis",
            SyntaxError::ExpectedStartBlock => "Missing start block",
            SyntaxError::UnterminatedBlock => "Missing end block",
            SyntaxError::ExpectedComparisonOperator => "Missing comparison operator",
            SyntaxError::ExpectedNumberOrVariableOrLParen => {
                "Missing number, variable, or left parenthesis"
            }
            SyntaxError::TrailingTokensAfterProgramEnd => "Unexpected trailing tokens",
            SyntaxError::ReservedKeywordAsIdentifier => "Use of reserved keyword as identifier",
        }
    }
}

/// The interface for the NaijaScript parser.
pub struct Parser<'src, I: Iterator<Item = SpannedToken<'src>>> {
    tokens: I,
    cur: SpannedToken<'src>,
    errors: Diagnostics,

    pub stmt_arena: Arena<Stmt<'src>>,
    pub expr_arena: Arena<Expr<'src>>,
    pub block_arena: Arena<Block>,
    pub param_arena: Arena<ParamList<'src>>,
    pub arg_arena: Arena<ArgList>,
}

impl<'src, I: Iterator<Item = SpannedToken<'src>>> Parser<'src, I> {
    /// Create a new parser instance with the given token iterator.
    #[inline]
    pub fn new(mut tokens: I) -> Self {
        let first = tokens.next().unwrap_or_default();
        Parser {
            tokens,
            cur: first,
            errors: Diagnostics::default(),
            stmt_arena: Arena::new(),
            expr_arena: Arena::new(),
            block_arena: Arena::new(),
            param_arena: Arena::new(),
            arg_arena: Arena::new(),
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

    /// The main entry point for parsing a complete NaijaScript program.
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
    // We look for statement keywords (make, if to say, jasi, do, return) or block boundaries.
    // Without this, one syntax error would make the rest of the file unparseable.
    #[inline]
    fn synchronize(&mut self) {
        while !matches!(
            self.cur.token,
            Token::EOF
                | Token::Make
                | Token::IfToSay
                | Token::Jasi
                | Token::Do
                | Token::Return
                | Token::End
        ) {
            self.bump();
        }
    }

    // Dispatches to the appropriate statement parser based on the current token.
    // This directly implements: <statement> ::= <assignment> | <if_statement> | <loop_statement> | <function_def> | <return_statement> | <expression_statement> | <block>
    // Returns None on error to trigger error recovery in the caller.
    #[inline]
    fn parse_statement(&mut self) -> Option<StmtId> {
        let start = self.cur.span.start;
        match &self.cur.token {
            Token::Do => self.parse_function_def(start),
            Token::Return => self.parse_return(start),
            Token::Make => self.parse_assignment(start),
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
                    // Function calls need special handling because they look like variable references initially
                    // We already consumed the identifier, so we peek ahead to see if parentheses follow
                    // This disambiguates between "foo()" (valid expression statement) and "foo" (invalid bare reference)
                    if let Token::LParen = self.cur.token {
                        // Build function call expression starting with the variable we already parsed
                        // We reuse parse_expression_continuation to handle the call syntax and any chained operations
                        let var_expr = self.expr_arena.alloc(Expr::Var(var_name, var_span.clone()));
                        let expr = self.parse_expression_continuation(var_expr, 0);
                        let sid = self
                            .stmt_arena
                            .alloc(Stmt::Expression { expr, span: start..self.cur.span.end });
                        Some(sid)
                    } else {
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
            }
            _ => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedStatement.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("I dey expect `do`, `return`, `make`, `if to say`, `jasi`, function call, or variable reassignment for here"),
                    }],
                );
                None
            }
        }
    }

    // Parses function definitions: do <name>(<params>?) start ... end
    #[inline]
    fn parse_function_def(&mut self, start: usize) -> Option<StmtId> {
        let do_span = self.cur.span.clone();
        self.bump(); // consume 'do'
        // Parse function name
        let name = match &self.cur.token {
            Token::Identifier(n) => *n,
            t if t.is_reserved_keyword() => {
                self.errors.emit(
                    self.cur.span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ReservedKeywordAsIdentifier.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Owned(format!(
                            "You no fit use reserved word `{t}` as function name"
                        )),
                    }],
                );
                return None;
            }
            _ => {
                self.errors.emit(
                    do_span.clone(),
                    Severity::Error,
                    "syntax",
                    SyntaxError::ExpectedIdentifier.as_str(),
                    vec![Label {
                        span: self.cur.span.clone(),
                        message: Cow::Borrowed("Put function name after `do` for here"),
                    }],
                );
                return None;
            }
        };
        self.bump();
        // Parse parameter list
        if let Token::LParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedLParen.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after function name for here"),
                }],
            );
            return None;
        }
        let mut params = Vec::new();
        loop {
            match &self.cur.token {
                Token::Identifier(p) => {
                    if Token::is_reserved_keyword(&Token::Identifier(p)) {
                        self.errors.emit(
                            self.cur.span.clone(),
                            Severity::Error,
                            "syntax",
                            SyntaxError::ReservedKeywordAsIdentifier.as_str(),
                            vec![Label {
                                span: self.cur.span.clone(),
                                message: Cow::Owned(format!(
                                    "You no fit use reserved word `{p}` as parameter name"
                                )),
                            }],
                        );
                        return None;
                    }
                    params.push(*p);
                    self.bump();
                    if let Token::Comma = self.cur.token {
                        self.bump();
                    } else {
                        break;
                    }
                }
                t if t.is_reserved_keyword() => {
                    self.errors.emit(
                        self.cur.span.clone(),
                        Severity::Error,
                        "syntax",
                        SyntaxError::ReservedKeywordAsIdentifier.as_str(),
                        vec![Label {
                            span: self.cur.span.clone(),
                            message: Cow::Owned(format!(
                                "You no fit use reserved word `{t}` as parameter name"
                            )),
                        }],
                    );
                    return None;
                }
                _ => break,
            }
        }
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParen.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Close parameter list with `)` for here"),
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
                SyntaxError::ExpectedStartBlock.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Begin function body with `start` for here"),
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
                SyntaxError::UnterminatedBlock.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Dis function body start here, but I no see `end`"),
                }],
            );
        }
        let params = self.param_arena.alloc(ParamList { params });
        let sid = self.stmt_arena.alloc(Stmt::FunctionDef {
            name,
            params,
            body,
            span: start..self.cur.span.end,
        });
        Some(sid)
    }

    // Parses return statements: return <expr>?
    #[inline]
    fn parse_return(&mut self, start: usize) -> Option<StmtId> {
        self.bump(); // consume 'return'
        let expr = match &self.cur.token {
            Token::Number(_)
            | Token::String(_)
            | Token::True
            | Token::False
            | Token::Identifier(_)
            | Token::Not
            | Token::LParen => Some(self.parse_expression(0)),
            Token::End | Token::EOF => None,
            _ => Some(self.parse_expression(0)),
        };
        let sid = self.stmt_arena.alloc(Stmt::Return { expr, span: start..self.cur.span.end });
        Some(sid)
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
                    SyntaxError::ExpectedIdentifier.as_str(),
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
                SyntaxError::ExpectedLParen.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after `if to say` for here"),
                }],
            );
            return None;
        }
        let cond = self.parse_expression(0);
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParen.as_str(),
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
                SyntaxError::ExpectedStartBlock.as_str(),
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
                SyntaxError::UnterminatedBlock.as_str(),
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
                    SyntaxError::ExpectedStartBlock.as_str(),
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
                    SyntaxError::UnterminatedBlock.as_str(),
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
    // "jasi" is the Nigerian Pidgin keyword for while loops.
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
                SyntaxError::ExpectedLParen.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Put `(` after `jasi` for here"),
                }],
            );
            return None;
        }
        let cond = self.parse_expression(0);
        if let Token::RParen = self.cur.token {
            self.bump();
        } else {
            self.errors.emit(
                self.cur.span.clone(),
                Severity::Error,
                "syntax",
                SyntaxError::ExpectedRParen.as_str(),
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
                SyntaxError::ExpectedStartBlock.as_str(),
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
                SyntaxError::UnterminatedBlock.as_str(),
                vec![Label {
                    span: self.cur.span.clone(),
                    message: Cow::Borrowed("Dis loop body start here, but I no see `end`"),
                }],
            );
        }
        let sid = self.stmt_arena.alloc(Stmt::Loop { cond, body, span: start..self.cur.span.end });
        Some(sid)
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
        let lhs = match &self.cur.token {
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

        self.parse_expression_continuation(lhs, min_bp)
    }

    // Continue parsing when we already have a left expression, handling operator precedence and function calls
    // The lhs parameter lets us resume parsing from any expression node, which is essential for handling
    // cases like "foo()" where we first parse "foo" as a variable, then discover it's actually a function call
    // This pattern also enables clean handling of operator chaining like "a + b * c" where Pratt parsing
    // builds the tree with correct precedence in a single forward pass
    #[inline]
    fn parse_expression_continuation(&mut self, mut lhs: ExprId, min_bp: u8) -> ExprId {
        let start = match &self.expr_arena.nodes[lhs.0] {
            Expr::Number(_, span) => span.start,
            Expr::String(_, span) => span.start,
            Expr::Bool(_, span) => span.start,
            Expr::Var(_, span) => span.start,
            Expr::Binary { span, .. } => span.start,
            Expr::Not { span, .. } => span.start,
            Expr::Call { span, .. } => span.start,
        };

        // Pratt parselet for function calls and binary operators
        loop {
            // Function call: <expr>(<args>?)
            if let Token::LParen = self.cur.token {
                let call_start = start;
                self.bump(); // consume '('
                let mut args = Vec::new();
                if !matches!(self.cur.token, Token::RParen) {
                    loop {
                        let arg = self.parse_expression(0);
                        args.push(arg);
                        if let Token::Comma = self.cur.token {
                            self.bump();
                            if matches!(self.cur.token, Token::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                if let Token::RParen = self.cur.token {
                    self.bump();
                } else {
                    self.errors.emit(
                        self.cur.span.clone(),
                        Severity::Error,
                        "syntax",
                        SyntaxError::ExpectedRParen.as_str(),
                        vec![Label {
                            span: self.cur.span.clone(),
                            message: Cow::Borrowed("Close argument list with `)` for here"),
                        }],
                    );
                }
                let args = self.arg_arena.alloc(ArgList { args });
                lhs = self.expr_arena.alloc(Expr::Call {
                    callee: lhs,
                    args,
                    span: call_start..self.cur.span.end,
                });
                continue;
            }
            let (op, l_bp, r_bp) = match &self.cur.token {
                Token::Times => (BinOp::Times, 20, 21),
                Token::Divide => (BinOp::Divide, 20, 21),
                Token::Mod => (BinOp::Mod, 20, 21),
                Token::Add => (BinOp::Add, 10, 11),
                Token::Minus => (BinOp::Minus, 10, 11),
                Token::Na => (BinOp::Eq, 7, 8),
                Token::Pass => (BinOp::Gt, 7, 8),
                Token::SmallPass => (BinOp::Lt, 7, 8),
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
                | Token::IfToSay
                | Token::Jasi
                | Token::Do
                | Token::Return
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
