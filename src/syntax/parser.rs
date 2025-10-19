//! The syntax parser for NaijaScript.

use std::range::Range;
use std::{mem, slice};

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::arena_format;
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::simd::{memchr, memchr2};
use crate::syntax::token::{SpannedToken, Token};

// Direct references to AST nodes allocated in the arena.
pub type StmtRef<'ast> = &'ast Stmt<'ast>;
pub type ExprRef<'ast> = &'ast Expr<'ast>;
pub type BlockRef<'ast> = &'ast Block<'ast>;
pub type ParamListRef<'ast> = &'ast ParamList<'ast>;
pub type ArgListRef<'ast> = &'ast ArgList<'ast>;

/// Binary operators for arithmetic and logical expressions.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
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

/// Unary operators for logical and arithmetic negation.
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,   // "not" keyword (logical negation)
    Minus, // "-" for negation
}

/// Represents a list of function parameters
#[derive(Debug)]
pub struct ParamList<'ast> {
    pub params: &'ast [&'ast str],
    pub param_spans: &'ast [Span],
}

/// Represents a list of function call arguments
#[derive(Debug)]
pub struct ArgList<'ast> {
    pub args: &'ast [ExprRef<'ast>],
}

/// Represents the parts of a string literal.
#[derive(Debug)]
pub enum StringParts<'ast> {
    Static(&'ast str),
    Interpolated(&'ast [StringSegment<'ast>]),
}

/// Represents a segment of a string literal.
#[derive(Debug, Clone, Copy)]
pub enum StringSegment<'ast> {
    Literal(&'ast str),
    Variable(&'ast str),
}

/// Represents a statement in NaijaScript.
#[derive(Debug)]
pub enum Stmt<'ast> {
    // Function definition: do <name>(<params>?) start ... end
    FunctionDef {
        name: &'ast str,
        name_span: Span,
        params: ParamListRef<'ast>,
        body: BlockRef<'ast>,
        span: Span,
    },
    // "make x get 5"
    Assign {
        var: &'ast str,
        var_span: Span,
        expr: ExprRef<'ast>,
        span: Span,
    },
    // "x get 5"
    AssignExisting {
        var: &'ast str,
        var_span: Span,
        expr: ExprRef<'ast>,
        span: Span,
    },
    // "arr[0] get 5"
    AssignIndex {
        target: ExprRef<'ast>,
        expr: ExprRef<'ast>,
        span: Span,
    },
    // "if to say(...) start...end"
    If {
        cond: ExprRef<'ast>,
        then_b: BlockRef<'ast>,
        else_b: Option<BlockRef<'ast>>,
        span: Span,
    },
    // "jasi(...) start...end"
    Loop {
        cond: ExprRef<'ast>,
        body: BlockRef<'ast>,
        span: Span,
    },
    // "start...end" - standalone or nested block
    Block {
        block: BlockRef<'ast>,
        span: Span,
    },
    // Return statement: return <expr>?
    Return {
        expr: Option<ExprRef<'ast>>,
        span: Span,
    },
    // Expression statement (e.g., function calls)
    Expression {
        expr: ExprRef<'ast>,
        span: Span,
    },
}

/// Represents an expression in NaijaScript.
#[derive(Debug)]
pub enum Expr<'ast> {
    // Array indexing: expr[expr]
    Index { array: ExprRef<'ast>, index: ExprRef<'ast>, index_span: Span, span: Span },
    String { parts: StringParts<'ast>, span: Span }, // "hello" or "hello {name}"
    Number(&'ast str, Span),                         // 42, 3.14, etc.
    Var(&'ast str, Span),                            // variable references
    // arithmetic/logical operations
    Binary { op: BinaryOp, lhs: ExprRef<'ast>, rhs: ExprRef<'ast>, span: Span },
    // Function call: <callee>(<args>?)
    Call { callee: ExprRef<'ast>, args: ArgListRef<'ast>, span: Span },
    // Array literal: [expr, expr, ...]
    Array { elements: &'ast [ExprRef<'ast>], span: Span },
    // logical/arithmetic negation
    Unary { op: UnaryOp, expr: ExprRef<'ast>, span: Span },
    Bool(bool, Span), // "true", "false"
    // Member access: expr.field
    Member { object: ExprRef<'ast>, field: &'ast str, field_span: Span, span: Span },
}

/// Represents a block of statements, which can be nested.
#[derive(Debug)]
pub struct Block<'ast> {
    pub stmts: &'ast [StmtRef<'ast>],
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
    ExpectedRBracket,
    ExpectedStartBlock,
    UnterminatedBlock,
    ExpectedComparisonOperator,
    ExpectedNumberOrVariableOrLParen,
    TrailingTokensAfterProgramEnd,
    ReservedKeyword,
    InvalidAssignmentTarget,
}

impl AsStr for SyntaxError {
    fn as_str(&self) -> &'static str {
        match self {
            SyntaxError::ExpectedStatement => "Missing statement",
            SyntaxError::ExpectedIdentifier => "Missing identifier",
            SyntaxError::ExpectedGetAfterIdentifier => "Missing `get` after identifier",
            SyntaxError::ExpectedLParen => "Missing left parenthesis",
            SyntaxError::ExpectedRParen => "Missing right parenthesis",
            SyntaxError::ExpectedRBracket => "Missing right bracket",
            SyntaxError::ExpectedStartBlock => "Missing start block",
            SyntaxError::UnterminatedBlock => "Missing end block",
            SyntaxError::ExpectedComparisonOperator => "Missing comparison operator",
            SyntaxError::ExpectedNumberOrVariableOrLParen => {
                "Missing number, variable, or left parenthesis"
            }
            SyntaxError::TrailingTokensAfterProgramEnd => "Unexpected token",
            SyntaxError::ReservedKeyword => "Use of reserved keyword",
            SyntaxError::InvalidAssignmentTarget => "Invalid assignment target",
        }
    }
}

/// An arena-backed recursive-descent parser.
///
/// Think of it like reading a book: you go sentence by sentence (recursive descent),
/// but instead of throwing away each page after reading, you keep everything
/// in one big notebook [`arena::Arena`].
///
/// We use a hybrid approach, combining recursive-descent with Pratt parsing for
/// expressions. See [https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html]
pub struct Parser<'src, 'ast, I: Iterator<Item = SpannedToken<'ast, 'src>>>
where
    'src: 'ast,
{
    tokens: I,
    cur: SpannedToken<'ast, 'src>,
    errors: Diagnostics<'ast>,
    arena: &'ast Arena,
}

impl<'src: 'ast, 'ast, I: Iterator<Item = SpannedToken<'ast, 'src>>> Parser<'src, 'ast, I> {
    /// Creates a new [`Parser`] instance.
    pub fn new(mut tokens: I, arena: &'ast Arena) -> Self {
        let cur = tokens.next().unwrap_or_default();
        Self { tokens, cur, errors: Diagnostics::new(arena), arena }
    }

    #[inline]
    fn alloc<T>(&self, value: T) -> &'ast T {
        let uninit = self.arena.alloc_uninit::<T>();
        uninit.write(value)
    }

    fn alloc_str(&self, s: &str) -> &'ast str {
        let arena_string = ArenaString::from_str(self.arena, s);
        unsafe { mem::transmute(arena_string.as_str()) }
    }

    #[inline]
    fn bump(&mut self) {
        self.cur = self.tokens.next().unwrap_or(SpannedToken {
            token: Token::EOF,
            span: Range::from(self.cur.span.end..self.cur.span.end),
        });
    }

    fn emit_error(&mut self, span: Span, error: SyntaxError, labels: Vec<Label<'ast>>) {
        self.errors.emit(span, Severity::Error, "syntax", error.as_str(), labels);
    }

    /// Returns the parsed program as a Block reference.
    pub fn parse_program(&mut self) -> (BlockRef<'ast>, &Diagnostics<'ast>) {
        let block_ref = self.parse_program_body();
        if self.cur.token != Token::EOF {
            self.emit_error(self.cur.span, SyntaxError::TrailingTokensAfterProgramEnd, Vec::new());
        }
        (block_ref, &self.errors)
    }

    fn parse_program_body(&mut self) -> BlockRef<'ast> {
        let start = self.cur.span.start;
        let mut stmts = Vec::new_in(self.arena);

        while matches!(
            self.cur.token,
            Token::Make
                | Token::IfToSay
                | Token::Jasi
                | Token::Do
                | Token::Return
                | Token::Identifier(..)
                | Token::Start
        ) {
            match self.parse_statement() {
                Some(stmt) => stmts.push(stmt),
                None => self.synchronize(),
            }
        }

        let end = self.cur.span.end;
        let stmts = self.arena.vec_into_slice(stmts);
        self.alloc(Block { stmts, span: Range::from(start..end) })
    }

    #[inline]
    fn parse_block_body(&mut self) -> BlockRef<'ast> {
        let start = self.cur.span.start;
        let mut stmts = Vec::new_in(self.arena);

        while !matches!(self.cur.token, Token::EOF | Token::End) {
            match self.parse_statement() {
                Some(stmt) => stmts.push(stmt),
                None => self.synchronize(),
            }
        }

        let end = self.cur.span.end;
        let stmts = self.arena.vec_into_slice(stmts);
        self.alloc(Block { stmts, span: Range::from(start..end) })
    }

    #[inline]
    fn synchronize(&mut self) {
        while !matches!(
            self.cur.token,
            Token::EOF
                | Token::RParen
                | Token::RBracket
                | Token::Comma
                | Token::End
                | Token::Make
                | Token::IfToSay
                | Token::Jasi
                | Token::Do
                | Token::Return
        ) {
            self.bump();
        }
    }

    #[inline]
    fn parse_statement(&mut self) -> Option<StmtRef<'ast>> {
        let start = self.cur.span.start;
        match &self.cur.token {
            Token::Do => self.parse_function_def(start),
            Token::Return => self.parse_return(start),
            Token::Make => self.parse_assignment(start),
            Token::IfToSay => self.parse_if(start),
            Token::Jasi => self.parse_loop(start),
            // Parse a standalone or nested block as a statement
            Token::Start => {
                self.bump(); // consume `start` block
                let block_ref = self.parse_block_body();
                if let Token::End = self.cur.token {
                    self.bump(); // consume `end` block
                } else {
                    self.emit_error(
                        self.cur.span,
                        SyntaxError::UnterminatedBlock,
                        vec![Label {
                            span: self.cur.span,
                            message: ArenaCow::Borrowed("I dey expect `end` block"),
                        }],
                    );
                }
                let end = self.cur.span.end;
                Some(self.alloc(Stmt::Block { block: block_ref, span: Range::from(start..end) }))
            }
            Token::Identifier(var) => {
                let var = *var;
                let var_span = self.cur.span;
                self.bump(); // consume `identifier`

                let initial_expr = self.alloc(Expr::Var(var, var_span));
                let expr = self.parse_expression_continuation(initial_expr, 0);

                if let Token::Get = self.cur.token {
                    self.bump(); // consume `get`
                    let value_expr = self.parse_expression(0);
                    let end = self.cur.span.end;
                    match expr {
                        Expr::Var(name, span) => Some(self.alloc(Stmt::AssignExisting {
                            var: name,
                            var_span: *span,
                            expr: value_expr,
                            span: Range::from(start..end),
                        })),
                        Expr::Index { .. } => Some(self.alloc(Stmt::AssignIndex {
                            target: expr,
                            expr: value_expr,
                            span: Range::from(start..end),
                        })),
                        _ => {
                            self.emit_error(
                                Range::from(start..end),
                                SyntaxError::InvalidAssignmentTarget,
                                vec![Label {
                                    span: Range::from(start..end),
                                    message: ArenaCow::Borrowed(
                                        "I dey expect variable or index on left side of assignment",
                                    ),
                                }],
                            );
                            None
                        }
                    }
                } else {
                    let end = self.cur.span.end;
                    Some(self.alloc(Stmt::Expression { expr, span: Range::from(start..end) }))
                }
            }
            _ => {
                self.emit_error(
                    self.cur.span,
                    SyntaxError::ExpectedStatement,
                    vec![Label {
                        span: self.cur.span,
                        message: ArenaCow::Borrowed("I dey expect statement"),
                    }],
                );
                None
            }
        }
    }

    fn parse_function_def(&mut self, start: usize) -> Option<StmtRef<'ast>> {
        let do_span = self.cur.span;
        self.bump(); // consume `do`

        // Parse function name and capture its span
        let (name, name_span) = match &self.cur.token {
            Token::Identifier(n) => (*n, self.cur.span),
            t if t.is_reserved_keyword() => {
                let span = self.cur.span;
                self.emit_error(
                    span,
                    SyntaxError::ReservedKeyword,
                    vec![Label {
                        span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "`{t}` na reserved keyword"
                        )),
                    }],
                );
                // To always produce a complete AST, even with errors.
                // We insert placeholder/dummy values and continue parsing.
                ("_", span)
            }
            _ => {
                let span = self.cur.span;
                self.emit_error(
                    do_span,
                    SyntaxError::ExpectedIdentifier,
                    vec![Label {
                        span: do_span,
                        message: ArenaCow::Borrowed("I dey expect function name after `do`"),
                    }],
                );
                // To always produce a complete AST, even with errors.
                // We insert placeholder/dummy values and continue parsing.
                ("_", span)
            }
        };
        self.bump();

        // Parse parameter list
        let lparen_span = self.cur.span;
        if let Token::LParen = self.cur.token {
            self.bump(); // consume `(`
        } else {
            self.emit_error(
                Range::from(start..name_span.end),
                SyntaxError::ExpectedLParen,
                vec![Label {
                    span: Range::from(start..name_span.end),
                    message: ArenaCow::Borrowed("I dey expect `(` after function name"),
                }],
            );
            return None;
        }

        let mut params = Vec::new_in(self.arena);
        let mut param_spans = Vec::new_in(self.arena);
        loop {
            match &self.cur.token {
                Token::Identifier(p) => {
                    if Token::is_reserved_keyword(&Token::Identifier(p)) {
                        let span = self.cur.span;
                        self.emit_error(
                            span,
                            SyntaxError::ReservedKeyword,
                            vec![Label {
                                span,
                                message: ArenaCow::Owned(arena_format!(
                                    self.arena,
                                    "`{p}` na reserved keyword"
                                )),
                            }],
                        );
                        // To always produce a complete AST, even with errors.
                        // We insert placeholder/dummy values and continue parsing.
                        params.push("_");
                        param_spans.push(span);
                        self.bump();
                        if let Token::Comma = self.cur.token {
                            self.bump();
                        } else {
                            break;
                        }
                        continue;
                    }
                    params.push(*p);
                    param_spans.push(self.cur.span);
                    self.bump(); // consume parameter name
                    if let Token::Comma = self.cur.token {
                        self.bump(); // consume `,`
                    } else {
                        break;
                    }
                }
                t if t.is_reserved_keyword() => {
                    let span = self.cur.span;
                    self.emit_error(
                        span,
                        SyntaxError::ReservedKeyword,
                        vec![Label {
                            span,
                            message: ArenaCow::Owned(arena_format!(
                                &self.arena,
                                "`{t}` na reserved keyword"
                            )),
                        }],
                    );
                    // To always produce a complete AST, even with errors.
                    // We insert placeholder/dummy values and continue parsing.
                    params.push("_");
                    param_spans.push(span);
                    self.bump();
                    if let Token::Comma = self.cur.token {
                        self.bump();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        let rparen_span = self.cur.span;
        if let Token::RParen = self.cur.token {
            self.bump(); // consume `)`
        } else {
            let span = Range::from(start..param_spans.last().map_or(lparen_span.end, |s| s.end));
            self.emit_error(
                span,
                SyntaxError::ExpectedRParen,
                vec![Label { span, message: ArenaCow::Borrowed("I dey expect `)`") }],
            );
        }

        let start_span = self.cur.span;
        if let Token::Start = self.cur.token {
            self.bump(); // consume `start` block
        } else {
            self.emit_error(
                Range::from(start..rparen_span.end),
                SyntaxError::ExpectedStartBlock,
                vec![Label {
                    span: Range::from(start..rparen_span.end),
                    message: ArenaCow::Borrowed("I dey expect `start` block after `)`"),
                }],
            );
        }

        let body = self.parse_block_body();

        if let Token::End = self.cur.token {
            self.bump(); // consume `end` block
        } else {
            self.emit_error(
                Range::from(start..start_span.end),
                SyntaxError::UnterminatedBlock,
                vec![Label {
                    span: Range::from(start..start_span.end),
                    message: ArenaCow::Borrowed("I dey expect `end` block"),
                }],
            );
        }

        let params = self.arena.vec_into_slice(params);
        let param_spans = self.arena.vec_into_slice(param_spans);

        let params = self.alloc(ParamList { params, param_spans });
        let end = self.cur.span.end;

        Some(self.alloc(Stmt::FunctionDef {
            name,
            name_span,
            params,
            body,
            span: Range::from(start..end),
        }))
    }

    fn parse_return(&mut self, start: usize) -> Option<StmtRef<'ast>> {
        self.bump(); // consume `return`
        let expr = match &self.cur.token {
            Token::Number(..)
            | Token::String(..)
            | Token::True
            | Token::False
            | Token::Identifier(..)
            | Token::Not
            | Token::LParen => Some(self.parse_expression(0)),
            Token::End | Token::EOF => None,
            _ => Some(self.parse_expression(0)),
        };
        let end = self.cur.span.end;
        Some(self.alloc(Stmt::Return { expr, span: Range::from(start..end) }))
    }

    fn parse_assignment(&mut self, start: usize) -> Option<StmtRef<'ast>> {
        let make_span = self.cur.span;
        self.bump(); // consume `make`

        let (var, var_span) = match &self.cur.token {
            Token::Identifier(n) => (*n, self.cur.span),
            t if t.is_reserved_keyword() => {
                let span = self.cur.span;
                self.emit_error(
                    span,
                    SyntaxError::ReservedKeyword,
                    vec![Label {
                        span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "`{t}` na reserved keyword"
                        )),
                    }],
                );
                // To always produce a complete AST, even with errors.
                // We insert placeholder/dummy values and continue parsing.
                ("_", span)
            }
            _ => {
                let span = self.cur.span;
                self.emit_error(
                    make_span,
                    SyntaxError::ExpectedIdentifier,
                    vec![Label {
                        span: make_span,
                        message: ArenaCow::Borrowed("I dey expect variable name after `make`"),
                    }],
                );
                // To always produce a complete AST, even with errors.
                // We insert placeholder/dummy values and continue parsing.
                ("_", span)
            }
        };
        self.bump(); // consume variable name

        let expr = if let Token::Get = self.cur.token {
            self.bump(); // consume `get`
            self.parse_expression(0)
        } else {
            // Oh, maybe uninitialized ?
            self.alloc(Expr::Number("0", var_span))
        };

        let end = self.cur.span.end;
        Some(self.alloc(Stmt::Assign { var, var_span, expr, span: Range::from(start..end) }))
    }

    fn parse_if(&mut self, start: usize) -> Option<StmtRef<'ast>> {
        let if_span = self.cur.span;
        self.bump(); // consume `if to say`

        if let Token::LParen = self.cur.token {
            self.bump(); // consume `(`
        } else {
            self.emit_error(
                if_span,
                SyntaxError::ExpectedLParen,
                vec![Label {
                    span: if_span,
                    message: ArenaCow::Borrowed("I dey expect `(` after `if to say`"),
                }],
            );
        }

        let cond = self.parse_expression(0);
        let cond_span = Self::expr_span(cond);

        let rparen_span = self.cur.span;
        if let Token::RParen = self.cur.token {
            self.bump(); // consume `)`
        } else {
            self.emit_error(
                Range::from(start..cond_span.end),
                SyntaxError::ExpectedRParen,
                vec![Label {
                    span: Range::from(start..cond_span.end),
                    message: ArenaCow::Borrowed("I dey expect `)`"),
                }],
            );
        }

        // Parse `if to say` block
        if let Token::Start = self.cur.token {
            self.bump(); // consume `start` block
        } else {
            self.emit_error(
                Range::from(start..rparen_span.end),
                SyntaxError::ExpectedStartBlock,
                vec![Label {
                    span: Range::from(start..rparen_span.end),
                    message: ArenaCow::Borrowed("I dey expect `start` block after `)`"),
                }],
            );
        }

        let then_b = self.parse_block_body();

        // Parse `if to say` block
        if let Token::End = self.cur.token {
            self.bump(); // consume `end` block
        } else {
            self.emit_error(
                self.cur.span,
                SyntaxError::UnterminatedBlock,
                vec![Label {
                    span: self.cur.span,
                    message: ArenaCow::Borrowed("I dey expect `end` block"),
                }],
            )
        }

        // Parse `if not so` block
        let else_b = if let Token::IfNotSo = self.cur.token {
            let else_span = self.cur.span;
            self.bump(); // consume `if not so`
            let start_span = self.cur.span;
            if let Token::Start = self.cur.token {
                self.bump(); // consume `start` block
            } else {
                self.emit_error(
                    else_span,
                    SyntaxError::ExpectedStartBlock,
                    vec![Label {
                        span: else_span,
                        message: ArenaCow::Borrowed("I dey expect `start` block after `if not so`"),
                    }],
                );
            }
            let b = self.parse_block_body();
            if let Token::End = self.cur.token {
                self.bump(); // consume `end` block
            } else {
                self.emit_error(
                    Range::from(else_span.start..start_span.end),
                    SyntaxError::UnterminatedBlock,
                    vec![Label {
                        span: Range::from(else_span.start..start_span.end),
                        message: ArenaCow::Borrowed("I dey expect `end` block"),
                    }],
                );
            }
            Some(b)
        } else {
            None
        };

        let end = self.cur.span.end;
        Some(self.alloc(Stmt::If { cond, then_b, else_b, span: Range::from(start..end) }))
    }

    fn parse_loop(&mut self, start: usize) -> Option<StmtRef<'ast>> {
        let jasi_span = self.cur.span;
        self.bump(); // consume `jasi`

        if let Token::LParen = self.cur.token {
            self.bump(); // consume `(`
        } else {
            self.emit_error(
                jasi_span,
                SyntaxError::ExpectedLParen,
                vec![Label {
                    span: jasi_span,
                    message: ArenaCow::Borrowed("I dey expect `(` after `jasi`"),
                }],
            );
        }

        let cond = self.parse_expression(0);
        let cond_span = Self::expr_span(cond);

        let rparen_span = self.cur.span;
        if let Token::RParen = self.cur.token {
            self.bump(); // consume `)`
        } else {
            self.emit_error(
                Range::from(start..cond_span.end),
                SyntaxError::ExpectedRParen,
                vec![Label {
                    span: Range::from(start..cond_span.end),
                    message: ArenaCow::Borrowed("I dey expect `)`"),
                }],
            );
        }

        let start_span = self.cur.span;
        if let Token::Start = self.cur.token {
            self.bump(); // consume `start` block
        } else {
            self.emit_error(
                Range::from(start..rparen_span.end),
                SyntaxError::ExpectedStartBlock,
                vec![Label {
                    span: Range::from(start..rparen_span.end),
                    message: ArenaCow::Borrowed("I dey expect `start` block after `)`"),
                }],
            );
        }

        let body = self.parse_block_body();

        if let Token::End = self.cur.token {
            self.bump(); // consume `end` block
        } else {
            self.emit_error(
                Range::from(start..start_span.end),
                SyntaxError::UnterminatedBlock,
                vec![Label {
                    span: Range::from(start..start_span.end),
                    message: ArenaCow::Borrowed("I dey expect `end` block"),
                }],
            );
        }

        let end = self.cur.span.end;
        Some(self.alloc(Stmt::Loop { cond, body, span: Range::from(start..end) }))
    }

    #[inline]
    fn parse_expression(&mut self, min_bp: u8) -> ExprRef<'ast> {
        let start = self.cur.span.start;

        // Parse the left-hand side (primary expression)
        let lhs = match &self.cur.token {
            Token::Number(n) => {
                let s = self.cur.span;
                let expr = self.alloc(Expr::Number(n, s));
                self.bump(); // consume number
                expr
            }
            Token::String(sval) => {
                let s = self.cur.span;
                let content = sval.clone();
                self.bump(); // consume string
                self.parse_string_literal(content, s)
            }
            Token::True | Token::False => {
                let s = self.cur.span;
                let value = matches!(&self.cur.token, Token::True);
                let expr = self.alloc(Expr::Bool(value, s));
                self.bump(); // consume boolean
                expr
            }
            Token::Identifier(v) => {
                let s = self.cur.span;
                let expr = self.alloc(Expr::Var(v, s));
                self.bump(); // consume identifier
                expr
            }
            Token::Not => {
                self.bump(); // consume unary `not`
                let expr = self.parse_expression(30);
                let end = self.cur.span.end;
                self.alloc(Expr::Unary { op: UnaryOp::Not, expr, span: Range::from(start..end) })
            }
            Token::Minus => {
                self.bump(); // consume unary `minus`
                let expr = self.parse_expression(30);
                let end = self.cur.span.end;
                self.alloc(Expr::Unary { op: UnaryOp::Minus, expr, span: Range::from(start..end) })
            }
            Token::LParen => {
                self.bump(); // consume `(`
                let expr = self.parse_expression(0);
                if let Token::RParen = self.cur.token {
                    self.bump(); // consume `)`
                } else {
                    self.emit_error(
                        self.cur.span,
                        SyntaxError::ExpectedNumberOrVariableOrLParen,
                        vec![Label {
                            span: self.cur.span,
                            message: ArenaCow::Borrowed("I dey expect `)`"),
                        }],
                    )
                }
                expr
            }
            Token::LBracket => {
                self.bump(); // consume `[`
                let mut elements = Vec::new_in(self.arena);
                if !matches!(self.cur.token, Token::RBracket) {
                    loop {
                        let element = self.parse_expression(0);
                        elements.push(element);
                        if let Token::Comma = self.cur.token {
                            self.bump(); // consume ','
                            if matches!(self.cur.token, Token::RBracket) {
                                break; // skip trailing comma
                            }
                        } else {
                            break;
                        }
                    }
                }

                let end = if let Token::RBracket = self.cur.token {
                    let end = self.cur.span.end;
                    self.bump(); // consume ']'
                    end
                } else {
                    self.emit_error(
                        self.cur.span,
                        SyntaxError::ExpectedRBracket,
                        vec![Label {
                            span: self.cur.span,
                            message: ArenaCow::Borrowed("I dey expect `]`"),
                        }],
                    );
                    self.cur.span.end
                };

                let elements = self.arena.vec_into_slice(elements);
                self.alloc(Expr::Array { elements, span: Range::from(start..end) })
            }
            _ => {
                self.emit_error(
                    self.cur.span,
                    SyntaxError::ExpectedNumberOrVariableOrLParen,
                    vec![Label {
                        span: self.cur.span,
                        message: ArenaCow::Borrowed("I dey expect expression"),
                    }],
                );
                self.synchronize();
                let s = self.cur.span;
                self.alloc(Expr::Number("0", s))
            }
        };

        self.parse_expression_continuation(lhs, min_bp)
    }

    // Helper for handling binary operators and function calls
    #[inline]
    fn parse_expression_continuation(
        &mut self,
        mut lhs: ExprRef<'ast>,
        min_bp: u8,
    ) -> ExprRef<'ast> {
        let start = Self::expr_span(lhs).start;

        // Pratt parselet for function calls and binary operators
        loop {
            // Member access: <expr>.<identifier>
            if let Token::Dot = self.cur.token {
                self.bump(); // consume '.'

                let (field, field_span) = match &self.cur.token {
                    Token::Identifier(name) => (*name, self.cur.span),
                    t if t.is_reserved_keyword() => {
                        let span = self.cur.span;
                        self.emit_error(
                            span,
                            SyntaxError::ReservedKeyword,
                            vec![Label {
                                span,
                                message: ArenaCow::Owned(arena_format!(
                                    self.arena,
                                    "`{t}` na reserved keyword"
                                )),
                            }],
                        );
                        ("_", span)
                    }
                    _ => {
                        let span = self.cur.span;
                        self.emit_error(
                            span,
                            SyntaxError::ExpectedIdentifier,
                            vec![Label {
                                span,
                                message: ArenaCow::Borrowed("I dey expect identifier after `.`"),
                            }],
                        );
                        ("_", span)
                    }
                };
                self.bump(); // consume identifier

                let end = self.cur.span.end;
                lhs = self.alloc(Expr::Member {
                    object: lhs,
                    field,
                    field_span,
                    span: Range::from(start..end),
                });
                continue;
            }

            // Function call: <expr>(<args>?)
            if let Token::LParen = self.cur.token {
                let call_start = start;
                self.bump(); // consume '('
                let mut args = Vec::new_in(self.arena);
                if !matches!(self.cur.token, Token::RParen) {
                    loop {
                        let arg = self.parse_expression(0);
                        args.push(arg);
                        if let Token::Comma = self.cur.token {
                            self.bump(); // consume ','
                            if matches!(self.cur.token, Token::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                if let Token::RParen = self.cur.token {
                    self.bump(); // consume ')'
                } else {
                    self.emit_error(
                        self.cur.span,
                        SyntaxError::ExpectedRParen,
                        vec![Label {
                            span: self.cur.span,
                            message: ArenaCow::Borrowed("I dey expect `)`"),
                        }],
                    );
                }
                let args = self.arena.vec_into_slice(args);
                let args = self.alloc(ArgList { args });
                let end = self.cur.span.end;
                lhs = self.alloc(Expr::Call {
                    callee: lhs,
                    args,
                    span: Range::from(call_start..end),
                });
                continue;
            }

            if let Token::LBracket = self.cur.token {
                let bracket_start = self.cur.span.start;
                self.bump(); // consume '['
                let index_expr = self.parse_expression(0);
                let end = if let Token::RBracket = self.cur.token {
                    let end = self.cur.span.end;
                    self.bump(); // consume ']'
                    end
                } else {
                    self.emit_error(
                        self.cur.span,
                        SyntaxError::ExpectedRBracket,
                        vec![Label {
                            span: self.cur.span,
                            message: ArenaCow::Borrowed("I dey expect `]`"),
                        }],
                    );
                    self.cur.span.end
                };

                lhs = self.alloc(Expr::Index {
                    array: lhs,
                    index: index_expr,
                    index_span: Range::from(bracket_start..end),
                    span: Range::from(start..end),
                });
                continue;
            }

            let (op, l_bp, r_bp) = match &self.cur.token {
                Token::Times => (BinaryOp::Times, 20, 21),
                Token::Divide => (BinaryOp::Divide, 20, 21),
                Token::Mod => (BinaryOp::Mod, 20, 21),
                Token::Add => (BinaryOp::Add, 10, 11),
                Token::Minus => (BinaryOp::Minus, 10, 11),
                Token::Na => (BinaryOp::Eq, 7, 8),
                Token::Pass => (BinaryOp::Gt, 7, 8),
                Token::SmallPass => (BinaryOp::Lt, 7, 8),
                Token::And => (BinaryOp::And, 5, 6),
                Token::Or => (BinaryOp::Or, 1, 2),
                _ => break,
            };

            // `l_bp` is not greater?
            if l_bp < min_bp {
                break;
            }
            self.bump(); // consume the operator
            let rhs = self.parse_expression(r_bp);
            let end = self.cur.span.end;
            lhs = self.alloc(Expr::Binary { op, lhs, rhs, span: Range::from(start..end) });
        }
        lhs
    }

    fn expr_span(expr: ExprRef<'ast>) -> Span {
        match expr {
            Expr::Number(.., span) => *span,
            Expr::String { span, .. } => *span,
            Expr::Bool(.., span) => *span,
            Expr::Var(.., span) => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Array { span, .. } => *span,
            Expr::Member { span, .. } => *span,
        }
    }

    fn parse_string_literal(&mut self, content: ArenaCow<'ast, 'src>, span: Span) -> ExprRef<'ast> {
        let bytes = content.as_bytes();
        let len = bytes.len();

        // No braces = static string
        let index = memchr(b'{', bytes, 0);
        if index == len {
            let s = self.alloc_str(&content);
            return self.alloc(Expr::String { parts: StringParts::Static(s), span });
        }

        let template: &'src str = match &content {
            ArenaCow::Borrowed(s) => s,
            ArenaCow::Owned(..) => {
                let s = self.alloc_str(&content);
                return self.alloc(Expr::String { parts: StringParts::Static(s), span });
            }
        };

        let segments = self.parse_template_segments(template);
        if segments.is_empty() {
            let s = self.alloc_str(&content);
            return self.alloc(Expr::String { parts: StringParts::Static(s), span });
        }

        let s = self.arena.vec_into_slice(segments);
        self.alloc(Expr::String { parts: StringParts::Interpolated(s), span })
    }

    fn parse_template_segments(
        &self,
        template: &'src str,
    ) -> Vec<StringSegment<'ast>, &'ast Arena> {
        let bytes = template.as_bytes();
        let len = bytes.len();
        let mut buffer = Vec::with_capacity_in(len, self.arena);
        // SAFETY: ptr is valid for reads up to len bytes
        let ptr = bytes.as_ptr();

        let (mut beg, mut i) = (0, 0);
        while i < len {
            let index = memchr2(b'{', b'}', bytes, i);
            if index == len {
                break;
            }
            i = index;
            match unsafe { *ptr.add(i) } {
                b'{' => {
                    // Is it an escaped brace `{{`?
                    if i + 1 < len && unsafe { *ptr.add(i + 1) } == b'{' {
                        if i > beg {
                            // SAFETY: beg..i are valid UTF-8 boundaries within template
                            let literal = unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(
                                    ptr.add(beg),
                                    i - beg,
                                ))
                            };
                            buffer.push(StringSegment::Literal(literal));
                        }
                        buffer.push(StringSegment::Literal("{"));
                        i += 2;
                        beg = i;
                        continue;
                    }

                    let mut j = i + 1;
                    while j < len && unsafe { (*ptr.add(j) as char).is_whitespace() } {
                        j += 1;
                    }

                    let name_start = j;
                    if j < len
                        && unsafe {
                            let b = *ptr.add(j);
                            b.is_ascii_alphabetic() || b == b'_'
                        }
                    {
                        j += 1;
                        while j < len
                            && unsafe {
                                let b = *ptr.add(j);
                                b.is_ascii_alphanumeric() || b == b'_'
                            }
                        {
                            j += 1;
                        }
                        let name_end = j;

                        while j < len && unsafe { (*ptr.add(j) as char).is_whitespace() } {
                            j += 1;
                        }

                        if j < len && unsafe { *ptr.add(j) } == b'}' {
                            if i > beg {
                                // SAFETY: beg..i are valid UTF-8 boundaries
                                let literal = unsafe {
                                    str::from_utf8_unchecked(slice::from_raw_parts(
                                        ptr.add(beg),
                                        i - beg,
                                    ))
                                };
                                buffer.push(StringSegment::Literal(literal));
                            }
                            // SAFETY: name_start..name_end are valid UTF-8 boundaries
                            let variable = unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(
                                    ptr.add(name_start),
                                    name_end - name_start,
                                ))
                            };
                            buffer.push(StringSegment::Variable(variable));
                            i = j + 1;
                            beg = i;
                            continue;
                        }
                    }

                    // TODO: Recursively parse nested braces
                    // for now, treat as literal until next '}'
                    let mut end = i + 1;
                    while end < len && unsafe { *ptr.add(end) } != b'}' {
                        end += 1;
                    }
                    if end < len {
                        end += 1; // include '}'
                    }

                    if i > beg {
                        // SAFETY: beg..i are valid UTF-8 boundaries
                        let before = unsafe {
                            str::from_utf8_unchecked(slice::from_raw_parts(ptr.add(beg), i - beg))
                        };
                        buffer.push(StringSegment::Literal(before));
                    }
                    // SAFETY: i..end are valid UTF-8 boundaries
                    let literal = unsafe {
                        str::from_utf8_unchecked(slice::from_raw_parts(ptr.add(i), end - i))
                    };
                    buffer.push(StringSegment::Literal(literal));
                    i = end;
                    beg = i
                }

                b'}' => {
                    if i + 1 < len && unsafe { *ptr.add(i + 1) } == b'}' {
                        if i > beg {
                            // SAFETY: beg..i are valid UTF-8 boundaries
                            let literal = unsafe {
                                str::from_utf8_unchecked(slice::from_raw_parts(
                                    ptr.add(beg),
                                    i - beg,
                                ))
                            };
                            buffer.push(StringSegment::Literal(literal));
                        }
                        buffer.push(StringSegment::Literal("}"));
                        i += 2;
                        beg = i;
                        continue;
                    }
                    i += 1;
                }

                _ => {
                    i += 1;
                }
            }
        }

        if beg < len {
            // SAFETY: beg..len are valid UTF-8 boundaries
            let literal =
                unsafe { str::from_utf8_unchecked(slice::from_raw_parts(ptr.add(beg), len - beg)) };
            buffer.push(StringSegment::Literal(literal));
        }

        buffer
    }
}
