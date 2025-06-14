//! The parser for NaijaScript.
//!
//! It implements recursive descent for statements and pratt for expressions parsing.

use std::iter::Peekable;

use super::ast::*;
use super::lexer::{self, Lexer, Token, TokenKind};
use crate::diagnostics::{Diagnostic, DiagnosticHandler};
use crate::syntax::{Span, offset_to_line_col};

/// Represents a syntax error encountered during parsing.
#[derive(Debug, PartialEq)]
pub enum ParseError<'source> {
    /// The input ended unexpectedly.
    UnexpectedEof(Span),
    /// A token did not match the expected grammar.
    UnexpectedToken { found: Token<'source>, expected: Option<TokenKind<'source>> },
    /// A lexer error was encountered and propagated.
    LexerError(lexer::LexError<'source>),
}

impl<'source> ParseError<'source> {
    /// Converts this parse error into a user-facing diagnostic.
    pub fn to_diagnostic(
        &self,
        source: &'source str,
        filename: Option<&'source str>,
    ) -> Diagnostic<'source> {
        match self {
            ParseError::UnexpectedEof(span) => {
                let (line, col) = offset_to_line_col(source, span.start);
                Diagnostic::error(
                    "syntax error",
                    "Omo! Your code finish for middle, e never complete",
                    source,
                    (span.start, span.end),
                    line,
                    col,
                    filename,
                )
            }
            ParseError::UnexpectedToken { found, expected } => {
                let (line, column) = offset_to_line_col(source, found.span.start);
                let msg = if let Some(exp) = expected {
                    format!("Omo! I see '{}', but I dey expect '{}' for here.", found.kind, exp)
                } else {
                    format!("Omo! I no sabi this token: '{}'.", found.kind)
                };
                Diagnostic::error(
                    "syntax error",
                    Box::leak(msg.into_boxed_str()),
                    source,
                    (found.span.start, found.span.end),
                    line,
                    column,
                    filename,
                )
            }
            ParseError::LexerError(e) => e.to_diagnostic(source, filename),
        }
    }
}

/// The result type for parser operations.
pub type ParseResult<'source, T> = Result<T, ParseError<'source>>;

/// The main parser struct for NaijaScript.
///
/// The parser operates on a peekable iterator of tokens, allowing lookahead for grammar decisions.
pub struct Parser<'source> {
    /// The stream of tokens to be parsed.
    tokens: Peekable<Lexer<'source>>,
}

impl<'source> Parser<'source> {
    /// Constructs a new parser from a lexer.
    #[inline]
    pub fn new(lexer: Lexer<'source>) -> Self {
        Parser { tokens: lexer.peekable() }
    }

    /// Parses a complete program and returns the root AST node.
    pub fn parse_program(&mut self) -> ParseResult<'source, Program<'source>> {
        let statements = self.parse_statement_list()?;
        Ok(Program::new(statements))
    }

    /// Parses a list of statements, skipping newlines, until EOF or an invalid statement is encountered.
    fn parse_statement_list(&mut self) -> ParseResult<'source, Vec<Statement<'source>>> {
        let mut statements = Vec::new();
        while self.skip_newlines() {
            if let Some(stmt) = self.parse_statement_opt()? {
                statements.push(stmt);
            } else {
                break;
            }
        }
        Ok(statements)
    }

    /// Optionally parses a statement, returning `None` at EOF.
    fn parse_statement_opt(&mut self) -> ParseResult<'source, Option<Statement<'source>>> {
        self.skip_newlines();
        match self.tokens.peek() {
            Some(Ok(tok)) if tok.kind == TokenKind::Eof => Ok(None),
            None => Ok(None),
            _ => self.parse_statement().map(Some),
        }
    }

    /// Parses a single statement, dispatching based on the next token.
    fn parse_statement(&mut self) -> ParseResult<'source, Statement<'source>> {
        let token =
            self.tokens.peek().ok_or(ParseError::UnexpectedEof(Span { start: 0, end: 0 }))?;
        let token = match token {
            Ok(t) => t,
            Err(e) => return Err(ParseError::LexerError((*e).clone())),
        };
        match &token.kind {
            TokenKind::Make => self.parse_assignment(),
            TokenKind::Shout => self.parse_output(),
            TokenKind::IfToSay => self.parse_if(),
            TokenKind::Jasi => self.parse_loop(),
            _ => Err(ParseError::UnexpectedToken { found: token.clone(), expected: None }),
        }
    }

    /// Parses an assignment statement: `make x get expr`.
    fn parse_assignment(&mut self) -> ParseResult<'source, Statement<'source>> {
        let make_tok = match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Make => tok,
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Make),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(Span { start: 0, end: 0 })),
        };
        let variable_name = match self.tokens.next() {
            Some(Ok(tok)) => match &tok.kind {
                TokenKind::Identifier(name) => *name,
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        found: tok,
                        expected: Some(TokenKind::Identifier("")),
                    });
                }
            },
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(make_tok.span)),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Get => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Get),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(make_tok.span)),
        };
        let value = self.parse_expression()?;
        let span = Span { start: make_tok.span.start, end: value.span().end };
        Ok(Statement::Assignment { variable: variable_name, value, span })
    }

    /// Parses an output statement: `shout ( expr )`.
    fn parse_output(&mut self) -> ParseResult<'source, Statement<'source>> {
        let shout_tok = match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Shout => tok,
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Shout),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(Span { start: 0, end: 0 })),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::LeftParen => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::LeftParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(shout_tok.span)),
        };
        let expr = self.parse_expression()?;
        let rparen_tok = match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::RightParen => tok,
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::RightParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(shout_tok.span)),
        };
        let span = Span { start: shout_tok.span.start, end: rparen_tok.span.end };
        Ok(Statement::Output(expr, span))
    }

    /// Parses an if statement, including optional else block.
    fn parse_if(&mut self) -> ParseResult<'source, Statement<'source>> {
        let if_tok = match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::IfToSay => tok,
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::IfToSay),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(Span { start: 0, end: 0 })),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::LeftParen => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::LeftParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(if_tok.span)),
        };
        let condition = self.parse_condition()?;
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::RightParen => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::RightParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(if_tok.span)),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Start => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Start),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(if_tok.span)),
        };
        let then_block = self.parse_block()?;
        let mut else_block = None;
        let mut end_span = then_block.span.end;
        if let Some(Ok(tok)) = self.tokens.peek()
            && tok.kind == TokenKind::IfNotSo
        {
            self.tokens.next();
            match self.tokens.next() {
                Some(Ok(tok)) if tok.kind == TokenKind::Start => {}
                Some(Ok(tok)) => {
                    return Err(ParseError::UnexpectedToken {
                        found: tok,
                        expected: Some(TokenKind::Start),
                    });
                }
                Some(Err(e)) => return Err(ParseError::LexerError(e)),
                None => return Err(ParseError::UnexpectedEof(if_tok.span)),
            };
            let else_blk = self.parse_block()?;
            end_span = else_blk.span.end;
            else_block = Some(else_blk);
        }
        let span = Span { start: if_tok.span.start, end: end_span };
        Ok(Statement::If { condition, then_block, else_block, span })
    }

    /// Parses a loop statement: `jasi (cond) start ... end`.
    fn parse_loop(&mut self) -> ParseResult<'source, Statement<'source>> {
        let jasi_tok = match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Jasi => tok,
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Jasi),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(Span { start: 0, end: 0 })),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::LeftParen => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::LeftParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(jasi_tok.span)),
        };
        let condition = self.parse_condition()?;
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::RightParen => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::RightParen),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(jasi_tok.span)),
        };
        match self.tokens.next() {
            Some(Ok(tok)) if tok.kind == TokenKind::Start => {}
            Some(Ok(tok)) => {
                return Err(ParseError::UnexpectedToken {
                    found: tok,
                    expected: Some(TokenKind::Start),
                });
            }
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(jasi_tok.span)),
        };
        let body = self.parse_block()?;
        let span = Span { start: jasi_tok.span.start, end: body.span.end };
        Ok(Statement::Loop { condition, body, span })
    }

    /// Parses a block of statements, ending at `end`.
    fn parse_block(&mut self) -> ParseResult<'source, Block<'source>> {
        let mut statements = Vec::new();
        let mut start = None;
        let end_span = loop {
            self.skip_newlines();
            match self.tokens.peek() {
                Some(Ok(tok)) if tok.kind == TokenKind::End => {
                    let end = tok.span.end;
                    self.tokens.next();
                    break Some(end);
                }
                Some(Ok(tok)) if tok.kind == TokenKind::Eof => {
                    break None;
                }
                None => break None,
                _ => {
                    let stmt = self.parse_statement()?;
                    if start.is_none() {
                        start = Some(stmt.span().start);
                    }
                    statements.push(stmt);
                }
            }
        };
        let span = Span {
            start: start.unwrap_or(0),
            end: statements.last().map(|s| s.span().end).or(end_span).unwrap_or(0),
        };
        Ok(Block { statements, span })
    }

    /// Parses a condition of the form `expr op expr`, where `op` is one of `na`, `pass`, or `small pass`.
    fn parse_condition(&mut self) -> ParseResult<'source, Condition<'source>> {
        let left = self.parse_expression()?;
        let op_tok = match self.tokens.next() {
            Some(Ok(tok)) => tok,
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(left.span())),
        };
        let op = match &op_tok.kind {
            TokenKind::Na => "na",
            TokenKind::Pass => "pass",
            TokenKind::SmallPass => "small pass",
            _ => return Err(ParseError::UnexpectedToken { found: op_tok, expected: None }),
        };
        let right = self.parse_expression()?;
        let span = Span { start: left.span().start, end: right.span().end };
        let cond = match op {
            "na" => Condition::Na(left, right, span),
            "pass" => Condition::Pass(left, right, span),
            "small pass" => Condition::SmallPass(left, right, span),
            _ => unreachable!(),
        };
        Ok(cond)
    }

    /// Parses an expression using the Pratt parsing algorithm.
    ///
    /// This method is the entry point for expression parsing and delegates to `parse_expr_bp` with minimum binding power 0.
    #[inline]
    pub fn parse_expression(&mut self) -> ParseResult<'source, Expression<'source>> {
        self.parse_expr_bp(0)
    }

    /// Pratt parser for expressions, supporting operator precedence and associativity.
    ///
    /// `min_bp` is the minimum binding power required to continue parsing infix operators.
    #[inline]
    fn parse_expr_bp(&mut self, min_bp: u8) -> ParseResult<'source, Expression<'source>> {
        let mut lhs = match self.tokens.next() {
            Some(Ok(tok)) => match &tok.kind {
                TokenKind::Number(n) => Expression::Number(*n, tok.span),
                TokenKind::Identifier(name) => Expression::Variable(name, tok.span),
                TokenKind::LeftParen => {
                    let expr = self.parse_expression()?;
                    let rparen_tok = match self.tokens.next() {
                        Some(Ok(tok2)) if tok2.kind == TokenKind::RightParen => tok2,
                        Some(Ok(tok2)) => {
                            return Err(ParseError::UnexpectedToken {
                                found: tok2,
                                expected: Some(TokenKind::RightParen),
                            });
                        }
                        Some(Err(e)) => return Err(ParseError::LexerError(e)),
                        None => return Err(ParseError::UnexpectedEof(tok.span)),
                    };
                    Expression::Grouping(
                        Box::new(expr),
                        Span { start: tok.span.start, end: rparen_tok.span.end },
                    )
                }
                _ => return Err(ParseError::UnexpectedToken { found: tok, expected: None }),
            },
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof(Span { start: 0, end: 0 })),
        };
        // Parse infix operators with correct precedence.
        while let Some(Ok(tok)) = self.tokens.peek() {
            let op = match &tok.kind {
                TokenKind::Add => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Minus,
                TokenKind::Times => BinaryOp::Times,
                TokenKind::Divide => BinaryOp::Divide,
                _ => break,
            };
            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = Span { start: lhs.span().start, end: rhs.span().end };
            lhs = Expression::Binary { left: Box::new(lhs), op, right: Box::new(rhs), span };
        }
        Ok(lhs)
    }

    /// Skips over any number of newline tokens, returning true if any tokens remain.
    fn skip_newlines(&mut self) -> bool {
        let mut skipped = false;
        while let Some(Ok(tok)) = self.tokens.peek() {
            if tok.kind == TokenKind::Newline {
                self.tokens.next();
                skipped = true;
            } else {
                break;
            }
        }
        skipped || self.tokens.peek().is_some()
    }

    /// Parses a program and reports errors via a diagnostic handler if provided.
    pub fn parse_program_with_handler(
        &mut self,
        handler: Option<&mut dyn DiagnosticHandler>,
        source: &'source str,
        filename: Option<&'source str>,
    ) -> ParseResult<'source, Program<'source>> {
        match self.parse_program() {
            Ok(program) => Ok(program),
            Err(e) => {
                if let Some(h) = handler {
                    h.report(&e.to_diagnostic(source, filename));
                }
                Err(e)
            }
        }
    }
}

/// Returns the left and right binding powers for each infix operator.
///
/// Higher numbers mean higher precedence. Used by the Pratt parser.
fn infix_binding_power(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Add | BinaryOp::Minus => (1, 2),
        BinaryOp::Times | BinaryOp::Divide => (3, 4),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(input: &str) -> Program<'_> {
        let mut parser = Parser::new(Lexer::new(input));
        parser.parse_program().expect("parse success")
    }

    #[test]
    fn test_assignment() {
        let prog = parse_ok("make x get 5");
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0] {
            Statement::Assignment { variable, value, .. } => {
                assert_eq!(variable, &"x");
                assert_eq!(value, &Expression::Number(5.0, Span { start: 11, end: 12 }));
            }
            _ => panic!("not assignment"),
        }
    }

    #[test]
    fn test_output() {
        let prog = parse_ok("shout ( 2 add 3 )");
        match &prog.statements[0] {
            Statement::Output(expr, _) => match expr {
                Expression::Binary { op: BinaryOp::Add, .. } => {}
                _ => panic!("not binary expr"),
            },
            _ => panic!("not output"),
        }
    }

    #[test]
    fn test_if_statement() {
        let src = "if to say ( x na 5 ) start make y get 1 end";
        let prog = parse_ok(src);
        match &prog.statements[0] {
            Statement::If { condition, then_block, else_block, .. } => {
                match condition {
                    Condition::Na(l, r, _) => {
                        assert!(matches!(l, Expression::Variable(_, _)));
                        assert!(matches!(r, Expression::Number(_, _)));
                    }
                    _ => panic!("not na condition"),
                }
                assert_eq!(then_block.statements.len(), 1);
                assert!(else_block.is_none());
            }
            _ => panic!("not if statement"),
        }
    }

    #[test]
    fn test_if_else_statement() {
        let src = "if to say ( x pass 2 ) start make y get 1 end if not so start make y get 0 end";
        let prog = parse_ok(src);
        match &prog.statements[0] {
            Statement::If { then_block, else_block, .. } => {
                assert!(else_block.is_some());
                assert_eq!(then_block.statements.len(), 1);
                assert_eq!(else_block.as_ref().unwrap().statements.len(), 1);
            }
            _ => panic!("not if statement"),
        }
    }

    #[test]
    fn test_loop_statement() {
        let src = "jasi ( x small pass 10 ) start make x get x add 1 end";
        let prog = parse_ok(src);
        match &prog.statements[0] {
            Statement::Loop { condition, body, .. } => {
                match condition {
                    Condition::SmallPass(_, _, _) => {}
                    _ => panic!("not small pass condition"),
                }
                assert_eq!(body.statements.len(), 1);
            }
            _ => panic!("not loop statement"),
        }
    }

    #[test]
    fn test_expression_precedence() {
        let prog = parse_ok("make x get 2 add 3 times 4");
        match &prog.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expression::Binary { op: BinaryOp::Add, left, right, .. } => {
                    assert!(matches!(&**left, Expression::Number(2.0, _)));
                    match &**right {
                        Expression::Binary { op: BinaryOp::Times, left: l, right: r, .. } => {
                            assert!(matches!(&**l, Expression::Number(3.0, _)));
                            assert!(matches!(&**r, Expression::Number(4.0, _)));
                        }
                        _ => panic!("not times binary"),
                    }
                }
                _ => panic!("not add binary"),
            },
            _ => panic!("not assignment"),
        }
    }
}
