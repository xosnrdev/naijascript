use std::iter::Peekable;

use super::ast::*;
use super::lexer::{self, Lexer, Token};

// Intent: Represents all possible errors that can occur during parsing, including unexpected tokens and lexer errors.
#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
    LexerError(lexer::LexError),
}

impl<'a> std::fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::UnexpectedToken(tok) => write!(f, "Wetin be dis token: {tok}"),
            ParseError::LexerError(e) => write!(f, "{e}"),
        }
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

// Intent: Alias for parser result type, used throughout the parser for error handling.
pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

// Intent: Recursive descent parser for NaijaScript, using a Peekable lexer for lookahead.
pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            tokens: lexer.peekable(),
        }
    }

    // Intent: Expect and consume a specific token, or return a parse error if the next token does not match.
    #[inline]
    fn expect_token(&mut self, expected: Token<'a>) -> ParseResult<'a, ()> {
        match self.tokens.next() {
            Some(Ok(tok)) if tok == expected => Ok(()),
            Some(Ok(tok)) => Err(ParseError::UnexpectedToken(tok)),
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    // Intent: Parse a full program (entry point), collecting all top-level statements.
    pub fn parse_program(&mut self) -> ParseResult<'a, Program<'a>> {
        let statements = self.parse_statement_list()?;
        Ok(Program::new(statements))
    }

    // Intent: Parse a list of statements until EOF or block end, skipping newlines.
    fn parse_statement_list(&mut self) -> ParseResult<'a, Vec<Statement<'a>>> {
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

    // Intent: Try to parse a statement, returning None if EOF is reached.
    fn parse_statement_opt(&mut self) -> ParseResult<'a, Option<Statement<'a>>> {
        self.skip_newlines();
        match self.tokens.peek() {
            Some(Ok(Token::Eof)) | None => Ok(None),
            _ => self.parse_statement().map(Some),
        }
    }

    // Intent: Dispatch to the correct statement parser based on the next token.
    fn parse_statement(&mut self) -> ParseResult<'a, Statement<'a>> {
        let token = self.tokens.peek().ok_or(ParseError::UnexpectedEof)?;
        let token = match token {
            Ok(t) => t,
            Err(e) => return Err(ParseError::LexerError((*e).clone())),
        };
        match token {
            Token::Make => self.parse_assignment(),
            Token::Shout => self.parse_output(),
            Token::IfToSay => self.parse_if(),
            Token::Jasi => self.parse_loop(),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    // Intent: Parse an assignment statement: make x get expr
    fn parse_assignment(&mut self) -> ParseResult<'a, Statement<'a>> {
        self.expect_token(Token::Make)?;
        let variable = match self.tokens.next() {
            Some(Ok(Token::Identifier(name))) => name,
            Some(Ok(tok)) => return Err(ParseError::UnexpectedToken(tok)),
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof),
        };
        self.expect_token(Token::Get)?;
        let value = self.parse_expression()?;
        Ok(Statement::Assignment { variable, value })
    }

    // Intent: Parse an output statement: shout (expr)
    fn parse_output(&mut self) -> ParseResult<'a, Statement<'a>> {
        self.expect_token(Token::Shout)?;
        self.expect_token(Token::LeftParen)?;
        let expr = self.parse_expression()?;
        self.expect_token(Token::RightParen)?;
        Ok(Statement::Output(expr))
    }

    // Intent: Parse an if statement, with optional else block.
    fn parse_if(&mut self) -> ParseResult<'a, Statement<'a>> {
        self.expect_token(Token::IfToSay)?;
        self.expect_token(Token::LeftParen)?;
        let condition = self.parse_condition()?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Start)?;
        let then_block = self.parse_block()?;
        let else_block = match self.tokens.peek() {
            Some(Ok(Token::IfNotSo)) => {
                self.tokens.next();
                self.expect_token(Token::Start)?;
                Some(self.parse_block()?)
            }
            _ => None,
        };
        Ok(Statement::If {
            condition,
            then_block,
            else_block,
        })
    }

    // Intent: Parse a block: statements until 'end'.
    fn parse_block(&mut self) -> ParseResult<'a, Block<'a>> {
        let mut statements = Vec::new();
        loop {
            self.skip_newlines();
            match self.tokens.peek() {
                Some(Ok(Token::End)) => {
                    self.tokens.next();
                    break;
                }
                Some(Ok(Token::Eof)) | None => return Err(ParseError::UnexpectedEof),
                _ => statements.push(self.parse_statement()?),
            }
        }
        Ok(Block::new(statements))
    }

    // Intent: Parse a condition: e.g. x na 5, x pass 3, x small pass 2
    fn parse_condition(&mut self) -> ParseResult<'a, Condition<'a>> {
        let left = self.parse_expression()?;
        let op = match self.tokens.next() {
            Some(Ok(Token::Na)) => "na",
            Some(Ok(Token::Pass)) => "pass",
            Some(Ok(Token::SmallPass)) => "small pass",
            Some(Ok(tok)) => return Err(ParseError::UnexpectedToken(tok)),
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof),
        };
        let right = self.parse_expression()?;
        let cond = match op {
            "na" => Condition::Na(left, right),
            "pass" => Condition::Pass(left, right),
            "small pass" => Condition::SmallPass(left, right),
            _ => unreachable!(),
        };
        Ok(cond)
    }

    // Intent: Parse a loop statement: jasi (cond) start ... end
    fn parse_loop(&mut self) -> ParseResult<'a, Statement<'a>> {
        self.expect_token(Token::Jasi)?;
        self.expect_token(Token::LeftParen)?;
        let condition = self.parse_condition()?;
        self.expect_token(Token::RightParen)?;
        self.expect_token(Token::Start)?;
        let body = self.parse_block()?;
        Ok(Statement::Loop { condition, body })
    }

    // Intent: Parse an expression using Pratt parsing (precedence climbing).
    #[inline]
    pub fn parse_expression(&mut self) -> ParseResult<'a, Expression<'a>> {
        self.parse_expr_bp(0)
    }

    // Intent: Pratt parser core: parse expression with minimum binding power.
    #[inline]
    fn parse_expr_bp(&mut self, min_bp: u8) -> ParseResult<'a, Expression<'a>> {
        let mut lhs = match self.tokens.next() {
            Some(Ok(Token::Number(n))) => Expression::number(n),
            Some(Ok(Token::Identifier(name))) => Expression::variable(name),
            Some(Ok(Token::LeftParen)) => {
                let expr = self.parse_expression()?;
                match self.tokens.next() {
                    Some(Ok(Token::RightParen)) => {}
                    Some(Ok(tok)) => return Err(ParseError::UnexpectedToken(tok)),
                    Some(Err(e)) => return Err(ParseError::LexerError(e)),
                    None => return Err(ParseError::UnexpectedEof),
                }
                Expression::grouping(expr)
            }
            Some(Ok(tok)) => return Err(ParseError::UnexpectedToken(tok)),
            Some(Err(e)) => return Err(ParseError::LexerError(e)),
            None => return Err(ParseError::UnexpectedEof),
        };

        // Intent: Loop to parse left-associative binary operators with correct precedence.
        loop {
            let op = match self.tokens.peek() {
                Some(Ok(Token::Add)) => BinaryOp::Add,
                Some(Ok(Token::Minus)) => BinaryOp::Minus,
                Some(Ok(Token::Times)) => BinaryOp::Times,
                Some(Ok(Token::Divide)) => BinaryOp::Divide,
                _ => break,
            };
            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }
            self.tokens.next();
            let rhs = self.parse_expr_bp(r_bp)?;
            lhs = Expression::binary(lhs, op, rhs);
        }
        Ok(lhs)
    }

    // Intent: Skip over any Newline tokens, returning true if any were skipped or if more tokens remain.
    fn skip_newlines(&mut self) -> bool {
        let mut skipped = false;
        while let Some(Ok(Token::Newline)) = self.tokens.peek() {
            self.tokens.next();
            skipped = true;
        }
        skipped || self.tokens.peek().is_some()
    }
}

// Intent: Get binding power for each binary operator (Pratt parsing)
fn infix_binding_power(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Add | BinaryOp::Minus => (1, 2),
        BinaryOp::Times | BinaryOp::Divide => (3, 4),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(input: &str) -> Program {
        let mut parser = Parser::new(Lexer::new(input));
        parser.parse_program().expect("parse success")
    }

    #[test]
    fn test_assignment() {
        let prog = parse_ok("make x get 5");
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0] {
            Statement::Assignment { variable, value } => {
                assert_eq!(variable, &"x");
                assert_eq!(value, &Expression::Number(5.0));
            }
            _ => panic!("not assignment"),
        }
    }

    #[test]
    fn test_output() {
        let prog = parse_ok("shout ( 2 add 3 )");
        match &prog.statements[0] {
            Statement::Output(expr) => match expr {
                Expression::Binary { op, .. } => assert_eq!(*op, BinaryOp::Add),
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
            Statement::If {
                condition,
                then_block,
                else_block,
            } => {
                match condition {
                    Condition::Na(l, r) => {
                        assert!(matches!(l, Expression::Variable(_)));
                        assert!(matches!(r, Expression::Number(_)));
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
            Statement::If {
                then_block,
                else_block,
                ..
            } => {
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
            Statement::Loop { condition, body } => {
                match condition {
                    Condition::SmallPass(_, _) => {}
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
                Expression::Binary {
                    op: BinaryOp::Add,
                    left,
                    right,
                } => {
                    assert!(matches!(&**left, Expression::Number(2.0)));
                    match &**right {
                        Expression::Binary {
                            op: BinaryOp::Times,
                            left: l,
                            right: r,
                        } => {
                            assert!(matches!(&**l, Expression::Number(3.0)));
                            assert!(matches!(&**r, Expression::Number(4.0)));
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
