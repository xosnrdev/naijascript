//! Recursive-descent parser for the `.nui` DSL.
//!
//! Produces the [`ast`] tree from the token stream. Pidgin operator words are
//! recognized here (including multi-word forms such as `no small pass`), giving
//! precedence-climbing expressions that mirror core `NaijaScript` spelling. See
//! `docs/spec/UI_DSL.md`.

use crate::diagnostics::Span;
use crate::ui::dsl::ast::{
    BinOp, Child, Component, EventTy, Expr, Handler, Param, Program, PropExpr, PropSet, Screen,
    Stmt, StrPart, TypeRef, UnOp,
};
use crate::ui::dsl::scanner::{Spanned, Tok, tokenize};
use crate::ui::error::UiError;

/// Parses `.nui` source into a [`Program`].
pub fn parse(source: &str) -> Result<Program, UiError> {
    let toks = tokenize(source)?;
    let mut parser = Parser { toks, pos: 0 };
    parser.program()
}

struct Parser {
    toks: Vec<Spanned>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }

    fn peek_at(&self, offset: usize) -> Option<&Tok> {
        self.toks.get(self.pos + offset).map(|spanned| &spanned.tok)
    }

    fn peek_word(&self) -> Option<&str> {
        match self.peek() {
            Tok::Word(word) => Some(word.as_str()),
            _ => None,
        }
    }

    fn word_at(&self, offset: usize) -> Option<&str> {
        match self.peek_at(offset) {
            Some(Tok::Word(word)) => Some(word.as_str()),
            _ => None,
        }
    }

    fn span(&self) -> Span {
        self.toks[self.pos].span
    }

    fn advance(&mut self) -> Spanned {
        let spanned = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        spanned
    }

    fn at_eof(&self) -> bool {
        matches!(self.peek(), Tok::Eof)
    }

    fn eat_word(&mut self, kw: &str) -> bool {
        if self.peek_word() == Some(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_word(&mut self, kw: &str) -> Result<Span, UiError> {
        let span = self.span();
        if self.eat_word(kw) {
            Ok(span)
        } else {
            Err(self.error("NUI0100", format!("expected `{kw}`")))
        }
    }

    fn expect(&mut self, tok: &Tok, name: &str) -> Result<Span, UiError> {
        let span = self.span();
        if self.peek() == tok {
            self.advance();
            Ok(span)
        } else {
            Err(self.error("NUI0100", format!("expected `{name}`")))
        }
    }

    fn expect_name(&mut self) -> Result<(String, Span), UiError> {
        let span = self.span();
        match self.peek().clone() {
            Tok::Word(word) if !is_keyword(&word) => {
                self.advance();
                Ok((word, span))
            }
            _ => Err(self.error("NUI0101", "expected an identifier")),
        }
    }

    fn error(&self, code: &'static str, message: impl Into<String>) -> UiError {
        UiError::compile(code, message).with_span(self.span())
    }

    fn program(&mut self) -> Result<Program, UiError> {
        let mut screens = Vec::new();
        let mut components = Vec::new();
        while !self.at_eof() {
            match self.peek_word() {
                Some("screen") => screens.push(self.screen()?),
                Some("component") => components.push(self.component()?),
                _ => return Err(self.error("NUI0102", "expected `screen` or `component`")),
            }
        }
        if screens.is_empty() && components.is_empty() {
            return Err(self.error("NUI0103", "a program needs at least one screen or component"));
        }
        Ok(Program { screens, components })
    }

    fn screen(&mut self) -> Result<Screen, UiError> {
        let start = self.expect_word("screen")?;
        let (name, _) = self.expect_name()?;
        let params = self.opt_param_list()?;
        self.expect_word("start")?;
        let (state, handlers) = self.members()?;
        self.expect_word("view")?;
        self.expect_word("start")?;
        let view = self.children_until_end()?;
        self.expect_word("end")?; // view block
        let end = self.expect_word("end")?; // screen block
        Ok(Screen { name, params, state, handlers, view, span: join(start, end) })
    }

    fn component(&mut self) -> Result<Component, UiError> {
        let start = self.expect_word("component")?;
        let (name, _) = self.expect_name()?;
        let params = self.opt_param_list()?;
        self.expect_word("start")?;
        let (state, handlers) = self.members()?;
        self.expect_word("view")?;
        self.expect_word("start")?;
        let view = self.children_until_end()?;
        self.expect_word("end")?; // view block
        let end = self.expect_word("end")?; // component block
        Ok(Component { name, params, state, handlers, view, span: join(start, end) })
    }

    fn opt_param_list(&mut self) -> Result<Vec<Param>, UiError> {
        let mut params = Vec::new();
        if !matches!(self.peek(), Tok::LParen) {
            return Ok(params);
        }
        self.advance();
        if matches!(self.peek(), Tok::RParen) {
            self.advance();
            return Ok(params);
        }
        loop {
            let (name, name_span) = self.expect_name()?;
            self.expect(&Tok::Colon, ":")?;
            let ty = self.type_ref()?;
            let default = if self.eat_word("get") { Some(self.expression()?) } else { None };
            params.push(Param { name, ty, default, span: name_span });
            if matches!(self.peek(), Tok::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        self.expect(&Tok::RParen, ")")?;
        Ok(params)
    }

    fn members(&mut self) -> Result<(Vec<crate::ui::dsl::ast::StateDecl>, Vec<Handler>), UiError> {
        let mut state = Vec::new();
        let mut handlers = Vec::new();
        loop {
            match self.peek_word() {
                Some("state") => state.push(self.state_decl()?),
                Some("when") => handlers.push(self.handler()?),
                _ => break,
            }
        }
        Ok((state, handlers))
    }

    fn state_decl(&mut self) -> Result<crate::ui::dsl::ast::StateDecl, UiError> {
        let start = self.expect_word("state")?;
        let (name, _) = self.expect_name()?;
        self.expect(&Tok::Colon, ":")?;
        let ty = self.type_ref()?;
        self.expect_word("get")?;
        let init = self.expression()?;
        let span = join(start, init.span());
        Ok(crate::ui::dsl::ast::StateDecl { name, ty, init, span })
    }

    fn handler(&mut self) -> Result<Handler, UiError> {
        let start = self.expect_word("when")?;
        let (name, _) = self.expect_name()?;
        self.expect(&Tok::LParen, "(")?;
        let (event_param, _) = self.expect_name()?;
        self.expect(&Tok::Colon, ":")?;
        let (event_ty_name, ty_span) = self.expect_name()?;
        let event_ty = match event_ty_name.as_str() {
            "TapEvent" => EventTy::Tap,
            "ChangeEvent" => EventTy::Change,
            "SubmitEvent" => EventTy::Submit,
            "FocusEvent" => EventTy::Focus,
            _ => {
                return Err(UiError::compile("NUI0104", "unknown event type").with_span(ty_span));
            }
        };
        self.expect(&Tok::RParen, ")")?;
        self.expect_word("start")?;
        let mut body = Vec::new();
        while self.peek_word() != Some("end") && !self.at_eof() {
            body.push(self.statement()?);
        }
        let end = self.expect_word("end")?;
        Ok(Handler { name, event_param, event_ty, body, span: join(start, end) })
    }

    fn statement(&mut self) -> Result<Stmt, UiError> {
        let (target, start) = self.expect_name()?;
        self.expect_word("get")?;
        let value = self.expression()?;
        let span = join(start, value.span());
        Ok(Stmt::Assign { target, value, span })
    }

    fn children_until_end(&mut self) -> Result<Vec<Child>, UiError> {
        let mut children = Vec::new();
        while self.peek_word() != Some("end") && !self.at_eof() {
            children.push(self.child()?);
        }
        Ok(children)
    }

    fn child(&mut self) -> Result<Child, UiError> {
        match self.peek_word() {
            Some("text") => self.text_child(),
            Some("show") => self.show_child(),
            Some("each") => self.each_child(),
            Some(_) => self.element_child(),
            None => Err(self.error("NUI0105", "expected a view child")),
        }
    }

    fn text_child(&mut self) -> Result<Child, UiError> {
        let start = self.expect_word("text")?;
        let value = self.expression()?;
        Ok(Child::Text { span: join(start, value.span()), value })
    }

    fn show_child(&mut self) -> Result<Child, UiError> {
        let start = self.expect_word("show")?;
        let cond = self.expression()?;
        self.expect_word("start")?;
        let then_children = self.children_until_end()?;
        let mut end = self.expect_word("end")?;
        let mut otherwise = Vec::new();
        if self.eat_word("otherwise") {
            self.expect_word("start")?;
            otherwise = self.children_until_end()?;
            end = self.expect_word("end")?;
        }
        Ok(Child::Show { cond, then_children, otherwise, span: join(start, end) })
    }

    fn each_child(&mut self) -> Result<Child, UiError> {
        let start = self.expect_word("each")?;
        let (item, _) = self.expect_name()?;
        self.expect_word("in")?;
        let collection = self.expression()?;
        self.expect_word("key")?;
        let key = self.expression()?;
        self.expect_word("start")?;
        let body = self.children_until_end()?;
        let end = self.expect_word("end")?;
        Ok(Child::Each { item, collection, key, body, span: join(start, end) })
    }

    fn element_child(&mut self) -> Result<Child, UiError> {
        let (name, start) = self.expect_name()?;
        let props = self.opt_prop_list()?;
        let key = if self.eat_word("key") { Some(self.expression()?) } else { None };
        let (children, end) = if matches!(self.peek(), Tok::Slash) {
            let slash = self.span();
            self.advance();
            (Vec::new(), slash)
        } else {
            let block_start = self.expect_word("start")?;
            let children = self.children_until_end()?;
            let end = self.expect_word("end")?;
            let _ = block_start;
            (children, end)
        };
        Ok(Child::Element { name, props, key, children, span: join(start, end) })
    }

    fn opt_prop_list(&mut self) -> Result<Vec<PropSet>, UiError> {
        let mut props = Vec::new();
        if !matches!(self.peek(), Tok::LParen) {
            return Ok(props);
        }
        self.advance();
        if matches!(self.peek(), Tok::RParen) {
            self.advance();
            return Ok(props);
        }
        loop {
            let (name, name_span) = self.expect_name()?;
            self.expect(&Tok::Colon, ":")?;
            let value = if matches!(self.peek(), Tok::At) {
                let at = self.span();
                self.advance();
                let (handler, hspan) = self.expect_name()?;
                PropExpr::HandlerRef { name: handler, span: join(at, hspan) }
            } else {
                PropExpr::Expr(self.expression()?)
            };
            props.push(PropSet { name, value, span: name_span });
            if matches!(self.peek(), Tok::Comma) {
                self.advance();
                continue;
            }
            break;
        }
        self.expect(&Tok::RParen, ")")?;
        Ok(props)
    }

    fn type_ref(&mut self) -> Result<TypeRef, UiError> {
        let (name, span) = self.expect_name()?;
        let ty = match name.as_str() {
            "Text" => TypeRef::Text,
            "Number" => TypeRef::Number,
            "Boolean" => TypeRef::Boolean,
            "Color" => TypeRef::Color,
            "Length" => TypeRef::Length,
            "ImageSource" => TypeRef::ImageSource,
            "Key" => TypeRef::Key,
            "Alignment" => TypeRef::Alignment,
            "Axis" => TypeRef::Axis,
            "ImageFit" => TypeRef::ImageFit,
            "List" => {
                self.expect(&Tok::LParen, "(")?;
                let inner = self.type_ref()?;
                self.expect(&Tok::RParen, ")")?;
                return Ok(TypeRef::List(Box::new(inner)));
            }
            _ => TypeRef::Named(name),
        };
        let _ = span;
        Ok(ty)
    }

    // ----- expressions (precedence climbing) -----

    fn expression(&mut self) -> Result<Expr, UiError> {
        self.parse_binary(1)
    }

    fn parse_binary(&mut self, min_level: u8) -> Result<Expr, UiError> {
        let mut lhs = self.parse_unary()?;
        while let Some((op, len)) = self.peek_binop() {
            let level = op_level(op);
            if level < min_level {
                break;
            }
            for _ in 0..len {
                self.advance();
            }
            let rhs = self.parse_binary(level + 1)?;
            let span = join(lhs.span(), rhs.span());
            lhs = Expr::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs), span };
        }
        Ok(lhs)
    }

    fn peek_binop(&self) -> Option<(BinOp, usize)> {
        let w0 = self.peek_word()?;
        let w1 = self.word_at(1);
        let w2 = self.word_at(2);
        match (w0, w1, w2) {
            ("no", Some("small"), Some("pass")) => Some((BinOp::Ge, 3)),
            ("small", Some("pass"), _) => Some((BinOp::Lt, 2)),
            ("no", Some("be"), _) => Some((BinOp::Ne, 2)),
            ("no", Some("pass"), _) => Some((BinOp::Le, 2)),
            ("na", _, _) => Some((BinOp::Eq, 1)),
            ("pass", _, _) => Some((BinOp::Gt, 1)),
            ("add", _, _) => Some((BinOp::Add, 1)),
            ("minus", _, _) => Some((BinOp::Minus, 1)),
            ("times", _, _) => Some((BinOp::Times, 1)),
            ("divide", _, _) => Some((BinOp::Divide, 1)),
            ("mod", _, _) => Some((BinOp::Mod, 1)),
            ("and", _, _) => Some((BinOp::And, 1)),
            ("or", _, _) => Some((BinOp::Or, 1)),
            _ => None,
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, UiError> {
        let start = self.span();
        if self.peek_word() == Some("not") {
            self.advance();
            let expr = self.parse_unary()?;
            let span = join(start, expr.span());
            return Ok(Expr::Unary { op: UnOp::Not, expr: Box::new(expr), span });
        }
        if self.peek_word() == Some("minus") {
            self.advance();
            let expr = self.parse_unary()?;
            let span = join(start, expr.span());
            return Ok(Expr::Unary { op: UnOp::Neg, expr: Box::new(expr), span });
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, UiError> {
        let mut expr = self.parse_primary()?;
        while matches!(self.peek(), Tok::Dot) {
            self.advance();
            let (field, fspan) = self.expect_name()?;
            let span = join(expr.span(), fspan);
            expr = Expr::Member { object: Box::new(expr), field, span };
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, UiError> {
        let span = self.span();
        match self.peek().clone() {
            Tok::Number(value) => {
                self.advance();
                Ok(Expr::Number(value, span))
            }
            Tok::Length(value) => {
                self.advance();
                Ok(Expr::Length(value, span))
            }
            Tok::Str(raw) => {
                self.advance();
                Ok(Expr::Str(parse_interpolation(&raw), span))
            }
            Tok::LParen => {
                self.advance();
                let inner = self.expression()?;
                self.expect(&Tok::RParen, ")")?;
                Ok(inner)
            }
            Tok::Word(word) => self.parse_word_primary(&word, span),
            _ => Err(self.error("NUI0106", "expected an expression")),
        }
    }

    fn parse_word_primary(&mut self, word: &str, span: Span) -> Result<Expr, UiError> {
        match word {
            "true" => {
                self.advance();
                Ok(Expr::Bool(true, span))
            }
            "false" => {
                self.advance();
                Ok(Expr::Bool(false, span))
            }
            "null" => {
                self.advance();
                Ok(Expr::Null(span))
            }
            "Color"
                if matches!(self.peek_at(1), Some(Tok::Dot))
                    && matches!(self.word_at(2), Some("rgb" | "rgba")) =>
            {
                self.advance(); // Color
                self.advance(); // .
                self.advance(); // rgb / rgba
                self.color_call(span)
            }
            "ImageSource"
                if matches!(self.peek_at(1), Some(Tok::Dot))
                    && self.word_at(2) == Some("asset") =>
            {
                self.advance(); // ImageSource
                self.advance(); // .
                self.advance(); // asset
                self.asset_call(span)
            }
            _ if matches!(word, "Alignment" | "Axis" | "ImageFit")
                && matches!(self.peek_at(1), Some(Tok::Dot)) =>
            {
                let ty = word.to_owned();
                self.advance(); // type word
                self.advance(); // dot
                let (case, cspan) = self.expect_name()?;
                Ok(Expr::EnumLit { ty, case, span: join(span, cspan) })
            }
            _ => {
                let (name, nspan) = self.expect_name()?;
                Ok(Expr::Var(name, nspan))
            }
        }
    }

    fn color_call(&mut self, span: Span) -> Result<Expr, UiError> {
        self.expect(&Tok::LParen, "(")?;
        let mut args = Vec::new();
        if !matches!(self.peek(), Tok::RParen) {
            loop {
                args.push(self.expression()?);
                if matches!(self.peek(), Tok::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
        }
        let end = self.expect(&Tok::RParen, ")")?;
        Ok(Expr::ColorCall { args, span: join(span, end) })
    }

    fn asset_call(&mut self, span: Span) -> Result<Expr, UiError> {
        self.expect(&Tok::LParen, "(")?;
        let arg = self.expression()?;
        let end = self.expect(&Tok::RParen, ")")?;
        Ok(Expr::AssetCall { arg: Box::new(arg), span: join(span, end) })
    }
}

fn parse_interpolation(raw: &str) -> Vec<StrPart> {
    let mut parts = Vec::new();
    let mut literal = String::new();
    let mut chars = raw.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\u{1}' => literal.push('{'),
            '\u{2}' => literal.push('}'),
            '{' => {
                if !literal.is_empty() {
                    parts.push(StrPart::Literal(std::mem::take(&mut literal)));
                }
                let mut path = String::new();
                for inner in chars.by_ref() {
                    if inner == '}' {
                        break;
                    }
                    path.push(inner);
                }
                let segments: Vec<String> =
                    path.split('.').map(|segment| segment.trim().to_owned()).collect();
                parts.push(StrPart::Interp(segments));
            }
            other => literal.push(other),
        }
    }
    if !literal.is_empty() {
        parts.push(StrPart::Literal(literal));
    }
    if parts.is_empty() {
        parts.push(StrPart::Literal(String::new()));
    }
    parts
}

const fn op_level(op: BinOp) -> u8 {
    match op {
        BinOp::Or => 1,
        BinOp::And => 2,
        BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => 3,
        BinOp::Add | BinOp::Minus => 4,
        BinOp::Times | BinOp::Divide | BinOp::Mod => 5,
    }
}

fn join(a: Span, b: Span) -> Span {
    Span::from(a.start.min(b.start)..a.end.max(b.end))
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "screen"
            | "component"
            | "state"
            | "when"
            | "view"
            | "show"
            | "otherwise"
            | "each"
            | "in"
            | "key"
            | "start"
            | "end"
            | "get"
            | "text"
    )
}
