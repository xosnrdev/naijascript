//! Abstract syntax tree for the `.nui` declarative UI DSL.
//!
//! See `docs/spec/UI_DSL.md` for the normative grammar. This tree is produced
//! by the DSL parser, type-checked by the DSL resolver, and evaluated into the
//! renderer-neutral IR by the engine.

use crate::diagnostics::Span;

/// A parsed `.nui` compilation unit.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub screens: Vec<Screen>,
    pub components: Vec<Component>,
}

/// A top-level screen declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Screen {
    pub name: String,
    pub params: Vec<Param>,
    pub state: Vec<StateDecl>,
    pub handlers: Vec<Handler>,
    pub view: Vec<Child>,
    pub span: Span,
}

/// A reusable component declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Component {
    pub name: String,
    pub params: Vec<Param>,
    pub state: Vec<StateDecl>,
    pub handlers: Vec<Handler>,
    pub view: Vec<Child>,
    pub span: Span,
}

/// A typed parameter with an optional default.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeRef,
    pub default: Option<Expr>,
    pub span: Span,
}

/// A reactive local-state declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct StateDecl {
    pub name: String,
    pub ty: TypeRef,
    pub init: Expr,
    pub span: Span,
}

/// A named event handler.
#[derive(Debug, Clone, PartialEq)]
pub struct Handler {
    pub name: String,
    pub event_param: String,
    pub event_ty: EventTy,
    pub body: Vec<Stmt>,
    pub span: Span,
}

/// The event type a handler accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventTy {
    Tap,
    Change,
    Submit,
    Focus,
}

/// A statement inside a handler body.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Assignment to a state variable: `count get count add 1`.
    Assign { target: String, value: Expr, span: Span },
}

/// A static type reference.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    Text,
    Number,
    Boolean,
    Color,
    Length,
    ImageSource,
    Key,
    Alignment,
    Axis,
    ImageFit,
    List(Box<TypeRef>),
    Named(String),
    Span(Span),
}

/// A child node in a view.
#[derive(Debug, Clone, PartialEq)]
pub enum Child {
    Element {
        name: String,
        props: Vec<PropSet>,
        key: Option<Expr>,
        children: Vec<Child>,
        span: Span,
    },
    Text {
        value: Expr,
        span: Span,
    },
    Show {
        cond: Expr,
        then_children: Vec<Child>,
        otherwise: Vec<Child>,
        span: Span,
    },
    Each {
        item: String,
        collection: Expr,
        key: Expr,
        body: Vec<Child>,
        span: Span,
    },
}

/// A single property assignment on an element.
#[derive(Debug, Clone, PartialEq)]
pub struct PropSet {
    pub name: String,
    pub value: PropExpr,
    pub span: Span,
}

/// A property value: either an expression or a handler reference.
#[derive(Debug, Clone, PartialEq)]
pub enum PropExpr {
    Expr(Expr),
    HandlerRef { name: String, span: Span },
}

/// A binary operator (Pidgin-spelled).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Minus,
    Times,
    Divide,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

/// A unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not,
    Neg,
}

/// A segment of an interpolated string literal.
#[derive(Debug, Clone, PartialEq)]
pub enum StrPart {
    Literal(String),
    /// A `{path}` interpolation of dotted identifiers.
    Interp(Vec<String>),
}

/// A DSL expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64, Span),
    Str(Vec<StrPart>, Span),
    Bool(bool, Span),
    Null(Span),
    Var(String, Span),
    Length(f64, Span),
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
        span: Span,
    },
    Member {
        object: Box<Expr>,
        field: String,
        span: Span,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    /// `Color.rgb(...)` / `Color.rgba(...)`.
    ColorCall {
        args: Vec<Expr>,
        span: Span,
    },
    /// `ImageSource.asset("name")`.
    AssetCall {
        arg: Box<Expr>,
        span: Span,
    },
    /// A qualified enum literal such as `Axis.vertical`.
    EnumLit {
        ty: String,
        case: String,
        span: Span,
    },
}

impl Expr {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(_, span)
            | Expr::Str(_, span)
            | Expr::Bool(_, span)
            | Expr::Null(span)
            | Expr::Var(_, span)
            | Expr::Length(_, span)
            | Expr::Binary { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Member { span, .. }
            | Expr::Index { span, .. }
            | Expr::ColorCall { span, .. }
            | Expr::AssetCall { span, .. }
            | Expr::EnumLit { span, .. } => *span,
        }
    }
}
