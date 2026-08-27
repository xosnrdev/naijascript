//! The retained UI engine session.
//!
//! A [`UiSession`] compiles a `.nui` program, evaluates the selected screen into
//! renderer-neutral IR, and holds the state, identity, handler registry, and
//! revision needed to turn inbound events into minimal patches. All values are
//! owned so the session can live across event boundaries. See
//! `docs/spec/UI_IR_PROTOCOL.md`.

use std::collections::BTreeMap;

use crate::ui::dsl::ast::{
    BinOp, Child, Expr, Handler, Program, PropExpr, PropSet, Screen, Stmt, StrPart, UnOp,
};
use crate::ui::dsl::{self};
use crate::ui::error::{UiError, UiResult};
use crate::ui::event::{
    ActiveHandler, EventKind, EventPayload, HandlerBinding, HandlerGeneration, HandlerId,
    HandlerRegistry, UiEvent,
};
use crate::ui::identity::{IdPath, NodeId};
use crate::ui::ir::{ElementNode, Node, PrimitiveKind, TextNode, UiTree};
use crate::ui::limits::Limits;
use crate::ui::patch::{Patch, Revision, Snapshot, diff};
use crate::ui::value::{Color, EnumType, ImageSource, Length, PropValue};

/// The MVP does not yet retire generations, so every binding shares one.
const BINDING_GENERATION: HandlerGeneration = 1;

/// A dynamically-typed value produced while evaluating the DSL.
#[derive(Debug, Clone, PartialEq)]
pub enum UiVal {
    Number(f64),
    Text(String),
    Bool(bool),
    Null,
    List(Vec<UiVal>),
    Record(BTreeMap<String, UiVal>),
    Color(Color),
    Length(f64),
    Image(ImageSource),
    Enum { ty: EnumType, case: String },
}

impl UiVal {
    fn truthy(&self) -> bool {
        match self {
            UiVal::Bool(value) => *value,
            UiVal::Null => false,
            UiVal::Number(number) => *number != 0.0,
            UiVal::Text(text) => !text.is_empty(),
            UiVal::List(items) => !items.is_empty(),
            _ => true,
        }
    }

    fn to_display(&self) -> String {
        match self {
            UiVal::Number(number) => format_number(*number),
            UiVal::Text(text) => text.clone(),
            UiVal::Bool(value) => value.to_string(),
            UiVal::Null => String::new(),
            UiVal::Enum { case, .. } => case.clone(),
            other => format!("{other:?}"),
        }
    }
}

/// The result of mounting a session.
#[derive(Debug, Clone, PartialEq)]
pub struct MountOutput {
    pub snapshot: Snapshot,
}

/// A retained, event-driven UI session for one screen instance.
pub struct UiSession {
    program: Program,
    screen: usize,
    session_id: u64,
    instance: u64,
    params: BTreeMap<String, UiVal>,
    state: BTreeMap<String, UiVal>,
    tree: UiTree,
    registry: HandlerRegistry,
    revision: Revision,
    limits: Limits,
}

impl UiSession {
    /// Compiles source and mounts the named screen (or the first screen when
    /// `screen_name` is `None`) with the given host parameters.
    pub fn mount(
        source: &str,
        screen_name: Option<&str>,
        params: BTreeMap<String, UiVal>,
        session_id: u64,
        limits: Limits,
    ) -> UiResult<(Self, MountOutput)> {
        let program = dsl::compile(source).map_err(|mut errors| {
            errors.pop().unwrap_or_else(|| UiError::compile("NUI0000", "unknown compile error"))
        })?;

        let screen = match screen_name {
            Some(name) => program
                .screens
                .iter()
                .position(|screen| screen.name == name)
                .ok_or_else(|| UiError::compile("NUI0300", "no screen with that name"))?,
            None => {
                if program.screens.is_empty() {
                    return Err(UiError::compile("NUI0301", "program has no screens"));
                }
                0
            }
        };

        let mut session = UiSession {
            program,
            screen,
            session_id,
            instance: session_id.wrapping_mul(0x9e37_79b9_7f4a_7c15).max(1),
            params,
            state: BTreeMap::new(),
            tree: UiTree::new(Node::Text(TextNode { id: NodeId(0), text: String::new() })),
            registry: HandlerRegistry::new(),
            revision: 1,
            limits,
        };

        session.init_state()?;
        let mut registry = HandlerRegistry::new();
        registry.begin_generation();
        let tree = session.build_tree(&mut registry)?;
        session.limits.check_tree(&tree)?;

        session.tree = tree.clone();
        session.registry = registry;

        let snapshot =
            Snapshot { session_id: session.session_id, tree_revision: session.revision, tree };
        Ok((session, MountOutput { snapshot }))
    }

    #[must_use]
    pub fn session_id(&self) -> u64 {
        self.session_id
    }

    #[must_use]
    pub fn revision(&self) -> Revision {
        self.revision
    }

    #[must_use]
    pub fn tree(&self) -> &UiTree {
        &self.tree
    }

    #[must_use]
    pub fn registry(&self) -> &HandlerRegistry {
        &self.registry
    }

    /// Validates and dispatches an event, returning the resulting patch.
    ///
    /// Stale or oversized events return a non-fatal error and leave the session
    /// unchanged.
    pub fn dispatch(&mut self, event: &UiEvent) -> UiResult<Patch> {
        self.limits.check_event(event)?;
        let active = self.registry.validate(event)?;
        let handler = self
            .screen()
            .handlers
            .iter()
            .find(|handler| handler.name == active.name)
            .cloned()
            .ok_or_else(|| UiError::protocol("UI_HANDLER_MISSING", "active handler not found"))?;

        let mut next_state = self.state.clone();
        self.run_handler(&handler, &event.payload, &mut next_state)?;

        // Re-evaluate against the tentative state before committing.
        let previous_state = std::mem::replace(&mut self.state, next_state);
        let mut registry = HandlerRegistry::new();
        registry.begin_generation();
        let new_tree = match self.build_tree(&mut registry) {
            Ok(tree) => tree,
            Err(error) => {
                self.state = previous_state;
                return Err(error);
            }
        };
        if let Err(error) = self.limits.check_tree(&new_tree) {
            self.state = previous_state;
            return Err(error);
        }

        let operations = diff(&self.tree, &new_tree);
        if let Err(error) = self.limits.check_patch_ops(operations.len()) {
            self.state = previous_state;
            return Err(error);
        }

        let base_revision = self.revision;
        let target_revision = base_revision + 1;
        self.tree = new_tree;
        self.registry = registry;
        self.revision = target_revision;

        Ok(Patch { session_id: self.session_id, base_revision, target_revision, operations })
    }

    fn screen(&self) -> &Screen {
        &self.program.screens[self.screen]
    }

    fn init_state(&mut self) -> UiResult<()> {
        let screen = self.program.screens[self.screen].clone();
        let base = self.params.clone();
        for decl in &screen.state {
            let env = Env::new(&base);
            let value = self.eval_expr(&decl.init, &env)?;
            self.state.insert(decl.name.clone(), value);
        }
        Ok(())
    }

    fn run_handler(
        &self,
        handler: &Handler,
        payload: &EventPayload,
        state: &mut BTreeMap<String, UiVal>,
    ) -> UiResult<()> {
        for stmt in &handler.body {
            match stmt {
                Stmt::Assign { target, value, .. } => {
                    let mut vars = self.params.clone();
                    for (name, val) in state.iter() {
                        vars.insert(name.clone(), val.clone());
                    }
                    vars.insert(handler.event_param.clone(), event_value(payload));
                    let env = Env::new(&vars);
                    let result = self.eval_expr(value, &env)?;
                    state.insert(target.clone(), result);
                }
            }
        }
        Ok(())
    }

    // ----- tree construction -----

    fn build_tree(&self, registry: &mut HandlerRegistry) -> UiResult<UiTree> {
        let screen = self.screen().clone();
        let mut base = self.params.clone();
        for (name, value) in &self.state {
            base.insert(name.clone(), value.clone());
        }
        let env = Env::new(&base);

        let root_path = IdPath::root(self.instance);
        let mut children = Vec::new();
        self.eval_children(&screen.view, &env, &root_path, registry, &mut children)?;

        let root = Node::Element(ElementNode {
            id: root_path.node_id(),
            kind: PrimitiveKind::Stack,
            props: BTreeMap::new(),
            handlers: BTreeMap::new(),
            children,
        });
        Ok(UiTree::new(root))
    }

    fn eval_children(
        &self,
        children: &[Child],
        env: &Env,
        path: &IdPath,
        registry: &mut HandlerRegistry,
        out: &mut Vec<Node>,
    ) -> UiResult<()> {
        for (index, child) in children.iter().enumerate() {
            let slot = index as u32;
            let base = path.slot(slot);
            match child {
                Child::Text { value, .. } => {
                    let text = self.eval_expr(value, env)?.to_display();
                    out.push(Node::Text(TextNode { id: base.node_id(), text }));
                }
                Child::Element { name, props, key, children, .. } => {
                    let element_path = match key {
                        Some(key_expr) => base.key(self.eval_expr(key_expr, env)?.to_display()),
                        None => base,
                    };
                    let node =
                        self.eval_element(name, props, children, env, &element_path, registry)?;
                    out.push(node);
                }
                Child::Show { cond, then_children, otherwise, .. } => {
                    let branch_path = if self.eval_expr(cond, env)?.truthy() {
                        (base.key("then"), then_children)
                    } else {
                        (base.key("else"), otherwise)
                    };
                    self.eval_children(branch_path.1, env, &branch_path.0, registry, out)?;
                }
                Child::Each { item, collection, key, body, .. } => {
                    let value = self.eval_expr(collection, env)?;
                    let UiVal::List(items) = value else {
                        return Err(UiError::validation(
                            "UI_EACH_NOT_LIST",
                            "each expects a list value",
                        ));
                    };
                    for entry in items {
                        let mut inner_vars = env.snapshot();
                        inner_vars.insert(item.clone(), entry);
                        let inner = Env::new(&inner_vars);
                        let key_value = self.eval_expr(key, &inner)?.to_display();
                        let item_path = base.key(key_value);
                        self.eval_children(body, &inner, &item_path, registry, out)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn eval_element(
        &self,
        name: &str,
        props: &[PropSet],
        children: &[Child],
        env: &Env,
        path: &IdPath,
        registry: &mut HandlerRegistry,
    ) -> UiResult<Node> {
        let kind = PrimitiveKind::from_str(name).ok_or_else(|| {
            UiError::validation("UI_UNKNOWN_ELEMENT", format!("unknown element `{name}`"))
        })?;
        let node_id = path.node_id();

        let mut prop_map = BTreeMap::new();
        let mut handlers = BTreeMap::new();
        for prop in props {
            match &prop.value {
                PropExpr::Expr(expr) => {
                    let value = self.eval_expr(expr, env)?;
                    if let Some(prop_value) = to_prop_value(&value)? {
                        prop_map.insert(prop.name.clone(), prop_value);
                    }
                }
                PropExpr::HandlerRef { name: handler_name, .. } => {
                    let event_kind = event_prop_kind(&prop.name).ok_or_else(|| {
                        UiError::validation("UI_UNKNOWN_EVENT_PROP", "unknown event property")
                    })?;
                    let id = self.handler_id(node_id, handler_name);
                    let binding = HandlerBinding { id, generation: BINDING_GENERATION, event_kind };
                    handlers.insert(event_kind, binding);
                    registry.register(
                        node_id,
                        ActiveHandler {
                            name: handler_name.clone(),
                            id,
                            generation: BINDING_GENERATION,
                            event_kind,
                        },
                    );
                }
            }
        }

        let mut child_nodes = Vec::new();
        self.eval_children(children, env, path, registry, &mut child_nodes)?;

        Ok(Node::Element(ElementNode {
            id: node_id,
            kind,
            props: prop_map,
            handlers,
            children: child_nodes,
        }))
    }

    fn handler_id(&self, node_id: NodeId, handler_name: &str) -> HandlerId {
        const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
        const PRIME: u64 = 0x0000_0100_0000_01b3;
        let mut hash = OFFSET;
        let mut mix = |bytes: &[u8]| {
            for &byte in bytes {
                hash ^= u64::from(byte);
                hash = hash.wrapping_mul(PRIME);
            }
        };
        mix(b"handler/1");
        mix(&self.instance.to_le_bytes());
        mix(&node_id.0.to_le_bytes());
        mix(handler_name.as_bytes());
        HandlerId(hash | 1)
    }

    // ----- expression evaluation -----

    fn eval_expr(&self, expr: &Expr, env: &Env) -> UiResult<UiVal> {
        match expr {
            Expr::Number(value, _) => Ok(UiVal::Number(*value)),
            Expr::Length(value, _) => Ok(UiVal::Length(*value)),
            Expr::Bool(value, _) => Ok(UiVal::Bool(*value)),
            Expr::Null(_) => Ok(UiVal::Null),
            Expr::Str(parts, _) => Ok(UiVal::Text(self.eval_string(parts, env)?)),
            Expr::Var(name, _) => env.get(name).cloned().ok_or_else(|| {
                UiError::validation("UI_UNKNOWN_NAME", format!("unknown name `{name}`"))
            }),
            Expr::EnumLit { ty, case, span } => {
                let enum_type = match ty.as_str() {
                    "Alignment" => EnumType::Alignment,
                    "Axis" => EnumType::Axis,
                    "ImageFit" => EnumType::ImageFit,
                    _ => {
                        return Err(UiError::validation("UI_UNKNOWN_ENUM", "unknown enum type")
                            .with_span(*span));
                    }
                };
                if !enum_type.accepts(case) {
                    return Err(UiError::validation("UI_BAD_ENUM_CASE", "invalid enum case")
                        .with_span(*span));
                }
                Ok(UiVal::Enum { ty: enum_type, case: case.clone() })
            }
            Expr::ColorCall { args, span } => self.eval_color(args, env, *span),
            Expr::AssetCall { arg, .. } => {
                let name = self.eval_expr(arg, env)?.to_display();
                Ok(UiVal::Image(ImageSource::Asset(name)))
            }
            Expr::Member { object, field, span } => {
                let value = self.eval_expr(object, env)?;
                match value {
                    UiVal::Record(fields) => fields.get(field).cloned().ok_or_else(|| {
                        UiError::validation("UI_UNKNOWN_FIELD", "record has no such field")
                            .with_span(*span)
                    }),
                    _ => Err(UiError::validation("UI_NOT_RECORD", "member access on non-record")
                        .with_span(*span)),
                }
            }
            Expr::Index { object, index, span } => {
                let collection = self.eval_expr(object, env)?;
                let idx = self.eval_expr(index, env)?;
                match (collection, idx) {
                    (UiVal::List(items), UiVal::Number(number)) => {
                        let i = number as usize;
                        items.get(i).cloned().ok_or_else(|| {
                            UiError::validation("UI_INDEX_RANGE", "list index out of range")
                                .with_span(*span)
                        })
                    }
                    _ => Err(UiError::validation("UI_BAD_INDEX", "invalid index expression")
                        .with_span(*span)),
                }
            }
            Expr::Unary { op, expr, span } => {
                let value = self.eval_expr(expr, env)?;
                match op {
                    UnOp::Not => Ok(UiVal::Bool(!value.truthy())),
                    UnOp::Neg => match value {
                        UiVal::Number(number) => Ok(UiVal::Number(-number)),
                        _ => Err(UiError::validation("UI_TYPE", "cannot negate non-number")
                            .with_span(*span)),
                    },
                }
            }
            Expr::Binary { op, lhs, rhs, span } => {
                let left = self.eval_expr(lhs, env)?;
                let right = self.eval_expr(rhs, env)?;
                eval_binary(*op, left, right, *span)
            }
        }
    }

    fn eval_string(&self, parts: &[StrPart], env: &Env) -> UiResult<String> {
        let mut out = String::new();
        for part in parts {
            match part {
                StrPart::Literal(text) => out.push_str(text),
                StrPart::Interp(path) => {
                    let mut value = env.get(&path[0]).cloned().ok_or_else(|| {
                        UiError::validation("UI_UNKNOWN_NAME", "unknown interpolation name")
                    })?;
                    for field in &path[1..] {
                        value = match value {
                            UiVal::Record(fields) => {
                                fields.get(field).cloned().ok_or_else(|| {
                                    UiError::validation("UI_UNKNOWN_FIELD", "unknown field")
                                })?
                            }
                            _ => {
                                return Err(UiError::validation(
                                    "UI_NOT_RECORD",
                                    "interpolation path is not a record",
                                ));
                            }
                        };
                    }
                    out.push_str(&value.to_display());
                }
            }
        }
        Ok(out)
    }

    fn eval_color(
        &self,
        args: &[Expr],
        env: &Env,
        span: crate::diagnostics::Span,
    ) -> UiResult<UiVal> {
        let mut numbers = Vec::new();
        for arg in args {
            match self.eval_expr(arg, env)? {
                UiVal::Number(number) => numbers.push(number),
                _ => {
                    return Err(UiError::validation("UI_TYPE", "color components must be numbers")
                        .with_span(span));
                }
            }
        }
        let channel = |value: f64| value.clamp(0.0, 255.0) as u8;
        match numbers.as_slice() {
            [r, g, b] => Ok(UiVal::Color(Color::rgb(channel(*r), channel(*g), channel(*b)))),
            [r, g, b, a] => Ok(UiVal::Color(Color::rgba(
                channel(*r),
                channel(*g),
                channel(*b),
                a.clamp(0.0, 1.0),
            ))),
            _ => {
                Err(UiError::validation("UI_ARITY", "color expects 3 or 4 components")
                    .with_span(span))
            }
        }
    }
}

/// A read-only variable environment for evaluation.
struct Env<'a> {
    vars: &'a BTreeMap<String, UiVal>,
}

impl<'a> Env<'a> {
    fn new(vars: &'a BTreeMap<String, UiVal>) -> Self {
        Self { vars }
    }

    fn get(&self, name: &str) -> Option<&UiVal> {
        self.vars.get(name)
    }

    fn snapshot(&self) -> BTreeMap<String, UiVal> {
        self.vars.clone()
    }
}

fn event_value(payload: &EventPayload) -> UiVal {
    let mut fields = BTreeMap::new();
    match payload {
        EventPayload::Tap => {}
        EventPayload::Change { value } => {
            fields.insert("value".to_owned(), UiVal::Text(value.clone()));
        }
        EventPayload::Submit { value } => {
            fields.insert("value".to_owned(), value.clone().map_or(UiVal::Null, UiVal::Text));
        }
        EventPayload::Focus { focused } => {
            fields.insert("focused".to_owned(), UiVal::Bool(*focused));
        }
    }
    UiVal::Record(fields)
}

fn event_prop_kind(prop: &str) -> Option<EventKind> {
    match prop {
        "onTap" => Some(EventKind::Tap),
        "onChange" => Some(EventKind::Change),
        "onSubmit" => Some(EventKind::Submit),
        "onFocus" => Some(EventKind::Focus),
        _ => None,
    }
}

fn to_prop_value(value: &UiVal) -> UiResult<Option<PropValue>> {
    let prop = match value {
        UiVal::Number(number) => PropValue::Number(*number),
        UiVal::Text(text) => PropValue::Text(text.clone()),
        UiVal::Bool(flag) => PropValue::Boolean(*flag),
        UiVal::Color(color) => PropValue::Color(*color),
        UiVal::Length(px) => PropValue::Length(Length::px(*px)),
        UiVal::Image(source) => PropValue::ImageSource(source.clone()),
        UiVal::Enum { ty, case } => PropValue::Enum { ty: *ty, case: case.clone() },
        // A null property is treated as absent.
        UiVal::Null => return Ok(None),
        UiVal::List(_) | UiVal::Record(_) => {
            return Err(UiError::validation("UI_BAD_PROP", "property cannot be a list or record"));
        }
    };
    Ok(Some(prop))
}

fn eval_binary(
    op: BinOp,
    left: UiVal,
    right: UiVal,
    span: crate::diagnostics::Span,
) -> UiResult<UiVal> {
    match op {
        BinOp::And => Ok(UiVal::Bool(left.truthy() && right.truthy())),
        BinOp::Or => Ok(UiVal::Bool(left.truthy() || right.truthy())),
        BinOp::Eq => Ok(UiVal::Bool(values_equal(&left, &right))),
        BinOp::Ne => Ok(UiVal::Bool(!values_equal(&left, &right))),
        BinOp::Add => match (&left, &right) {
            (UiVal::Number(a), UiVal::Number(b)) => Ok(UiVal::Number(a + b)),
            (UiVal::Text(_), _) | (_, UiVal::Text(_)) => {
                Ok(UiVal::Text(format!("{}{}", left.to_display(), right.to_display())))
            }
            _ => Err(UiError::validation("UI_TYPE", "cannot add these values").with_span(span)),
        },
        BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Mod => {
            let (UiVal::Number(a), UiVal::Number(b)) = (&left, &right) else {
                return Err(
                    UiError::validation("UI_TYPE", "arithmetic needs numbers").with_span(span)
                );
            };
            let result =
                match op {
                    BinOp::Minus => a - b,
                    BinOp::Times => a * b,
                    BinOp::Divide => {
                        if *b == 0.0 {
                            return Err(UiError::validation("UI_DIV_ZERO", "division by zero")
                                .with_span(span));
                        }
                        a / b
                    }
                    BinOp::Mod => {
                        if *b == 0.0 {
                            return Err(UiError::validation("UI_DIV_ZERO", "modulo by zero")
                                .with_span(span));
                        }
                        a % b
                    }
                    _ => unreachable!(),
                };
            Ok(UiVal::Number(result))
        }
        BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => {
            let (UiVal::Number(a), UiVal::Number(b)) = (&left, &right) else {
                return Err(
                    UiError::validation("UI_TYPE", "comparison needs numbers").with_span(span)
                );
            };
            let result = match op {
                BinOp::Gt => a > b,
                BinOp::Lt => a < b,
                BinOp::Ge => a >= b,
                BinOp::Le => a <= b,
                _ => unreachable!(),
            };
            Ok(UiVal::Bool(result))
        }
    }
}

fn values_equal(left: &UiVal, right: &UiVal) -> bool {
    match (left, right) {
        (UiVal::Number(a), UiVal::Number(b)) => a == b,
        (UiVal::Text(a), UiVal::Text(b)) => a == b,
        (UiVal::Bool(a), UiVal::Bool(b)) => a == b,
        (UiVal::Null, UiVal::Null) => true,
        _ => false,
    }
}

fn format_number(number: f64) -> String {
    if number.fract() == 0.0 && number.is_finite() {
        format!("{}", number as i64)
    } else {
        format!("{number}")
    }
}
