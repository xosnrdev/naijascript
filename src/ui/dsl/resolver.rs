//! Static checks for a parsed `.nui` [`Program`].
//!
//! The resolver enforces the safety-relevant invariants the evaluator relies on:
//! names resolve, primitives and their properties exist, event handler
//! references point to declared handlers whose event type matches the property
//! slot, and assignments target real state. It collects every diagnostic rather
//! than stopping at the first. See `docs/spec/UI_DSL.md`.

use std::collections::HashSet;

use crate::ui::dsl::ast::{
    Child, Expr, Handler, Param, Program, PropExpr, Screen, StateDecl, Stmt,
};
use crate::ui::error::UiError;
use crate::ui::event::EventKind;
use crate::ui::ir::PrimitiveKind;

/// A property slot's expected role.
enum Slot {
    /// A value property (type checked lightly at evaluation time).
    Value,
    /// An event property bound with `@handler`.
    Event(EventKind),
}

/// Resolves a program, returning all diagnostics found.
pub fn resolve(program: &Program) -> Result<(), Vec<UiError>> {
    let mut errors = Vec::new();

    let mut seen = HashSet::new();
    for screen in &program.screens {
        if !seen.insert(screen.name.clone()) {
            errors
                .push(UiError::compile("NUI0200", "duplicate screen name").with_span(screen.span));
        }
    }
    for component in &program.components {
        if !seen.insert(component.name.clone()) {
            errors.push(
                UiError::compile("NUI0200", "duplicate declaration name").with_span(component.span),
            );
        }
    }

    for screen in &program.screens {
        resolve_screen(screen, &mut errors);
    }
    // Components share the screen shape; validate them the same way.
    for component in &program.components {
        let as_screen = Screen {
            name: component.name.clone(),
            params: component.params.clone(),
            state: component.state.clone(),
            handlers: component.handlers.clone(),
            view: component.view.clone(),
            span: component.span,
        };
        resolve_screen(&as_screen, &mut errors);
    }

    if errors.is_empty() { Ok(()) } else { Err(errors) }
}

fn resolve_screen(screen: &Screen, errors: &mut Vec<UiError>) {
    let mut names: HashSet<String> = HashSet::new();
    for Param { name, .. } in &screen.params {
        if !names.insert(name.clone()) {
            errors.push(UiError::compile("NUI0201", "duplicate parameter").with_span(screen.span));
        }
    }
    for StateDecl { name, .. } in &screen.state {
        if !names.insert(name.clone()) {
            errors.push(UiError::compile("NUI0202", "state name shadows another symbol"));
        }
    }

    let state_names: HashSet<String> =
        screen.state.iter().map(|state| state.name.clone()).collect();

    for state in &screen.state {
        resolve_expr(&state.init, &names, errors);
    }

    let handler_names: HashSet<String> =
        screen.handlers.iter().map(|handler| handler.name.clone()).collect();

    for handler in &screen.handlers {
        resolve_handler(handler, &names, &state_names, errors);
    }

    for child in &screen.view {
        resolve_child(child, &names, &handler_names, screen, errors);
    }
}

fn resolve_handler(
    handler: &Handler,
    outer: &HashSet<String>,
    state_names: &HashSet<String>,
    errors: &mut Vec<UiError>,
) {
    let mut scope = outer.clone();
    scope.insert(handler.event_param.clone());
    for stmt in &handler.body {
        match stmt {
            Stmt::Assign { target, value, span } => {
                if !state_names.contains(target) {
                    errors.push(
                        UiError::compile("NUI0203", "assignment target is not state")
                            .with_span(*span),
                    );
                }
                resolve_expr(value, &scope, errors);
            }
        }
    }
}

fn resolve_child(
    child: &Child,
    scope: &HashSet<String>,
    handlers: &HashSet<String>,
    screen: &Screen,
    errors: &mut Vec<UiError>,
) {
    match child {
        Child::Text { value, .. } => resolve_expr(value, scope, errors),
        Child::Show { cond, then_children, otherwise, .. } => {
            resolve_expr(cond, scope, errors);
            for grandchild in then_children.iter().chain(otherwise) {
                resolve_child(grandchild, scope, handlers, screen, errors);
            }
        }
        Child::Each { item, collection, key, body, .. } => {
            resolve_expr(collection, scope, errors);
            let mut inner = scope.clone();
            inner.insert(item.clone());
            resolve_expr(key, &inner, errors);
            for grandchild in body {
                resolve_child(grandchild, &inner, handlers, screen, errors);
            }
        }
        Child::Element { name, props, key, children, span } => {
            let is_component = screen_component(screen, name);
            let kind = PrimitiveKind::from_str(name);
            if kind.is_none() && !is_component {
                errors.push(
                    UiError::compile("NUI0204", "unknown element or component").with_span(*span),
                );
            }
            if let Some(key_expr) = key {
                resolve_expr(key_expr, scope, errors);
            }
            for prop in props {
                if let Some(kind) = kind {
                    match prop_slot(kind, &prop.name) {
                        Some(Slot::Event(expected)) => match &prop.value {
                            PropExpr::HandlerRef { name, span } => {
                                if !handlers.contains(name) {
                                    errors.push(
                                        UiError::compile("NUI0205", "unknown handler reference")
                                            .with_span(*span),
                                    );
                                } else if let Some(handler) =
                                    screen.handlers.iter().find(|handler| &handler.name == name)
                                {
                                    let actual = event_kind(handler);
                                    if actual != expected {
                                        errors.push(
                                            UiError::compile(
                                                "NUI0206",
                                                "handler event type does not match property",
                                            )
                                            .with_span(*span),
                                        );
                                    }
                                }
                            }
                            PropExpr::Expr(expr) => {
                                errors.push(
                                    UiError::compile(
                                        "NUI0207",
                                        "event property expects an @handler reference",
                                    )
                                    .with_span(expr.span()),
                                );
                            }
                        },
                        Some(Slot::Value) => match &prop.value {
                            PropExpr::Expr(expr) => resolve_expr(expr, scope, errors),
                            PropExpr::HandlerRef { span, .. } => {
                                errors.push(
                                    UiError::compile(
                                        "NUI0208",
                                        "value property cannot take a handler reference",
                                    )
                                    .with_span(*span),
                                );
                            }
                        },
                        None => {
                            errors.push(
                                UiError::compile("NUI0209", "unknown property for this element")
                                    .with_span(prop.span),
                            );
                        }
                    }
                } else if let PropExpr::Expr(expr) = &prop.value {
                    // Component prop: resolve the expression; type binding is
                    // validated when component evaluation lands.
                    resolve_expr(expr, scope, errors);
                }
            }
            for grandchild in children {
                resolve_child(grandchild, scope, handlers, screen, errors);
            }
        }
    }
}

fn resolve_expr(expr: &Expr, scope: &HashSet<String>, errors: &mut Vec<UiError>) {
    match expr {
        Expr::Number(..)
        | Expr::Str(..)
        | Expr::Bool(..)
        | Expr::Null(..)
        | Expr::Length(..)
        | Expr::EnumLit { .. } => {}
        Expr::Var(name, span) => {
            if !scope.contains(name) {
                errors.push(UiError::compile("NUI0210", "unknown name").with_span(*span));
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            resolve_expr(lhs, scope, errors);
            resolve_expr(rhs, scope, errors);
        }
        Expr::Unary { expr, .. } => resolve_expr(expr, scope, errors),
        Expr::Member { object, .. } => resolve_expr(object, scope, errors),
        Expr::Index { object, index, .. } => {
            resolve_expr(object, scope, errors);
            resolve_expr(index, scope, errors);
        }
        Expr::ColorCall { args, .. } => {
            for arg in args {
                resolve_expr(arg, scope, errors);
            }
        }
        Expr::AssetCall { arg, .. } => resolve_expr(arg, scope, errors),
    }
}

fn screen_component(screen: &Screen, name: &str) -> bool {
    // Components are resolved by name at the program level; treat any
    // capitalized non-primitive as a potential component reference.
    let _ = screen;
    name.chars().next().is_some_and(char::is_uppercase)
}

const fn event_kind(handler: &Handler) -> EventKind {
    use crate::ui::dsl::ast::EventTy;
    match handler.event_ty {
        EventTy::Tap => EventKind::Tap,
        EventTy::Change => EventKind::Change,
        EventTy::Submit => EventKind::Submit,
        EventTy::Focus => EventKind::Focus,
    }
}

fn prop_slot(kind: PrimitiveKind, prop: &str) -> Option<Slot> {
    // Common layout properties available on every element.
    match prop {
        "width" | "height" | "padding" | "background" | "align" => return Some(Slot::Value),
        _ => {}
    }
    let slot = match (kind, prop) {
        (PrimitiveKind::Stack, "axis" | "spacing") => Slot::Value,
        (PrimitiveKind::Row, "spacing") => Slot::Value,
        (PrimitiveKind::Scroll, "axis") => Slot::Value,
        (PrimitiveKind::Spacer, "size") => Slot::Value,
        (PrimitiveKind::Label, "color" | "size") => Slot::Value,
        (PrimitiveKind::Button, "label" | "enabled") => Slot::Value,
        (PrimitiveKind::Button, "onTap") => Slot::Event(EventKind::Tap),
        (PrimitiveKind::Input, "value" | "placeholder") => Slot::Value,
        (PrimitiveKind::Input, "onChange") => Slot::Event(EventKind::Change),
        (PrimitiveKind::Input, "onSubmit") => Slot::Event(EventKind::Submit),
        (PrimitiveKind::Input, "onFocus") => Slot::Event(EventKind::Focus),
        (PrimitiveKind::Image, "source" | "fit") => Slot::Value,
        _ => return None,
    };
    Some(slot)
}
