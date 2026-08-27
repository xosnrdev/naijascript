//! Typed UI property values.
//!
//! These are the only value shapes allowed to cross the renderer boundary. They
//! are intentionally closed: a renderer can never receive raw HTML, selectors,
//! code, or native objects. See `docs/spec/UI_IR_PROTOCOL.md`.

use std::fmt;

/// An sRGB color with straight (non-premultiplied) alpha.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    /// Alpha in `0.0..=1.0`.
    pub a: f64,
}

impl Color {
    #[must_use]
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 1.0 }
    }

    #[must_use]
    pub const fn rgba(r: u8, g: u8, b: u8, a: f64) -> Self {
        Self { r, g, b, a }
    }
}

/// A resolution-independent length. The MVP supports logical pixels only.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Length {
    /// Logical pixels: CSS px on web, dp on Compose, points on SwiftUI.
    LogicalPixels(f64),
}

impl Length {
    #[must_use]
    pub const fn px(value: f64) -> Self {
        Length::LogicalPixels(value)
    }
}

/// A validated, non-executable image reference.
#[derive(Debug, Clone, PartialEq)]
pub enum ImageSource {
    /// A packaged, manifest-declared asset by name.
    Asset(String),
}

/// A closed set of enumerated UI property types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumType {
    Alignment,
    Axis,
    ImageFit,
}

impl EnumType {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            EnumType::Alignment => "Alignment",
            EnumType::Axis => "Axis",
            EnumType::ImageFit => "ImageFit",
        }
    }

    /// Returns true when `case` is a valid member of this enum type.
    #[must_use]
    pub fn accepts(self, case: &str) -> bool {
        match self {
            EnumType::Alignment => {
                matches!(case, "start" | "center" | "end" | "stretch")
            }
            EnumType::Axis => matches!(case, "vertical" | "horizontal"),
            EnumType::ImageFit => matches!(case, "contain" | "cover" | "fill"),
        }
    }
}

/// A typed property value carried in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum PropValue {
    Text(String),
    /// Always finite: NaN and infinities are rejected during construction.
    Number(f64),
    Boolean(bool),
    Color(Color),
    Length(Length),
    ImageSource(ImageSource),
    Key(String),
    Enum {
        ty: EnumType,
        case: String,
    },
}

impl PropValue {
    /// The renderer-facing type tag for diagnostics.
    #[must_use]
    pub const fn type_name(&self) -> &'static str {
        match self {
            PropValue::Text(_) => "Text",
            PropValue::Number(_) => "Number",
            PropValue::Boolean(_) => "Boolean",
            PropValue::Color(_) => "Color",
            PropValue::Length(_) => "Length",
            PropValue::ImageSource(_) => "ImageSource",
            PropValue::Key(_) => "Key",
            PropValue::Enum { .. } => "Enum",
        }
    }

    /// Approximate encoded text weight for the `max_text_bytes` limit.
    #[must_use]
    pub fn text_weight(&self) -> usize {
        match self {
            PropValue::Text(s) | PropValue::Key(s) => s.len(),
            PropValue::ImageSource(ImageSource::Asset(name)) => name.len(),
            PropValue::Enum { case, .. } => case.len(),
            _ => 0,
        }
    }
}

impl fmt::Display for PropValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropValue::Text(s) | PropValue::Key(s) => write!(f, "{s}"),
            PropValue::Number(n) => write!(f, "{n}"),
            PropValue::Boolean(b) => write!(f, "{b}"),
            PropValue::Color(c) => write!(f, "rgba({},{},{},{})", c.r, c.g, c.b, c.a),
            PropValue::Length(Length::LogicalPixels(px)) => write!(f, "{px}px"),
            PropValue::ImageSource(ImageSource::Asset(name)) => write!(f, "asset:{name}"),
            PropValue::Enum { ty, case } => write!(f, "{}.{}", ty.as_str(), case),
        }
    }
}
