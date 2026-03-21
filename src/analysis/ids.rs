//! Stable identifiers used by the analysis layer.

/// Identifier for a lexical scope.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

/// Identifier for a function body.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

/// Identifier for a local declaration.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

/// Identifier for a CFG basic block.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

/// Identifier for a lowered analysis op.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpId(pub u32);

/// Identifier for a statement node inside the analysis graph.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub u32);

/// Sentinel used only while the root scope is being wired up.
pub const INVALID_SCOPE_ID: ScopeId = ScopeId(u32::MAX);
