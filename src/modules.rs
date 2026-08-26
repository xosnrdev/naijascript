//! Host-supplied source lookup for embeddable multi-source applications.
//!
//! NaijaScript deliberately does not resolve paths or read files in the core
//! runtime. Hosts can use this abstraction to name and provide multiple source
//! units, then run each source through the normal lexer/parser pipeline.

/// One named NaijaScript source unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleSource<'src> {
    pub name: &'src str,
    pub source: &'src str,
}

/// Source lookup implemented by an embedding host.
pub trait ModuleProvider<'src> {
    fn get(&self, name: &str) -> Option<ModuleSource<'src>>;
}

/// A small provider over an existing host-owned slice.
pub struct SliceModuleProvider<'src> {
    modules: &'src [ModuleSource<'src>],
}

impl<'src> SliceModuleProvider<'src> {
    #[must_use]
    pub const fn new(modules: &'src [ModuleSource<'src>]) -> Self {
        Self { modules }
    }
}

impl<'src> ModuleProvider<'src> for SliceModuleProvider<'src> {
    fn get(&self, name: &str) -> Option<ModuleSource<'src>> {
        self.modules.iter().copied().find(|module| module.name == name)
    }
}
