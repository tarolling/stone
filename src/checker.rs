//! Type checker that validates a stone AST before it runs.
//!
//! It currently accepts every program.

use crate::ast::Mod;

pub struct TypeChecker;

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check(&mut self, _ast: &Mod) -> Result<(), Box<dyn std::error::Error>> {
        Ok(())
    }
}
