//! Type Checker
//!

use crate::types::Mod;

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check(&mut self, ast: &Mod) -> Result<(), Box<dyn std::error::Error>> {
        Ok(())
    }
}
