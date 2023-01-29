use crate::definition::Definition;
use oal_syntax::atom::Ident;
use std::collections::HashMap;

pub type Scope = HashMap<Ident, Definition>;

#[derive(Debug)]
pub struct Env(Vec<Scope>);

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Env(vec![Scope::new()])
    }

    pub fn declare(&mut self, n: Ident, defn: Definition) {
        self.0.last_mut().unwrap().insert(n, defn);
    }

    pub fn lookup(&self, n: &Ident) -> Option<&Definition> {
        self.0
            .iter()
            .rev()
            .map(|s| s.get(n))
            .skip_while(Option::is_none)
            .map(|s| s.unwrap())
            .next()
    }

    pub fn open(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn close(&mut self) {
        self.0.pop();
    }
}
