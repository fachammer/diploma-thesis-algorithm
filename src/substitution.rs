use std::collections::HashMap;

use crate::term::Term;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Substitution(HashMap<u32, Term>);

impl Substitution {
    pub(crate) fn new() -> Self {
        Self(HashMap::new())
    }

    pub(crate) fn get(&self, variable: &u32) -> Option<&Term> {
        self.0.get(variable)
    }

    pub fn compose(mut self, other: Self) -> Self {
        for (v, t) in other.0.iter() {
            self.0
                .entry(*v)
                .and_modify(|term| *term = term.substitute(&other))
                .or_insert_with(|| t.clone());
        }
        self
    }
}

impl Default for Substitution {
    fn default() -> Self {
        Self::new()
    }
}

impl FromIterator<(u32, Term)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (u32, Term)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Extend<(u32, Term)> for Substitution {
    fn extend<T: IntoIterator<Item = (u32, Term)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.0.insert(k, v);
        }
    }
}
