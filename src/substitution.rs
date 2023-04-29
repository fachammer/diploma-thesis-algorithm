use std::collections::HashMap;

use crate::term::Term;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Substitution(HashMap<u32, Term>);

impl Substitution {
    pub fn new() -> Self {
        Self(HashMap::new())
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

    pub fn get(&self, variable: &u32) -> Option<&Term> {
        self.0.get(variable)
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

#[cfg(test)]
mod test {
    use crate::term::Term;

    use super::Substitution;

    #[test]
    fn compose_substitutions() {
        let left = Substitution::from_iter(vec![
            (0, Term::S(Term::Zero.into())),
            (1, Term::S(Term::Variable(1).into())),
        ]);
        let right = Substitution::from_iter(vec![(0, Term::Zero), (1, Term::S(Term::Zero.into()))]);

        assert_eq!(
            left.compose(right),
            Substitution::from_iter(vec![
                (0, Term::S(Term::Zero.into())),
                (1, Term::S(Term::S(Term::Zero.into()).into())),
            ])
        )
    }
}
