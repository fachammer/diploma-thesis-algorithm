use std::{collections::HashSet, fmt::Display};

use crate::{polynomial::Polynomial, substitution::Substitution};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Term {
    Variable(u32),
    Zero,
    S(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
}

impl Term {
    pub fn free_variables(&self) -> HashSet<u32> {
        match self {
            Term::Variable(v) => HashSet::from_iter(vec![*v]),
            Term::Zero => HashSet::new(),
            Term::S(t) => t.free_variables(),
            Term::Add(t, u) | Term::Mul(t, u) => t
                .free_variables()
                .union(&u.free_variables())
                .cloned()
                .collect(),
        }
    }

    pub fn sum_of_terms(mut terms: Vec<Self>) -> Self {
        let mut sum_term = Self::Zero;
        terms.reverse();
        for term in terms.into_iter() {
            sum_term = Self::Add(term.into(), sum_term.into());
        }

        sum_term
    }

    pub fn product_of_terms(terms: Vec<Self>) -> Self {
        let Some((first, rest)) = terms.split_first() else {
            return Self::S(Self::Zero.into());
        };

        Self::Mul(
            first.clone().into(),
            Self::product_of_terms(rest.into()).into(),
        )
    }

    pub fn substitute(&self, substitution: &Substitution) -> Self {
        match self {
            Term::Variable(v) => substitution.get(v).cloned().unwrap_or(Term::Variable(*v)),
            Term::Zero => Term::Zero,
            Term::S(t) => Term::S(t.substitute(substitution).into()),
            Term::Add(t, u) => Term::Add(
                t.substitute(substitution).into(),
                u.substitute(substitution).into(),
            ),
            Term::Mul(t, u) => Term::Mul(
                t.substitute(substitution).into(),
                u.substitute(substitution).into(),
            ),
        }
    }
}

struct PolynomialDisplay<'a>(&'a Term);

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        PolynomialDisplay(self).fmt(f)
    }
}

impl<'a> Display for PolynomialDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Polynomial::from(self.0.clone()).fmt(f)
    }
}
