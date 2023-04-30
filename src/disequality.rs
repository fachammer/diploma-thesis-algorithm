use std::fmt::Display;

use crate::{multiset::Multiset, polynomial::Polynomial, substitution::Substitution, term::Term};

#[derive(PartialEq, Eq, Clone)]
pub struct PolynomialDisequality {
    pub left: Polynomial,
    pub right: Polynomial,
}

impl PolynomialDisequality {
    pub fn from_polynomials_reduced(left: Polynomial, right: Polynomial) -> Self {
        let disequality = PolynomialDisequality { left, right };
        disequality.reduce()
    }

    pub fn reduce(self) -> Self {
        let left = Multiset::from(self.left);
        let right = Multiset::from(self.right);
        let left_reduced_monomials = left.clone().subtract(right.clone());
        let right_reduced_monomials = right.subtract(left);

        Self {
            left: Polynomial::from(left_reduced_monomials),
            right: Polynomial::from(right_reduced_monomials),
        }
    }

    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        let self_reduced = self.clone().reduce();
        let other_reduced = other.clone().reduce();

        self_reduced.left == other_reduced.left && self_reduced.right == other_reduced.right
            || self_reduced.left == other_reduced.right && self_reduced.right == other_reduced.left
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TermDisequality {
    left: Term,
    right: Term,
}

impl TermDisequality {
    pub fn from_terms<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        TermDisequality {
            left: left.into(),
            right: right.into(),
        }
    }

    pub fn left(&self) -> &Term {
        &self.left
    }

    pub fn right(&self) -> &Term {
        &self.right
    }

    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        PolynomialDisequality::from(self.clone())
            .is_equivalent_to(&PolynomialDisequality::from(other.clone()))
    }

    pub fn substitute(&self, substitution: &Substitution) -> Self {
        Self {
            left: self.left.substitute(substitution),
            right: self.right.substitute(substitution),
        }
    }
}

impl From<PolynomialDisequality> for TermDisequality {
    fn from(p: PolynomialDisequality) -> Self {
        Self {
            left: p.left.into(),
            right: p.right.into(),
        }
    }
}

impl From<TermDisequality> for PolynomialDisequality {
    fn from(t: TermDisequality) -> Self {
        Self {
            left: t.left.into(),
            right: t.right.into(),
        }
    }
}

impl Display for TermDisequality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} != {}", self.left, self.right)
    }
}
