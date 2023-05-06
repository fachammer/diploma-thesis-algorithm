use std::{collections::HashSet, fmt::Display};

use crate::{
    multiset::Multiset,
    polynomial::{Monomial, Polynomial},
    substitution::Substitution,
    term::Term,
};

#[derive(PartialEq, Eq, Clone)]
pub(crate) struct PolynomialDisequality {
    pub(crate) left: Polynomial,
    pub(crate) right: Polynomial,
}

trait MinMax
where
    Self: Sized,
{
    fn min_max(self, other: Self) -> (Self, Self);
}

impl<T: Ord> MinMax for T {
    fn min_max(self, other: Self) -> (Self, Self) {
        if self <= other {
            (self, other)
        } else {
            (other, self)
        }
    }
}

impl PolynomialDisequality {
    pub(crate) fn from_polynomials_reduced(left: Polynomial, right: Polynomial) -> Self {
        let disequality = PolynomialDisequality { left, right };
        disequality.reduce()
    }

    pub(crate) fn reduce(self) -> Self {
        let mut left = Multiset::from(self.left);
        let mut right = Multiset::from(self.right);

        for (monomial, left_amount) in left.amount_iter_mut() {
            if let Some(right_amount) = right.amount_mut_by_ref(monomial) {
                let (smaller, larger) = left_amount.min_max(right_amount);
                *larger -= *smaller;
                *smaller = 0;
            }
        }

        Self {
            left: Polynomial::from(left),
            right: Polynomial::from(right),
        }
    }

    pub(crate) fn variables(&self) -> impl Iterator<Item = &u32> {
        let unique_variables: HashSet<&u32> = self
            .left
            .variables()
            .chain(self.right.variables())
            .collect();
        unique_variables.into_iter()
    }

    pub(crate) fn at_variable_zero(&self, variable: u32) -> Self {
        Self {
            left: self.left.at_variable_zero(variable),
            right: self.right.at_variable_zero(variable),
        }
    }

    pub(crate) fn into_at_variable_plus_one(self, variable: u32) -> Self {
        Self {
            left: self.left.into_at_variable_plus_one(variable),
            right: self.right.into_at_variable_plus_one(variable),
        }
    }

    pub(crate) fn is_strictly_monomially_comparable(&self) -> bool {
        self.left.is_strictly_monomially_comparable_to(&self.right)
    }

    pub(crate) fn has_zero_root(&self) -> bool {
        self.left.coefficient(&Monomial::one()) == 0
            && self.right.coefficient(&Monomial::one()) == 0
    }

    pub(crate) fn is_in_successor_non_zero_form(&self) -> bool {
        self.left.coefficient(&Monomial::one()) > 0 && self.right == 0.into()
            || self.right.coefficient(&Monomial::one()) > 0 && self.left == 0.into()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TermDisequality {
    left: Term,
    right: Term,
}

impl TermDisequality {
    pub(crate) fn from_terms<L: Into<Term>, R: Into<Term>>(left: L, right: R) -> Self {
        TermDisequality {
            left: left.into(),
            right: right.into(),
        }
    }

    pub(crate) fn left(&self) -> &Term {
        &self.left
    }

    pub(crate) fn right(&self) -> &Term {
        &self.right
    }

    pub(crate) fn substitute(&self, substitution: &Substitution) -> Self {
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

#[cfg(test)]
mod test {
    use crate::polynomial::Polynomial;

    use super::PolynomialDisequality;

    #[test]
    fn reduce() {
        let left = Polynomial::from_variable(0);
        let right = 2 * Polynomial::from_variable(0);

        let PolynomialDisequality { left, right } =
            PolynomialDisequality::from_polynomials_reduced(left, right);
        assert_eq!(left, 0.into());
        assert_eq!(right, Polynomial::from_variable(0));
    }
}
