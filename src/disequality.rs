use std::{collections::HashSet, fmt::Display};

use crate::{
    multiset::Multiset,
    polynomial::{Monomial, Polynomial},
    substitution::Substitution,
    term::Term,
};

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
        let mut left = Multiset::from(self.left);
        let mut right = Multiset::from(self.right);

        for (monomial, amount) in left.amount_iter_mut() {
            if let Some(right_amount) = right.amount_mut_by_ref(monomial) {
                if amount > right_amount {
                    *amount -= *right_amount;
                    *right_amount = 0;
                } else {
                    *right_amount -= *amount;
                    *amount = 0;
                }
            }
        }

        Self {
            left: Polynomial::from(left),
            right: Polynomial::from(right),
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = &u32> {
        let unique_variables: HashSet<&u32> = self
            .left
            .variables()
            .chain(self.right.variables())
            .collect();
        unique_variables.into_iter()
    }

    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        let self_reduced = self.clone().reduce();
        let other_reduced = other.clone().reduce();

        self_reduced.left == other_reduced.left && self_reduced.right == other_reduced.right
            || self_reduced.left == other_reduced.right && self_reduced.right == other_reduced.left
    }

    pub fn at_variable_zero(&self, variable: u32) -> Self {
        Self {
            left: self.left.at_variable_zero(variable),
            right: self.right.at_variable_zero(variable),
        }
    }

    pub fn into_at_variable_plus_one(self, variable: u32) -> Self {
        Self {
            left: self.left.into_at_variable_plus_one(variable),
            right: self.right.into_at_variable_plus_one(variable),
        }
    }

    pub fn is_strictly_monomially_comparable(&self) -> bool {
        self.left.is_strictly_monomially_comparable_to(&self.right)
    }

    pub fn has_zero_root(&self) -> bool {
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
