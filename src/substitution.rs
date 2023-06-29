use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    polynomial::{Monomial, Polynomial},
    term::Term,
};

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize, Default)]
pub(crate) struct PolynomialSubstitution(HashMap<u32, Polynomial>);

impl PolynomialSubstitution {
    pub(crate) fn apply_to_polynomial(&self, polynomial: Polynomial) -> Polynomial {
        let mut sum = Polynomial::from(0);
        for (monomial, coefficient) in polynomial.0.into_amount_iter() {
            sum = sum + coefficient * self.apply_to_monomial(monomial);
        }
        sum
    }

    pub(crate) fn apply_to_monomial(&self, monomial: Monomial) -> Polynomial {
        let mut product = Polynomial::from(1);
        for (v, exponent) in monomial.0.into_amount_iter() {
            product = product * self.apply_to_variable(v).pow(exponent);
        }

        product
    }

    pub(crate) fn apply_to_variable(&self, variable: u32) -> Polynomial {
        self.0
            .get(&variable)
            .cloned()
            .unwrap_or_else(|| Polynomial::from_variable(variable))
    }

    pub(crate) fn compose(mut self, other: Self) -> Self {
        for (v, p) in other.0.iter() {
            self.0
                .entry(*v)
                .and_modify(|polynomial| {
                    *polynomial = other.apply_to_polynomial(polynomial.clone())
                })
                .or_insert_with(|| p.clone());
        }
        self
    }
}

impl FromIterator<(u32, Polynomial)> for PolynomialSubstitution {
    fn from_iter<T: IntoIterator<Item = (u32, Polynomial)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[cfg(test)]
mod test {
    use crate::polynomial::{Monomial, Polynomial};

    use super::PolynomialSubstitution;

    #[test]
    fn apply_to_monomial_works() {
        let substitution = PolynomialSubstitution::from_iter([
            (0, Polynomial::from_variable(1)),
            (1, Polynomial::from_variable(0)),
        ]);
        let monomial = Monomial::from_variable(0, 2) * Monomial::from_variable(1, 1);

        assert_eq!(
            substitution.apply_to_monomial(monomial),
            Polynomial::from(Monomial::from_variable(1, 2) * Monomial::from_variable(0, 1))
        );
    }

    #[test]
    fn apply_to_polynomial_works() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        let substitution = PolynomialSubstitution::from_iter([(0, y() + 1), (1, x() + 1)]);
        let polynomial = x() * x() * y();

        assert_eq!(
            substitution.apply_to_polynomial(polynomial),
            (y() * y() + 2 * y() + 1) * (x() + 1),
        );
    }
}
