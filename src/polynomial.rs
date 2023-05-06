use std::{
    fmt::Display,
    ops::{Add, Mul, MulAssign},
};

use crate::{
    disequality::PolynomialDisequality, multiset::Multiset, substitution::Substitution, Term,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Monomial(Multiset<u32>);

impl Monomial {
    pub fn one() -> Self {
        Self(Multiset::new())
    }

    pub fn from_variable(v: u32, exponent: u32) -> Self {
        Self(Multiset::from_iter(vec![(v, exponent)]))
    }

    pub fn from_exponents<T: IntoIterator<Item = (u32, u32)>>(exponents: T) -> Self {
        Self(Multiset::from_iter(exponents))
    }

    pub fn into_variables_iter(self) -> impl Iterator<Item = u32> {
        self.0.into_iter()
    }

    pub fn strictly_divides(&self, other: &Monomial) -> bool {
        self.0.is_multisubset_of(&other.0) && self != other
    }

    pub fn non_zero_variables_iter(&self) -> impl Iterator<Item = &u32> {
        self.0.support()
    }

    pub fn exponent(&self, variable: &u32) -> u32 {
        self.0.amount(variable)
    }

    fn has_variable(&self, variable: &u32) -> bool {
        self.0.contains(variable)
    }

    fn without_variable(mut self, variable: u32) -> Self {
        self.0.remove_all(&variable);
        self
    }
}

impl Mul for Monomial {
    type Output = Monomial;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self.0.extend(rhs.0.into_amount_iter());
        self
    }
}

impl MulAssign for Monomial {
    fn mul_assign(&mut self, rhs: Self) {
        self.0.extend(rhs.0.into_amount_iter())
    }
}

impl PartialOrd for Monomial {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Monomial {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut self_variables: Vec<&u32> = self.0.iter().collect();
        let mut other_variables: Vec<&u32> = other.0.iter().collect();
        self_variables.sort_by(|x, y| x.cmp(y).reverse());
        other_variables.sort_by(|x, y| x.cmp(y).reverse());

        self_variables.cmp(&other_variables)
    }
}

impl From<Monomial> for Multiset<u32> {
    fn from(m: Monomial) -> Self {
        m.0
    }
}

impl Display for Monomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<&u32> = self.non_zero_variables_iter().collect();
        non_zero.sort();
        if non_zero.is_empty() {
            return write!(f, "1");
        }

        let strings: Vec<String> = non_zero
            .into_iter()
            .map(|variable| match self.exponent(variable) {
                1 => format!("x_{variable}"),
                exponent => format!("x_{variable}^{exponent}"),
            })
            .collect();
        write!(f, "{}", strings.join(""))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Polynomial(Multiset<Monomial>);

impl Polynomial {
    pub fn from_variable(v: u32) -> Self {
        Self(Multiset::from_iter(vec![(
            Monomial::from_variable(v, 1),
            1,
        )]))
    }

    pub fn from_coefficients<T: IntoIterator<Item = (Monomial, u32)>>(coefficients: T) -> Self {
        Self(Multiset::from_iter(coefficients))
    }

    pub fn coefficient(&self, monomial: &Monomial) -> u32 {
        self.0.amount(monomial)
    }

    pub fn at_substitution(&self, substitution: &Substitution) -> Term {
        Term::from(self.clone()).substitute(substitution)
    }

    pub fn is_monomially_smaller_than(&self, other: &Polynomial) -> bool {
        for m_1 in self.0.support().filter(|m| !other.0.contains(m)) {
            if other
                .0
                .support()
                .filter(|m| !self.0.contains(m))
                .all(|m_2| !m_1.strictly_divides(m_2))
            {
                return false;
            }
        }

        true
    }

    pub fn is_strictly_monomially_comparable_to(&self, other: &Polynomial) -> bool {
        self.is_monomially_smaller_than(other) || other.is_monomially_smaller_than(self)
    }

    pub fn predecessor(&self) -> Polynomial {
        let mut polynomial = self.clone();
        let constant_coefficient = polynomial.0.amount_mut(Monomial::one());
        *constant_coefficient = constant_coefficient.saturating_sub(1);
        polynomial
    }

    pub fn non_zero_monomials_iter(&self) -> impl Iterator<Item = &Monomial> {
        self.0.support()
    }

    pub fn at_variable_zero(&self, variable: u32) -> Self {
        let mut monomials = Multiset::with_capacity(self.0.support().count());
        for (monomial, amount) in self.0.amount_iter() {
            if !monomial.has_variable(&variable) {
                *monomials.amount_mut(monomial.clone()) = *amount;
            }
        }
        Self(monomials)
    }

    fn power_of_variable_plus_one(variable: u32, exponent: u32) -> Vec<(Monomial, u32)> {
        let exponent_usize: usize = exponent
            .try_into()
            .expect("we should be on a system with at least 32 bit");
        let mut monomials = Vec::with_capacity(exponent_usize + 1);

        for k in 0..=exponent {
            monomials.push((
                Monomial::from_variable(variable, k),
                Self::binomial_coefficient(exponent, k),
            ));
        }
        monomials
    }

    fn binomial_coefficient(n: u32, k: u32) -> u32 {
        let (numerator, denominator): (u32, u32) = if 2 * k > n {
            (
                (k + 1..=n).into_iter().product(),
                (1..=(n - k)).into_iter().product(),
            )
        } else {
            (
                ((n - k) + 1..=n).into_iter().product(),
                (1..=k).into_iter().product(),
            )
        };
        numerator / denominator
    }

    pub fn into_at_variable_plus_one(self, variable: u32) -> Self {
        let mut monomials = Multiset::with_capacity(5 * self.0.support().count());
        for (monomial, amount) in self.0.into_amount_iter() {
            let variable_exponent = monomial.exponent(&variable);
            if variable_exponent > 0 {
                let mut binomials = Self::power_of_variable_plus_one(variable, variable_exponent);
                let monomial_without_variable = monomial.without_variable(variable);
                for (k, v) in binomials.iter_mut() {
                    *k *= monomial_without_variable.clone();
                    *v *= amount
                }
                monomials.extend(binomials);
            } else {
                *monomials.amount_mut(monomial) += amount;
            }
        }
        Self(monomials)
    }
}

impl From<Polynomial> for Multiset<Monomial> {
    fn from(p: Polynomial) -> Self {
        p.0
    }
}

impl From<Multiset<Monomial>> for Polynomial {
    fn from(monomials: Multiset<Monomial>) -> Self {
        Self(monomials)
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.0.extend(rhs.0.into_amount_iter());
        self
    }
}

impl Add<u32> for Polynomial {
    type Output = Polynomial;

    fn add(mut self, rhs: u32) -> Self::Output {
        self.0.extend([(Monomial::one(), rhs)]);
        self
    }
}

impl Mul for Polynomial {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut output: Multiset<Monomial> = Multiset::new();
        for self_monomial in self.0.support() {
            for other_monomial in rhs.0.support() {
                let product_monomial = self_monomial.clone() * other_monomial.clone();
                *output.amount_mut(product_monomial) +=
                    self.0.amount(self_monomial) * rhs.0.amount(other_monomial);
            }
        }
        Polynomial(output)
    }
}

impl Mul<Polynomial> for u32 {
    type Output = Polynomial;

    fn mul(self, rhs: Polynomial) -> Self::Output {
        Polynomial::from(self) * rhs
    }
}

impl From<u32> for Polynomial {
    fn from(n: u32) -> Self {
        Polynomial(Multiset::from_iter([(Monomial::one(), n)]))
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<&Monomial> = self.non_zero_monomials_iter().collect();
        non_zero.sort_by(|x, y| x.cmp(y).reverse());

        if non_zero.is_empty() {
            return write!(f, "0");
        }
        let strings: Vec<String> = non_zero
            .into_iter()
            .map(|monomial| {
                let amount = self.coefficient(monomial);
                if monomial == &Monomial::one() {
                    format!("{amount}")
                } else {
                    format!("{amount}{monomial}")
                }
            })
            .collect();
        write!(f, "{}", strings.join(" + "))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn monomial_order() {
        let x = || Monomial::from_variable(0, 1);
        let y = || Monomial::from_variable(1, 1);
        assert!(y() < x() * y());
    }

    #[test]
    fn strict_divisibility_satisfied() {
        assert!(Monomial::from_variable(0, 1)
            .strictly_divides(&(Monomial::from_variable(0, 1) * Monomial::from_variable(1, 1))))
    }

    #[test]
    fn strict_divisibility_not_satisfied() {
        assert!(
            !(Monomial::from_variable(0, 1) * Monomial::from_variable(0, 1))
                .strictly_divides(&(Monomial::from_variable(0, 1) * Monomial::from_variable(1, 1)))
        )
    }

    #[test]
    fn eq_polynomials() {
        let p = Polynomial::from_coefficients(vec![(Monomial::one(), 0)].into_iter());
        let q = Polynomial::from_coefficients(vec![].into_iter());
        assert_eq!(p, q)
    }

    #[test]
    fn add_polynomials() {
        let x = || Polynomial::from_variable(0);
        let p = 3 * x() * x() + 2 * x() + 1;
        let q = 2 * x() + 1;
        assert_eq!(p + q, 3 * x() * x() + 4 * x() + 2)
    }

    #[test]
    fn mul_polynomials() {
        let x = || Polynomial::from_variable(0);
        let p = 3 * x() * x() + 2 * x() + 1;
        let q = 2 * x() + 1;
        assert_eq!(p * q, 6 * x() * x() * x() + 7 * x() * x() + 4 * x() + 1)
    }

    #[test]
    fn polynomial_from_term() {
        let t = Term::Add(Term::Variable(0).into(), Term::Variable(0).into());
        let x = Polynomial::from_variable(0);
        assert_eq!(2 * x, t.into());
    }

    #[test]
    fn polynomial_from_mul_term() {
        let t = Term::S(Term::Mul(Term::Variable(0).into(), Term::Variable(0).into()).into());
        let x = || Polynomial::from_variable(0);
        assert_eq!(x() * x() + 1, t.into());
    }

    #[test]
    fn term_from_polynomial() {
        let x = || Polynomial::from_variable(0);
        let p = x() * x() + x() + 1;
        assert_eq!(
            Term::Add(
                Term::Mul(
                    Term::Variable(0).into(),
                    Term::Mul(Term::Variable(0).into(), Term::S(Term::Zero.into()).into()).into()
                )
                .into(),
                Term::Add(
                    Term::Mul(Term::Variable(0).into(), Term::S(Term::Zero.into()).into()).into(),
                    Term::Add(Term::S(Term::Zero.into()).into(), Term::Zero.into()).into()
                )
                .into()
            ),
            p.into()
        );
    }

    #[test]
    fn x_is_monomially_smaller_than_x_squared_plus_one() {
        let x = || Polynomial::from_variable(0);
        assert!(x().is_monomially_smaller_than(&(x() * x() + 1)))
    }

    #[test]
    fn x_and_y_are_incomparable() {
        let x = Polynomial::from_variable(0);
        let y = Polynomial::from_variable(1);
        assert!(!x.is_strictly_monomially_comparable_to(&y))
    }

    #[test]
    fn at_variable_zero() {
        let x = Polynomial::from_variable(0);
        let y = Polynomial::from_variable(1);
        assert_eq!((2 * x * y + 1).at_variable_zero(0), 1.into());
    }

    #[test]
    fn into_at_variable_plus_one() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        assert_eq!(
            (2 * x() * y() + 1).into_at_variable_plus_one(0),
            2 * x() * y() + 1 + (2 * y())
        );
    }
}
