use std::{
    collections::HashSet,
    fmt::Display,
    ops::{Add, Mul, MulAssign},
};

use serde::{Deserialize, Serialize};

use crate::multiset::{hash_map::MultisetHashMap, Multiset};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub(crate) struct Monomial(pub(crate) Multiset<u32>);

impl Monomial {
    pub(crate) fn one() -> Self {
        Self(Multiset::new())
    }

    pub(crate) fn from_variable(v: u32, exponent: u32) -> Self {
        Self(Multiset::from_iter(vec![(v, exponent)]))
    }

    pub(crate) fn into_variables_iter(self) -> impl Iterator<Item = u32> {
        self.0.into_iter()
    }

    pub(crate) fn strictly_divides(&self, other: &Monomial) -> bool {
        self.0.is_multisubset_of(&other.0) && self != other
    }

    pub(crate) fn non_zero_variables_iter(&self) -> impl Iterator<Item = &u32> {
        self.0.support()
    }

    pub(crate) fn exponent(&self, variable: &u32) -> u32 {
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
        match self.0.len().cmp(&other.0.len()) {
            std::cmp::Ordering::Equal => {
                let mut self_variables: Vec<(&u32, &u32)> = self.0.amount_iter().collect();
                let mut other_variables: Vec<(&u32, &u32)> = other.0.amount_iter().collect();
                self_variables.sort_unstable();
                other_variables.sort_unstable();
                self_variables.cmp(&other_variables).reverse()
            }
            len_order => len_order,
        }
    }
}

impl From<Monomial> for Multiset<u32> {
    fn from(m: Monomial) -> Self {
        m.0
    }
}

fn default_variable_display(variable: u32) -> String {
    format!("x_{variable}")
}

impl Display for Monomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        MonomialDisplay {
            monomial: self,
            variable_mapping: &default_variable_display,
            exponent_display_style: ExponentDisplayStyle::UnicodeSuperscript,
        }
        .fmt(f)
    }
}

struct MonomialDisplay<'a, 'b, V>
where
    V: Fn(u32) -> String,
{
    monomial: &'a Monomial,
    variable_mapping: &'b V,
    exponent_display_style: ExponentDisplayStyle,
}

impl<'a, 'b, V> Display for MonomialDisplay<'a, 'b, V>
where
    V: Fn(u32) -> String,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<&u32> = self.monomial.non_zero_variables_iter().collect();
        non_zero.sort();
        if non_zero.is_empty() {
            return write!(f, "1");
        }

        let strings: Vec<String> = non_zero
            .into_iter()
            .map(|variable| match self.monomial.exponent(variable) {
                1 => (self.variable_mapping)(*variable),
                exponent => format!(
                    "{}{}",
                    (self.variable_mapping)(*variable),
                    SuperscriptDisplay {
                        number: exponent,
                        exponent_display_style: self.exponent_display_style
                    }
                ),
            })
            .collect();
        write!(f, "{}", strings.join(""))
    }
}

#[derive(Clone, Copy)]
pub(crate) enum ExponentDisplayStyle {
    UnicodeSuperscript,
    SuperscriptTag,
}

fn format_unicode_superscript(f: &mut std::fmt::Formatter<'_>, number: u32) -> std::fmt::Result {
    for character in number.to_string().chars() {
        write!(
            f,
            "{}",
            to_unicode_superscript(character).expect("all digits have superscrips")
        )?;
    }
    Ok(())
}

fn to_unicode_superscript(c: char) -> Result<char, String> {
    let superscript = match c {
        '0' => '⁰',
        '1' => '¹',
        '2' => '²',
        '3' => '³',
        '4' => '⁴',
        '5' => '⁵',
        '6' => '⁶',
        '7' => '⁷',
        '8' => '⁸',
        '9' => '⁹',
        _ => return Err(format!("character '{}' not supported", c)),
    };
    Ok(superscript)
}

fn format_superscript_tag(f: &mut std::fmt::Formatter<'_>, number: u32) -> std::fmt::Result {
    write!(f, "<sup>{number}</sup>")
}

struct SuperscriptDisplay {
    number: u32,
    exponent_display_style: ExponentDisplayStyle,
}

impl Display for SuperscriptDisplay {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.exponent_display_style {
            ExponentDisplayStyle::UnicodeSuperscript => format_unicode_superscript(f, self.number),
            ExponentDisplayStyle::SuperscriptTag => format_superscript_tag(f, self.number),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct Polynomial(pub(crate) MultisetHashMap<Monomial>);

impl Polynomial {
    pub(crate) fn from_variable(v: u32) -> Self {
        Self(MultisetHashMap::from_iter(vec![(
            Monomial::from_variable(v, 1),
            1,
        )]))
    }

    pub(crate) fn coefficient(&self, monomial: &Monomial) -> u32 {
        self.0.amount(monomial)
    }

    pub(crate) fn is_monomially_smaller_than(&self, other: &Polynomial) -> bool {
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

    pub(crate) fn is_strictly_monomially_comparable_to(&self, other: &Polynomial) -> bool {
        self.is_monomially_smaller_than(other) || other.is_monomially_smaller_than(self)
    }

    pub(crate) fn non_zero_monomials_amount_iter(&self) -> impl Iterator<Item = (&Monomial, &u32)> {
        self.0.amount_iter()
    }

    pub(crate) fn at_variable_zero(&self, variable: u32) -> Self {
        let mut monomials = MultisetHashMap::with_capacity(self.0.support().count());
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
            ((k + 1..=n).product(), (1..=(n - k)).product())
        } else {
            (((n - k) + 1..=n).product(), (1..=k).product())
        };
        numerator / denominator
    }

    pub(crate) fn into_at_variable_plus_one(self, variable: u32) -> Self {
        let mut monomials = MultisetHashMap::with_capacity(5 * self.0.support().count());
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

    pub(crate) fn variables(&self) -> impl Iterator<Item = &u32> {
        let unique_variables: HashSet<&u32> =
            self.0.support().flat_map(|m| m.0.support()).collect();
        unique_variables.into_iter()
    }
}

impl From<Polynomial> for MultisetHashMap<Monomial> {
    fn from(p: Polynomial) -> Self {
        p.0
    }
}

impl From<MultisetHashMap<Monomial>> for Polynomial {
    fn from(monomials: MultisetHashMap<Monomial>) -> Self {
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
        let mut output: MultisetHashMap<Monomial> = MultisetHashMap::new();
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
        Polynomial(MultisetHashMap::from_iter([(Monomial::one(), n)]))
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        PolynomialDisplay {
            polynomial: self,
            variable_mapping: &default_variable_display,
            number_of_largest_monomials: 5,
            number_of_smallest_monomials: 5,
            exponent_display_style: ExponentDisplayStyle::UnicodeSuperscript,
        }
        .fmt(f)
    }
}

pub(crate) struct PolynomialDisplay<'a, 'b, V>
where
    V: Fn(u32) -> String,
{
    pub(crate) polynomial: &'a Polynomial,
    pub(crate) variable_mapping: &'b V,
    pub(crate) number_of_largest_monomials: usize,
    pub(crate) number_of_smallest_monomials: usize,
    pub(crate) exponent_display_style: ExponentDisplayStyle,
}

impl<'a, 'b, V> Display for PolynomialDisplay<'a, 'b, V>
where
    V: Fn(u32) -> String,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<(&Monomial, &u32)> =
            self.polynomial.non_zero_monomials_amount_iter().collect();

        if non_zero.len() > self.number_of_largest_monomials + self.number_of_smallest_monomials {
            let (small_monomials, _, larger) =
                non_zero.select_nth_unstable_by_key(self.number_of_smallest_monomials, |a| a.0);
            small_monomials.sort_by(|a, b| a.cmp(b).reverse());

            let small_monomials: Vec<String> = small_monomials
                .iter_mut()
                .map(|(monomial, &amount)| {
                    if monomial == &&Monomial::one() {
                        format!("{amount}")
                    } else if amount == 1 {
                        format!(
                            "{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    } else {
                        format!(
                            "{amount}{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    }
                })
                .collect();

            let (large_monomials, monomial, _) = larger
                .select_nth_unstable_by(self.number_of_largest_monomials - 1, |a, b| {
                    a.cmp(b).reverse()
                });
            let mut large_monomials = Vec::from(large_monomials);
            large_monomials.push(*monomial);
            large_monomials.sort_by(|a, b| a.cmp(b).reverse());

            let large_monomials: Vec<String> = large_monomials
                .iter_mut()
                .map(|(monomial, &amount)| {
                    if monomial == &&Monomial::one() {
                        format!("{amount}")
                    } else if amount == 1 {
                        format!(
                            "{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    } else {
                        format!(
                            "{amount}{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    }
                })
                .collect();
            write!(
                f,
                "{} + ... + {}",
                large_monomials.join(" + "),
                small_monomials.join(" + ")
            )
        } else {
            non_zero.sort_by(|x, y| x.cmp(y).reverse());

            if non_zero.is_empty() {
                return write!(f, "0");
            }
            let strings: Vec<String> = non_zero
                .into_iter()
                .map(|(monomial, &amount)| {
                    if monomial == &Monomial::one() {
                        format!("{amount}")
                    } else if amount == 1 {
                        format!(
                            "{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    } else {
                        format!(
                            "{amount}{}",
                            MonomialDisplay {
                                monomial,
                                variable_mapping: self.variable_mapping,
                                exponent_display_style: self.exponent_display_style,
                            }
                        )
                    }
                })
                .collect();
            write!(f, "{}", strings.join(" + "))
        }
    }
}

#[cfg(test)]
mod test {
    use crate::term::Term;

    use super::*;

    #[test]
    fn monomial_order() {
        let x = || Monomial::from_variable(0, 1);
        let y = || Monomial::from_variable(1, 1);
        assert!(y() < x() * y());
    }

    #[test]
    fn smaller_variables_are_larger() {
        let x = || Monomial::from_variable(0, 1);
        let y = || Monomial::from_variable(1, 1);
        assert!(x() > y());
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
        let p = Polynomial(MultisetHashMap::from_iter([(Monomial::one(), 0)]));
        let q = Polynomial(MultisetHashMap::from_iter([]));
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
