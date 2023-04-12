use std::{
    ops::{Add, Mul},
    vec,
};

#[derive(PartialEq, Eq, Debug, Clone)]
enum Term {
    Variable,
    Zero,
    S(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
}

impl From<Term> for Polynomial {
    fn from(t: Term) -> Self {
        match t {
            Term::Variable => Polynomial(vec![0, 1]),
            Term::Zero => Polynomial(vec![]),
            Term::S(u) => Polynomial::from(*u) + Polynomial(vec![1]),
            Term::Add(u, v) => Polynomial::from(*u) + Polynomial::from(*v),
            Term::Mul(u, v) => Polynomial::from(*u) * Polynomial::from(*v),
        }
    }
}

#[derive(Debug, Eq)]
struct Polynomial(Vec<u32>);

impl Polynomial {
    fn coefficient(&self, degree: usize) -> u32 {
        *self.0.get(degree).unwrap_or(&0)
    }
}

impl PartialEq for Polynomial {
    fn eq(&self, other: &Self) -> bool {
        for i in 0..self.0.len() {
            if self.coefficient(i) != other.coefficient(i) {
                return false;
            }
        }

        for j in 0..other.0.len() {
            if self.coefficient(j) != other.coefficient(j) {
                return false;
            }
        }

        true
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.0.len() > rhs.0.len() {
            rhs + self
        } else {
            Self(
                self.0
                    .into_iter()
                    .chain(std::iter::repeat(0))
                    .zip(rhs.0.into_iter())
                    .map(|(x, y)| x + y)
                    .collect(),
            )
        }
    }
}

impl Mul for Polynomial {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut coefficients = vec![0; usize::max(0, self.0.len() + rhs.0.len() - 1)];
        for i in 0..self.0.len() {
            for j in 0..rhs.0.len() {
                if i + j < coefficients.len() {
                    coefficients[i + j] += self.0[i] * rhs.0[j];
                }
            }
        }
        Self(coefficients)
    }
}

impl From<Polynomial> for Term {
    fn from(p: Polynomial) -> Self {
        let mut sum = vec![];
        for (i, c) in p.0.iter().enumerate().rev() {
            for j in 0..*c {
                sum.push(Self::monomial(
                    i.try_into().expect("degree should be small enough"),
                ));
            }
        }

        Self::sum_of_terms(sum)
    }
}

impl Term {
    fn sum_of_terms(terms: Vec<Self>) -> Self {
        let Some((first, rest)) = terms.split_first() else {
            return Self::Zero;
        };

        Term::Add(first.clone().into(), Self::sum_of_terms(rest.into()).into())
    }

    fn monomial(degree: u32) -> Self {
        if degree == 0 {
            return Self::S(Self::Zero.into());
        }

        Self::Mul(Self::Variable.into(), Self::monomial(degree - 1).into())
    }
}

#[test]
fn add_polynomials() {
    let p = Polynomial(vec![1, 2, 3]);
    let q = Polynomial(vec![1, 2]);
    assert_eq!(p + q, Polynomial(vec![2, 4, 3]));
}

#[test]
fn mul_polynomials() {
    let p = Polynomial(vec![1, 2, 3]);
    let q = Polynomial(vec![1, 2]);
    assert_eq!(p * q, Polynomial(vec![1, 4, 3 + 4, 6]));
}

#[test]
fn equal_polynomials() {
    let p = Polynomial(vec![]);
    let q = Polynomial(vec![0, 0]);
    assert!(p == q);
}

#[test]
fn polynomial_from_term() {
    let t = Term::Add(Term::Variable.into(), Term::Variable.into());
    assert_eq!(Polynomial(vec![0, 2]), t.into());
}

#[test]
fn polynomial_from_mul_term() {
    let t = Term::S(Term::Mul(Term::Variable.into(), Term::Variable.into()).into());
    assert_eq!(Polynomial(vec![1, 0, 1]), t.into());
}

#[test]
fn term_from_polynomial() {
    let p = Polynomial(vec![1, 1, 1]);
    assert_eq!(
        Term::Add(
            Term::Mul(
                Term::Variable.into(),
                Term::Mul(Term::Variable.into(), Term::S(Term::Zero.into()).into()).into()
            )
            .into(),
            Term::Add(
                Term::Mul(Term::Variable.into(), Term::S(Term::Zero.into()).into()).into(),
                Term::Add(Term::S(Term::Zero.into()).into(), Term::Zero.into()).into()
            )
            .into()
        ),
        p.into()
    );
}

fn main() {
    println!("{p:?}", p = Polynomial(vec![1, 2, 3]));
}
