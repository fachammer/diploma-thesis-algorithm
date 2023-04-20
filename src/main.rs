use std::{
    collections::{hash_map::Keys, HashMap, HashSet},
    hash::Hash,
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

#[derive(Debug, Eq, Clone)]
struct Polynomial(Vec<u32>);

impl Polynomial {
    fn coefficient(&self, degree: usize) -> u32 {
        *self.0.get(degree).unwrap_or(&0)
    }

    fn evaluate_at_zero(&self) -> u32 {
        self.coefficient(0)
    }

    fn at_variable_plus_one(&self) -> Term {
        Term::from(self.clone()).substitute(Term::S(Term::Variable.into()))
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

    fn substitute(&self, term: Self) -> Self {
        match self {
            Term::Variable => term,
            Term::Zero => Term::Zero,
            Term::S(t) => Term::S(t.substitute(term).into()),
            Term::Add(t, u) => {
                Term::Add(t.substitute(term.clone()).into(), u.substitute(term).into())
            }
            Term::Mul(t, u) => {
                Term::Mul(t.substitute(term.clone()).into(), u.substitute(term).into())
            }
        }
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

#[test]
fn provability_in_AB() {
    let t = Term::S(Term::Zero.into());
    let u = Term::Zero;
    assert!(is_negated_equality_provable_in_AB(t, u));
}

#[test]
fn golden_ratio_polynomial_provability_in_AT() {
    let t = Term::Mul(Term::Variable.into(), Term::Variable.into());
    let u = Term::Add(Term::Variable.into(), Term::S(Term::Zero.into()).into());

    assert!(is_negated_equality_provable_in_AB(t, u));
}

#[test]
fn negated_equality_of_same_terms_is_not_provable_in_AB() {
    assert!(!is_negated_equality_provable_in_AB(
        Term::S(Term::Zero.into()),
        Term::S(Term::Zero.into())
    ));
}

fn reduce(mut left: Polynomial, mut right: Polynomial) -> (Polynomial, Polynomial) {
    let left_clone = left.clone();
    let right_clone = right.clone();
    for (i, c) in left.0.iter_mut().enumerate() {
        *c = c.saturating_sub(right_clone.coefficient(i));
    }

    for (i, c) in right.0.iter_mut().enumerate() {
        *c = c.saturating_sub(left_clone.coefficient(i));
    }

    (left, right)
}

fn is_negated_equality_provable_in_AB(left: Term, right: Term) -> bool {
    let left_poly = Polynomial::from(left);
    let right_poly = Polynomial::from(right);

    let (left_poly, right_poly) = reduce(left_poly, right_poly);

    if left_poly == Polynomial(vec![]) {
        return right_poly.coefficient(0) > 0;
    } else if right_poly == Polynomial(vec![]) {
        return left_poly.coefficient(0) > 0;
    }

    is_negated_equality_provable_in_AB(
        Polynomial(vec![left_poly.evaluate_at_zero()]).into(),
        Polynomial(vec![right_poly.evaluate_at_zero()]).into(),
    ) && is_negated_equality_provable_in_AB(
        left_poly.at_variable_plus_one(),
        right_poly.at_variable_plus_one(),
    )
}

struct Multiset<T> {
    elements: HashMap<T, u32>,
}

impl<T> FromIterator<(T, u32)> for Multiset<T>
where
    T: Eq + Hash,
{
    fn from_iter<U: IntoIterator<Item = (T, u32)>>(iter: U) -> Self {
        let mut multiset = Self::new();
        for e in iter {
            *multiset.amount_mut(e.0) = e.1;
        }
        multiset
    }
}

impl<T> Multiset<T> {
    fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }

    fn support(&self) -> impl Iterator<Item = &T> {
        self.elements.keys()
    }
}

impl<T> Multiset<T>
where
    T: Eq + Hash,
{
    fn amount(&self, element: &T) -> u32 {
        *self.elements.get(element).unwrap_or(&0)
    }

    fn amount_mut(&mut self, element: T) -> &mut u32 {
        self.elements.entry(element).or_insert(0)
    }
}

impl<T> Multiset<T>
where
    T: Eq + Hash + Clone,
{
    fn union(&self, other: &Self) -> Self {
        let mut result = Self::new();
        let self_support: HashSet<&T> = self.support().collect();
        let other_support = other.support().collect();

        let support = self_support.union(&other_support);

        for e in support {
            *result.amount_mut((*e).clone()) = self.amount(e) + other.amount(e);
        }

        result
    }

    fn subtract(&self, other: &Self) -> Self {
        let mut result = Self::new();

        for e in self.support() {
            *result.amount_mut((*e).clone()) = self.amount(e).saturating_sub(other.amount(e));
        }

        result
    }

    fn intersect(&self, other: &Self) -> Self {
        let mut result = Self::new();

        for e in self.support() {
            *result.amount_mut((*e).clone()) = self.amount(e).min(other.amount(e));
        }

        result
    }
}

#[test]
fn multiset_union() {
    let left = Multiset::from_iter(vec![(0, 2), (1, 1)]);
    let right = Multiset::<u32>::from_iter(vec![(1, 1), (2, 2)]);
    let union = left.union(&right);

    assert_eq!(union.amount(&0), 2);
    assert_eq!(union.amount(&1), 2);
    assert_eq!(union.amount(&2), 2);
}

#[test]
fn multiset_subtract() {
    let left = Multiset::from_iter(vec![(0, 2), (1, 1)]);
    let right = Multiset::<u32>::from_iter(vec![(1, 1), (2, 2)]);
    let difference = left.subtract(&right);

    assert_eq!(difference.amount(&0), 2);
    assert_eq!(difference.amount(&1), 0);
    assert_eq!(difference.amount(&2), 0);
}

#[test]
fn multiset_intersect() {
    let left = Multiset::from_iter(vec![(0, 2), (1, 1)]);
    let right = Multiset::<u32>::from_iter(vec![(1, 1), (2, 2)]);
    let difference = left.intersect(&right);

    assert_eq!(difference.amount(&0), 0);
    assert_eq!(difference.amount(&1), 1);
    assert_eq!(difference.amount(&2), 0);
}

#[test]
fn multiset_from_iter() {
    let multiset = Multiset::from_iter(vec![(0, 1), (1, 2), (2, 0), (0, 2)]);

    assert_eq!(multiset.amount(&0), 2);
    assert_eq!(multiset.amount(&1), 2);
    assert_eq!(multiset.amount(&2), 0);
    assert_eq!(multiset.amount(&3), 0);
}

fn main() {
    println!("{p:?}", p = Polynomial(vec![1, 2, 3]));
}
