use std::{
    collections::{BTreeMap, HashMap, HashSet},
    hash::Hash,
    iter::{Product, Sum},
    num,
    ops::{Add, Mul},
    vec,
};

#[derive(PartialEq, Eq, Debug, Clone)]
enum Term {
    Variable(u32),
    Zero,
    S(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
}

impl From<Term> for Polynomial {
    fn from(t: Term) -> Self {
        match t {
            Term::Variable(v) => Polynomial::from_variable(v),
            Term::Zero => Polynomial(Multiset::new()),
            Term::S(u) => Polynomial::from(*u) + Polynomial::one(),
            Term::Add(u, v) => Polynomial::from(*u) + Polynomial::from(*v),
            Term::Mul(u, v) => Polynomial::from(*u) * Polynomial::from(*v),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Polynomial(Multiset<Monomial>);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Monomial(Multiset<u32>);

impl Monomial {
    fn one() -> Self {
        Self(Multiset::new())
    }

    fn from_variable(v: u32, exponent: u32) -> Self {
        Self(Multiset::from_iter(vec![(v, exponent)]))
    }

    fn variables(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.0.amount_iter().map(|(&v, &n)| (v, n))
    }
}

impl Mul for Monomial {
    type Output = Monomial;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0.union(&rhs.0))
    }
}

impl PartialOrd for Monomial {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Monomial {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let mut self_variables: Vec<(u32, u32)> = self.variables().collect();
        let mut other_variables: Vec<(u32, u32)> = other.variables().collect();

        self_variables.sort_by_key(|(v, _n)| *v);
        other_variables.sort_by_key(|(v, _n)| *v);

        let self_sorted_amounts: Vec<u32> = self_variables.into_iter().map(|(_, n)| n).collect();
        let other_sorted_amounts: Vec<u32> = other_variables.into_iter().map(|(_, n)| n).collect();

        self_sorted_amounts.cmp(&other_sorted_amounts)
    }
}

impl Polynomial {
    fn one() -> Self {
        Self(Multiset::from_iter(vec![(Monomial::one(), 1)]))
    }

    fn from_variable(v: u32) -> Self {
        Self(Multiset::from_iter(vec![(
            Monomial::from_variable(v, 1),
            1,
        )]))
    }

    fn coefficient(&self, monomial: &Monomial) -> u32 {
        self.0.amount(monomial)
    }

    fn at_variable_zero(&self) -> Term {
        Term::from(self.clone()).substitute(Term::Zero)
    }

    fn at_variable_plus_one(&self) -> Term {
        Term::from(self.clone()).substitute(Term::S(Term::Variable(0).into()))
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.union(&rhs.0))
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
        Self(output)
    }
}

impl From<Monomial> for Term {
    fn from(monomial: Monomial) -> Self {
        let mut factors = vec![];
        for (v, n) in monomial.variables() {
            for i in 0..n {
                factors.push(Term::Variable(v));
            }
        }

        Self::product_of_terms(factors)
    }
}

impl From<Polynomial> for Term {
    fn from(p: Polynomial) -> Self {
        let mut summands = vec![];
        let mut monomials: Vec<&Monomial> = p.0.support().collect();
        monomials.sort_by(|m_1, m_2| m_1.cmp(m_2).reverse());
        for monomial in monomials {
            for _ in 0..p.0.amount(monomial) {
                summands.push(monomial.clone().into())
            }
        }

        Self::sum_of_terms(summands)
    }
}

impl Term {
    fn sum_of_terms(terms: Vec<Self>) -> Self {
        let Some((first, rest)) = terms.split_first() else {
            return Self::Zero;
        };

        Self::Add(first.clone().into(), Self::sum_of_terms(rest.into()).into())
    }

    fn product_of_terms(terms: Vec<Self>) -> Self {
        let Some((first, rest)) = terms.split_first() else {
            return Self::S(Self::Zero.into());
        };

        Self::Mul(
            first.clone().into(),
            Self::product_of_terms(rest.into()).into(),
        )
    }

    fn monomial(degree: u32) -> Self {
        if degree == 0 {
            return Self::S(Self::Zero.into());
        }

        Self::Mul(Self::Variable(0).into(), Self::monomial(degree - 1).into())
    }

    fn substitute(&self, term: Self) -> Self {
        match self {
            Term::Variable(v) => term,
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
fn eq_polynomials() {
    let p = Polynomial(Multiset::from_iter(vec![(Monomial::one(), 0)]));
    let q = Polynomial(Multiset::new());
    assert_eq!(p, q)
}

#[test]
fn add_polynomials() {
    let p = Polynomial(Multiset::from_iter(vec![
        (Monomial::from_variable(0, 0), 1),
        (Monomial::from_variable(0, 1), 2),
        (Monomial::from_variable(0, 2), 3),
    ]));
    let q = Polynomial(Multiset::from_iter(vec![
        (Monomial::from_variable(0, 0), 1),
        (Monomial::from_variable(0, 1), 2),
    ]));
    assert_eq!(
        p + q,
        Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 2),
            (Monomial::from_variable(0, 1), 4),
            (Monomial::from_variable(0, 2), 3)
        ]))
    )
}

#[test]
fn mul_polynomials() {
    let p = Polynomial(Multiset::from_iter(vec![
        (Monomial::from_variable(0, 0), 1),
        (Monomial::from_variable(0, 1), 2),
        (Monomial::from_variable(0, 2), 3),
    ]));
    let q = Polynomial(Multiset::from_iter(vec![
        (Monomial::from_variable(0, 0), 1),
        (Monomial::from_variable(0, 1), 2),
    ]));
    assert_eq!(
        p * q,
        Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 1),
            (Monomial::from_variable(0, 1), 4),
            (Monomial::from_variable(0, 2), 3 + 4),
            (Monomial::from_variable(0, 3), 6)
        ]))
    )
}

#[test]
fn polynomial_from_term() {
    let t = Term::Add(Term::Variable(0).into(), Term::Variable(0).into());
    assert_eq!(
        Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 0),
            (Monomial::from_variable(0, 1), 2)
        ])),
        t.into()
    );
}

#[test]
fn polynomial_from_mul_term() {
    let t = Term::S(Term::Mul(Term::Variable(0).into(), Term::Variable(0).into()).into());
    assert_eq!(
        Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 1),
            (Monomial::from_variable(0, 2), 1)
        ])),
        t.into()
    );
}

#[test]
fn term_from_polynomial() {
    let p = Polynomial(Multiset::from_iter(vec![
        (Monomial::from_variable(0, 0), 1),
        (Monomial::from_variable(0, 1), 1),
        (Monomial::from_variable(0, 2), 1),
    ]));
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
fn provability_in_AB() {
    let t = Term::S(Term::Zero.into());
    let u = Term::Zero;
    assert!(is_negated_equality_provable_in_AB(t, u));
}

#[test]
fn golden_ratio_polynomial_provability_in_AT() {
    let t = Term::Mul(Term::Variable(0).into(), Term::Variable(0).into());
    let u = Term::Add(Term::Variable(0).into(), Term::S(Term::Zero.into()).into());

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
    let left_reduced_monomials = left.0.subtract(&right.0);
    let right_reduced_monomials = right.0.subtract(&left.0);

    (
        Polynomial(left_reduced_monomials),
        Polynomial(right_reduced_monomials),
    )
}

fn is_negated_equality_provable_in_AB(left: Term, right: Term) -> bool {
    let left_poly = Polynomial::from(left);
    let right_poly = Polynomial::from(right);

    let (left_poly, right_poly) = reduce(left_poly, right_poly);

    if left_poly == Polynomial(Multiset::new()) {
        return right_poly.coefficient(&Monomial::one()) > 0;
    } else if right_poly == Polynomial(Multiset::new()) {
        return left_poly.coefficient(&Monomial::one()) > 0;
    }

    is_negated_equality_provable_in_AB(left_poly.at_variable_zero(), right_poly.at_variable_zero())
        && is_negated_equality_provable_in_AB(
            left_poly.at_variable_plus_one(),
            right_poly.at_variable_plus_one(),
        )
}

#[derive(Debug, Clone)]
struct Multiset<T>
where
    T: Hash + Eq,
{
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

impl<T> PartialEq for Multiset<T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        for e in self.support().chain(other.support()) {
            if self.amount(e) != other.amount(e) {
                return false;
            }
        }

        true
    }
}

impl<T> Eq for Multiset<T> where T: Eq + Hash {}

impl<T> Hash for Multiset<T>
where
    T: Eq + Hash + Ord,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut support: Vec<&T> = self.support().collect();
        support.sort();
        let hashable: Vec<(&T, u32)> = support
            .into_iter()
            .filter(|e| self.amount(e) > 0)
            .map(|e| (e, self.amount(e)))
            .collect();
        hashable.hash(state)
    }
}

impl<T> Multiset<T>
where
    T: Eq + Hash,
{
    fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }

    fn support(&self) -> impl Iterator<Item = &T> {
        self.elements.keys()
    }

    fn amount(&self, element: &T) -> u32 {
        *self.elements.get(element).unwrap_or(&0)
    }

    fn amount_mut(&mut self, element: T) -> &mut u32 {
        self.elements.entry(element).or_insert(0)
    }

    fn amount_iter(&self) -> impl Iterator<Item = (&T, &u32)> {
        self.elements.iter()
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

    assert_eq!(union, Multiset::from_iter(vec![(0, 2), (1, 2), (2, 2)]));
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

#[test]
fn multiset_eq() {
    let left = Multiset::from_iter(vec![(0, 1), (1, 2), (2, 0)]);
    let right = Multiset::from_iter(vec![(0, 1), (1, 2)]);

    assert_eq!(left, right);
}

#[test]
fn multiset_eq_for_empty() {
    let left = Multiset::new();
    let right = Multiset::from_iter(vec![(0, 0)]);

    assert_eq!(left, right);
}

fn main() {
    println!(
        "{p:?}",
        p = Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 1),
            (Monomial::from_variable(0, 1), 2),
            (Monomial::from_variable(0, 2), 3)
        ]))
    );
}
