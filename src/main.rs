use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::{Add, Mul, Sub},
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

impl Term {
    fn free_variables(&self) -> HashSet<u32> {
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

    fn strictly_divides(&self, other: &Monomial) -> bool {
        self.0.is_multisubset_of(&other.0) && self != other
    }
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

        self_variables.sort_by_key(|(v, _)| *v);
        other_variables.sort_by_key(|(v, _)| *v);

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

    fn at_substitution(&self, substitution: &HashMap<u32, Term>) -> Term {
        Term::from(self.clone()).substitute(substitution)
    }

    fn is_monomially_smaller_than(&self, other: &Polynomial) -> bool {
        let (left, right) = reduce(self, other);

        for m_1 in left.0.support() {
            if right.0.support().all(|m_2| !m_1.strictly_divides(m_2)) {
                return false;
            }
        }

        true
    }

    fn is_strictly_monomially_comparable_to(&self, other: &Polynomial) -> bool {
        self.is_monomially_smaller_than(other) || other.is_monomially_smaller_than(self)
    }

    fn predecessor(&self) -> Polynomial {
        let mut polynomial = self.clone();
        polynomial.0.amount_mut(Monomial::one()).saturating_sub(1);
        polynomial
    }
}

impl Add for Polynomial {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl Add<u32> for Polynomial {
    type Output = Polynomial;

    fn add(self, rhs: u32) -> Self::Output {
        self + Polynomial::from(rhs)
    }
}

impl Add<u32> for &Polynomial {
    type Output = Polynomial;

    fn add(self, rhs: u32) -> Self::Output {
        self + &Polynomial::from(rhs)
    }
}

impl Add for &Polynomial {
    type Output = Polynomial;

    fn add(self, rhs: Self) -> Self::Output {
        Polynomial(self.0.union(&rhs.0))
    }
}

impl Add<&Polynomial> for Polynomial {
    type Output = Polynomial;

    fn add(self, rhs: &Polynomial) -> Self::Output {
        &self + rhs
    }
}

impl Mul for Polynomial {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Mul for &Polynomial {
    type Output = Polynomial;

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

impl Mul<&Polynomial> for Polynomial {
    type Output = Polynomial;

    fn mul(self, rhs: &Polynomial) -> Self::Output {
        &self * rhs
    }
}

impl Mul<&Polynomial> for u32 {
    type Output = Polynomial;

    fn mul(self, rhs: &Polynomial) -> Self::Output {
        &Polynomial::from(self) * rhs
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
        Polynomial(Multiset::from_iter(vec![(Monomial::one(), n)]))
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

    fn substitute(&self, substitution: &HashMap<u32, Term>) -> Self {
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

#[test]
fn eq_polynomials() {
    let p = Polynomial(Multiset::from_iter(vec![(Monomial::one(), 0)]));
    let q = Polynomial(Multiset::new());
    assert_eq!(p, q)
}

#[test]
fn add_polynomials() {
    let x = Polynomial::from_variable(0);
    let p = 3 * &x * &x + 2 * &x + 1;
    let q = 2 * &x + 1;
    assert_eq!(p + q, 3 * &x * &x + 4 * &x + 2)
}

#[test]
fn mul_polynomials() {
    let x = Polynomial::from_variable(0);
    let p = 3 * &x * &x + 2 * &x + 1;
    let q = 2 * &x + 1;
    assert_eq!(p * q, 6 * &x * &x * &x + 7 * &x * &x + 4 * &x + 1)
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
    let x = Polynomial::from_variable(0);
    assert_eq!(&x * &x + 1, t.into());
}

#[test]
fn term_from_polynomial() {
    let x = Polynomial::from_variable(0);
    let p = &x * &x + x + 1;
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
    let x = Polynomial::from_variable(0);
    assert!(x.is_monomially_smaller_than(&(&x * &x + 1)))
}

#[test]
fn x_and_y_are_incomparable() {
    let x = Polynomial::from_variable(0);
    let y = Polynomial::from_variable(1);
    assert!(!x.is_strictly_monomially_comparable_to(&y))
}

#[test]
fn provability_in_AB() {
    let t = Term::S(Term::Zero.into());
    let u = Term::Zero;
    assert!(is_negated_equality_provable_in_AB(&t, &u));
}

#[test]
fn golden_ratio_polynomial_provability_in_AB() {
    let x = Polynomial::from_variable(0);
    let left = &x * &x;
    let right = x + 1;

    assert!(is_negated_equality_provable_in_AB(
        &left.into(),
        &right.into()
    ));
}

#[test]
fn negated_equality_of_same_terms_is_not_provable_in_AB() {
    assert!(!is_negated_equality_provable_in_AB(
        &Term::S(Term::Zero.into()),
        &Term::S(Term::Zero.into())
    ));
}

#[test]
fn even_odd_disequality_is_not_provable_in_AB() {
    let x = Polynomial::from_variable(0);
    let y = Polynomial::from_variable(1);
    let left = 2 * x;
    let right = 2 * y + 1;
    assert!(!is_negated_equality_provable_in_AB(
        &left.into(),
        &right.into()
    ))
}

#[test]
fn two_xy_plus_one_not_equal_to_two_x_plus_two_y_in_AB() {
    let x = Polynomial::from_variable(0);
    let y = Polynomial::from_variable(1);
    let left = 2 * &x * &y + 1;
    let right = 2 * &x + 2 * &y;
    assert!(is_negated_equality_provable_in_AB(
        &left.into(),
        &right.into()
    ))
}

fn reduce(left: &Polynomial, right: &Polynomial) -> (Polynomial, Polynomial) {
    let left_reduced_monomials = left.0.subtract(&right.0);
    let right_reduced_monomials = right.0.subtract(&left.0);

    (
        Polynomial(left_reduced_monomials),
        Polynomial(right_reduced_monomials),
    )
}

fn substitutions(
    mut variables: impl Iterator<Item = u32>,
) -> (ProofWithHoles, Vec<HashMap<u32, Term>>) {
    let Some(variable) = variables.next() else {return (ProofWithHoles::Hole,vec![Substitution::new()]);};

    let (proof, subs) = substitutions(variables);
    let mut output = Vec::new();

    for sub in subs {
        let mut zero_sub = sub.clone();
        zero_sub.insert(variable, Term::Zero);
        output.push(zero_sub);
        let mut s_sub = sub;
        s_sub.insert(variable, Term::S(Term::Variable(variable).into()));
        output.push(s_sub);
    }

    (
        ProofWithHoles::Split {
            variable,
            zero_proof: Box::new(proof.clone()),
            successor_proof: Box::new(proof.clone()),
        },
        output,
    )
}

#[test]
fn test_substitutions() {
    assert_eq!(
        substitutions(vec![0].into_iter()).1,
        (vec![
            HashMap::from_iter(vec![(0, Term::Zero)]),
            HashMap::from_iter(vec![(0, Term::S(Term::Variable(0).into()))])
        ])
    );
}

fn is_negated_equality_provable_in_AB(left: &Term, right: &Term) -> bool {
    search_proof(left, right).is_some()
}

type Substitution = HashMap<u32, Term>;

fn search_proof(left: &Term, right: &Term) -> Option<Proof> {
    let right_free_variables = right.free_variables();
    let left_free_variables = left.free_variables();
    let free_variables = left_free_variables.union(&right_free_variables);
    let left_poly = Polynomial::from(left.clone());
    let right_poly = Polynomial::from(right.clone());

    let (left_poly, right_poly) = reduce(&left_poly, &right_poly);

    if !left_poly.is_strictly_monomially_comparable_to(&right_poly) {
        return None;
    }

    if left_poly == 0.into() {
        if right_poly.coefficient(&Monomial::one()) > 0 {
            return Some(Proof::SuccessorNonZero {
                conclusion: (left_poly.into(), right_poly.clone().into()),
                term: right_poly.predecessor().into(),
            });
        }
        return None;
    } else if right_poly == 0.into() {
        if left_poly.coefficient(&Monomial::one()) > 0 {
            return Some(Proof::SuccessorNonZero {
                conclusion: (left_poly.clone().into(), right_poly.into()),
                term: left_poly.predecessor().into(),
            });
        }
        return None;
    }

    let (proof_structure, substitutions) = substitutions(free_variables.cloned());
    let mut proofs: Vec<(Substitution, Proof)> = vec![];

    for substitution in substitutions {
        let Some(proof) = search_proof(
            &left_poly.at_substitution(&substitution),
            &right_poly.at_substitution(&substitution),
        ) else {
            return None;
        };
        proofs.push((substitution, proof));
    }

    Some(
        proof_with_holes_to_proof(
            (left.clone(), right.clone()),
            &proof_structure,
            &proofs,
            &Substitution::new(),
        )
        .expect("there are base proofs for every substitution"),
    )
}

fn proof_with_holes_to_proof(
    conclusion: (Term, Term),
    proof_structure: &ProofWithHoles,
    base_proofs: &Vec<(Substitution, Proof)>,
    substitution: &Substitution,
) -> Result<Proof, Substitution> {
    match proof_structure {
        ProofWithHoles::Hole => {
            let proof = base_proofs
                .iter()
                .filter_map(|(s, p)| {
                    if s == substitution {
                        Some(p.clone())
                    } else {
                        None
                    }
                })
                .next();
            proof.ok_or_else(|| substitution.clone())
        }
        ProofWithHoles::Split {
            variable,
            zero_proof,
            successor_proof,
        } => {
            let zero_sub = Substitution::from_iter(vec![(*variable, Term::Zero)]);
            let zero_proof = proof_with_holes_to_proof(
                (
                    conclusion.0.substitute(&zero_sub),
                    conclusion.1.substitute(&zero_sub),
                ),
                zero_proof,
                base_proofs,
                &compose_substitutions(substitution, &zero_sub),
            )?;

            let s_sub = Substitution::from_iter(vec![(
                *variable,
                Term::S(Term::Variable(*variable).into()),
            )]);
            let successor_proof = proof_with_holes_to_proof(
                (
                    conclusion.0.substitute(&s_sub),
                    conclusion.1.substitute(&s_sub),
                ),
                successor_proof,
                base_proofs,
                &compose_substitutions(substitution, &s_sub),
            )?;
            Ok(Proof::Split {
                conclusion,
                variable: *variable,
                zero_proof: Box::new(zero_proof),
                successor_proof: Box::new(successor_proof),
            })
        }
    }
}

fn compose_substitutions(left: &Substitution, right: &Substitution) -> Substitution {
    let mut substitution = left.clone();
    for (v, t) in right.iter() {
        substitution
            .entry(*v)
            .and_modify(|term| *term = term.substitute(right))
            .or_insert_with(|| t.clone());
    }
    substitution
}

#[derive(Clone)]
enum ProofWithHoles {
    Hole,
    Split {
        variable: u32,
        zero_proof: Box<ProofWithHoles>,
        successor_proof: Box<ProofWithHoles>,
    },
}

#[derive(Clone, Debug)]
enum Proof {
    SuccessorNonZero {
        conclusion: (Term, Term),
        term: Term,
    },
    Split {
        conclusion: (Term, Term),
        variable: u32,
        zero_proof: Box<Proof>,
        successor_proof: Box<Proof>,
    },
}

impl Proof {
    fn conclusion(&self) -> &(Term, Term) {
        match self {
            Proof::SuccessorNonZero { conclusion, .. } => conclusion,
            Proof::Split { conclusion, .. } => conclusion,
        }
    }

    fn check(&self) -> bool {
        match self {
            Proof::SuccessorNonZero { conclusion, term } => {
                Self::are_equivalent_in_T(conclusion, &(Term::S(term.clone().into()), Term::Zero))
            }
            Proof::Split {
                conclusion,
                variable,
                zero_proof,
                successor_proof,
            } => {
                if !zero_proof.check() || !successor_proof.check() {
                    return false;
                }

                let zero_conclusion = zero_proof.conclusion();
                let successor_conclusion = successor_proof.conclusion();

                let zero_sub = HashMap::from_iter(vec![(*variable, Term::Zero)]);
                let conclusion_at_zero_left = conclusion.0.substitute(&zero_sub);
                let conclusion_at_zero_right = conclusion.1.substitute(&zero_sub);

                let s_sub = HashMap::from_iter(vec![(
                    *variable,
                    Term::S(Term::Variable(*variable).into()),
                )]);
                let conclusion_at_s_left = conclusion.0.substitute(&s_sub);
                let conclusion_at_s_right = conclusion.1.substitute(&s_sub);

                Self::are_equivalent_in_T(
                    &(conclusion_at_zero_left, conclusion_at_zero_right),
                    zero_conclusion,
                ) && Self::are_equivalent_in_T(
                    &(conclusion_at_s_left, conclusion_at_s_right),
                    successor_conclusion,
                )
            }
        }
    }

    fn are_equivalent_in_T(left: &(Term, Term), right: &(Term, Term)) -> bool {
        let left_polys: (Polynomial, Polynomial) = (left.0.clone().into(), left.1.clone().into());
        let left = reduce(&left_polys.0, &left_polys.1);
        let right_polys: (Polynomial, Polynomial) =
            (right.0.clone().into(), right.1.clone().into());
        let right = reduce(&right_polys.0, &right_polys.1);
        left.0 == right.0 && left.1 == right.1 || left.0 == right.1 && left.1 == right.0
    }
}

#[test]
fn check_valid_successor_non_zero_proof() {
    let proof = Proof::SuccessorNonZero {
        conclusion: (Term::S(Term::Zero.into()), Term::Zero),
        term: Term::Zero,
    };
    assert!(proof.check())
}

#[test]
fn check_valid_successor_non_zero_proof_with_common_monomials() {
    let left: Polynomial = 2.into();
    let right: Polynomial = 1.into();
    let proof = Proof::SuccessorNonZero {
        conclusion: (left.into(), right.into()),
        term: Term::Zero,
    };
    assert!(proof.check())
}

#[test]
fn check_invalid_successor_non_zero_proof() {
    let x = Polynomial::from_variable(0);
    let y = Polynomial::from_variable(1);
    let proof = Proof::SuccessorNonZero {
        conclusion: (x.into(), y.into()),
        term: Term::S(Term::Zero.into()),
    };
    assert!(!proof.check())
}

#[test]
fn check_proof_for_golden_ratio_polynomial_is_valid() {
    let x = Polynomial::from_variable(0);
    let proof = Proof::Split {
        conclusion: ((&x * &x).into(), (&x + 1).into()),
        variable: 0,
        zero_proof: Proof::SuccessorNonZero {
            conclusion: (Term::Zero, Term::S(Term::Zero.into())),
            term: Term::Zero,
        }
        .into(),
        successor_proof: Proof::Split {
            conclusion: ((&x * &x + 2 * &x + 1).into(), (&x + 2).into()),
            variable: 0,
            zero_proof: Proof::SuccessorNonZero {
                conclusion: (Term::Zero, Term::S(Term::Zero.into())),
                term: Term::Zero,
            }
            .into(),
            successor_proof: Proof::SuccessorNonZero {
                conclusion: ((&x * &x + 3 * &x + 1).into(), Term::Zero),
                term: (&x * &x + 3 * x).into(),
            }
            .into(),
        }
        .into(),
    };
    assert!(proof.check())
}

#[test]
fn check_invalid_split_proof() {
    let x = Polynomial::from_variable(0);
    let proof = Proof::Split {
        conclusion: (x.into(), Term::Zero),
        variable: 0,
        zero_proof: Proof::SuccessorNonZero {
            conclusion: (Term::Zero, Term::S(Term::Zero.into())),
            term: Term::Zero,
        }
        .into(),
        successor_proof: Proof::SuccessorNonZero {
            conclusion: (Term::Zero, Term::S(Term::Zero.into())),
            term: Term::Zero,
        }
        .into(),
    };
    assert!(!proof.check())
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
        self.is_multisubset_of(other) && other.is_multisubset_of(self)
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

    fn is_multisubset_of(&self, other: &Multiset<T>) -> bool {
        for e in self.support() {
            if self.amount(e) > other.amount(e) {
                return false;
            }
        }

        true
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
        "print polynomial: {p:?}",
        p = Polynomial(Multiset::from_iter(vec![
            (Monomial::from_variable(0, 0), 1),
            (Monomial::from_variable(0, 1), 2),
            (Monomial::from_variable(0, 2), 3)
        ]))
    );

    let x = Polynomial::from_variable(0);
    let left = &x * &x;
    let right = x + 1;
    println!(
        "print proof: {:?}",
        search_proof(&left.into(), &right.into())
    )
}
