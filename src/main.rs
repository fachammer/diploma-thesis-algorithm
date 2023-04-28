use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::{Add, Mul},
    vec,
};

use multiset::Multiset;

mod multiset;

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

    fn into_variables_iter(self) -> impl Iterator<Item = u32> {
        self.0.into_monomials_iter()
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
        &self * &rhs
    }
}

impl Mul for &Monomial {
    type Output = Monomial;

    fn mul(self, rhs: Self) -> Self::Output {
        Monomial(self.0.union(&rhs.0))
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

#[test]
fn monomial_order() {
    let x = Monomial::from_variable(0, 1);
    let y = Monomial::from_variable(1, 1);
    assert!(y < &x * &y);
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
        let constant_coefficient = polynomial.0.amount_mut(Monomial::one());
        *constant_coefficient = constant_coefficient.saturating_sub(1);
        polynomial
    }
}

impl From<Polynomial> for Multiset<Monomial> {
    fn from(p: Polynomial) -> Self {
        p.0
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
        let mut factors: Vec<u32> = monomial.into_variables_iter().collect();
        factors.sort();
        let variable_terms = factors.into_iter().map(Term::Variable).collect();
        Self::product_of_terms(variable_terms)
    }
}

impl From<Polynomial> for Term {
    fn from(p: Polynomial) -> Self {
        let mut summands = vec![];
        let monomials: Multiset<Monomial> = p.into();
        let mut monomials: Vec<Monomial> = monomials.into_monomials_iter().collect();
        monomials.sort_by(|m_1, m_2| m_1.cmp(m_2).reverse());
        for monomial in monomials {
            summands.push(monomial.clone().into())
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

    let mut free_variables: Vec<u32> = free_variables.cloned().collect();
    free_variables.sort();
    let (proof_structure, substitutions) = substitutions(free_variables.into_iter());
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

struct ProofDisplay<'a> {
    proof: &'a Proof,
    indentation: u32,
}

impl<'a> ProofDisplay<'a> {
    fn indent(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.indentation {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl<'a> Display for ProofDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.proof {
            Proof::SuccessorNonZero { conclusion, term } => write!(
                f,
                "snz: {} != {} <=>_T {} != 0",
                conclusion.0, conclusion.1, term
            ),
            Proof::Split {
                conclusion,
                variable,
                zero_proof,
                successor_proof,
            } => {
                writeln!(
                    f,
                    "split on x_{} for {} != {}:",
                    variable, conclusion.0, conclusion.1
                )?;
                self.indent(f)?;
                writeln!(
                    f,
                    "  x_{variable} -> 0: {}",
                    ProofDisplay {
                        proof: zero_proof,
                        indentation: self.indentation + 1
                    }
                )?;
                self.indent(f)?;
                write!(
                    f,
                    "  x_{variable} -> s(x_{variable}): {}",
                    ProofDisplay {
                        proof: successor_proof,
                        indentation: self.indentation + 1
                    }
                )
            }
        }
    }
}

impl Display for Proof {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ProofDisplay {
            proof: self,
            indentation: 0,
        }
        .fmt(f)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Polynomial::from(self.clone()).fmt(f)
    }
}

impl Display for Polynomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<&Monomial> = self
            .0
            .amount_iter()
            .filter_map(|(m, a)| if *a > 0 { Some(m) } else { None })
            .collect();
        non_zero.sort_by(|x, y| x.cmp(y).reverse());

        if non_zero.is_empty() {
            return write!(f, "0");
        }
        let strings: Vec<String> = non_zero
            .into_iter()
            .map(|monomial| {
                let amount = self.0.amount(monomial);
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

impl Display for Monomial {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut non_zero: Vec<(&u32, &u32)> =
            self.0.amount_iter().filter(|(_, a)| **a > 0).collect();
        non_zero.sort();
        if non_zero.is_empty() {
            return write!(f, "1");
        }

        let strings: Vec<String> = non_zero
            .into_iter()
            .map(|(variable, amount)| match amount {
                1 => format!("x_{variable}"),
                _ => format!("x_{variable}^{amount}"),
            })
            .collect();
        write!(f, "{}", strings.join(""))
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
    let y = Polynomial::from_variable(1);
    let left = 2 * &x * &y + 1;
    let right = 2 * &x + 2 * &y;
    println!(
        "print proof:\n{}",
        search_proof(&left.into(), &right.into()).unwrap()
    )
}
