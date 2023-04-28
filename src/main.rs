mod multiset;
mod polynomial;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    vec,
};

use multiset::Multiset;
use polynomial::{reduce, Monomial, Polynomial};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Term {
    Variable(u32),
    Zero,
    S(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
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

impl From<Term> for Polynomial {
    fn from(t: Term) -> Self {
        match t {
            Term::Variable(v) => Polynomial::from_variable(v),
            Term::Zero => 0.into(),
            Term::S(u) => Polynomial::from(*u) + 1,
            Term::Add(u, v) => Polynomial::from(*u) + Polynomial::from(*v),
            Term::Mul(u, v) => Polynomial::from(*u) * Polynomial::from(*v),
        }
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
        p = Polynomial::from_coefficients(
            vec![
                (Monomial::from_variable(0, 0), 1),
                (Monomial::from_variable(0, 1), 2),
                (Monomial::from_variable(0, 2), 3)
            ]
            .into_iter()
        )
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
