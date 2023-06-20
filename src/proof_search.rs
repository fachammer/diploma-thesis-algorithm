use serde::{Deserialize, Serialize};

use crate::{
    multiset::hash_map::MultisetHashMap,
    polynomial::{Monomial, Polynomial},
    proof::CompletePolynomialProof,
    substitution::Substitution,
    term::Term,
};

use std::collections::VecDeque;

use crate::{
    disequality::{PolynomialDisequality, TermDisequality},
    proof::{Proof, Skeleton},
};

pub fn is_disequality_provable(disequality: &TermDisequality) -> bool {
    matches!(
        search_proof(disequality),
        ProofSearchResult::ProofFound { .. }
    )
}

#[derive(Clone, Serialize, Deserialize)]
pub enum NoProofFoundReason {
    NotStrictlyMonomiallyComparable { substitution: Substitution },
    ExistsRoot { substitution: Substitution },
}

impl NoProofFoundReason {
    fn with_updated_substitution<F: FnOnce(Substitution) -> Substitution>(self, f: F) -> Self {
        match self {
            NoProofFoundReason::NotStrictlyMonomiallyComparable { substitution: s } => {
                NoProofFoundReason::NotStrictlyMonomiallyComparable { substitution: f(s) }
            }
            NoProofFoundReason::ExistsRoot { substitution: s } => {
                Self::ExistsRoot { substitution: f(s) }
            }
        }
    }
}

pub enum ProofSearchResult {
    ProofFound(Proof),
    NoProofFound {
        attempt: ProofAttempt,
        reason: NoProofFoundReason,
    },
}

pub fn search_proof(disequality: &TermDisequality) -> ProofSearchResult {
    let polynomial_disequality = PolynomialDisequality::from(disequality.clone());

    let proof_attempt = search_proof_as_polynomials(polynomial_disequality);
    match Skeleton::try_from(proof_attempt) {
        Ok(skeleton) => ProofSearchResult::ProofFound(Proof {
            conclusion: disequality.clone(),
            skeleton,
        }),
        Err((attempt, reason)) => ProofSearchResult::NoProofFound { attempt, reason },
    }
}

#[derive(Serialize, Deserialize)]
pub enum CompletePolynomialProofSearchResult {
    ProofFound(CompletePolynomialProof),
    NoProofFound {
        attempt: CompletePolynomialProof,
        reason: NoProofFoundReason,
    },
}

pub fn search_complete_proof(disequality: &TermDisequality) -> CompletePolynomialProofSearchResult {
    match search_proof(disequality) {
        ProofSearchResult::ProofFound(proof) => {
            CompletePolynomialProofSearchResult::ProofFound(CompletePolynomialProof::from(proof))
        }
        ProofSearchResult::NoProofFound { attempt, reason } => {
            CompletePolynomialProofSearchResult::NoProofFound {
                attempt: CompletePolynomialProof::from((
                    attempt,
                    PolynomialDisequality::from(disequality.clone()),
                )),
                reason,
            }
        }
    }
}

struct ProofHole<'a> {
    hole: &'a mut ProofInProgress,
    disequality: PolynomialDisequality,
    previous_split_variable: u32,
}

struct ProofSearchIterator<'a> {
    holes: VecDeque<ProofHole<'a>>,
}

impl<'a> ProofSearchIterator<'a> {
    fn new(
        disequality: PolynomialDisequality,
        proof: &'a mut ProofInProgress,
    ) -> ProofSearchIterator<'a> {
        Self {
            holes: VecDeque::from_iter([ProofHole {
                hole: proof,
                disequality,
                previous_split_variable: u32::MAX,
            }]),
        }
    }
}

impl<'a> Iterator for ProofSearchIterator<'a> {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        let ProofHole {
            hole,
            disequality,
            previous_split_variable,
        } = self.holes.pop_front()?;

        match search_proof_step(disequality, previous_split_variable) {
            ProofStepResult::NotStrictlyMonomiallyComparable => {
                *hole = ProofInProgress::NotStrictlyMonomiallyComparable
            }
            ProofStepResult::AllZeroIsRoot => *hole = ProofInProgress::FoundRoot,
            ProofStepResult::SuccessorNonZero => *hole = ProofInProgress::SuccessorNonZero,
            ProofStepResult::Split {
                variable,
                zero_conclusion,
                successor_conclusion,
            } => {
                let (zero_proof, successor_proof) = fill_hole_with_split_proof(hole, variable);
                self.holes.push_back(ProofHole {
                    hole: zero_proof,
                    disequality: zero_conclusion,
                    previous_split_variable: variable,
                });
                self.holes.push_back(ProofHole {
                    hole: successor_proof,
                    disequality: successor_conclusion,
                    previous_split_variable: variable,
                });
            }
        }

        Some(())
    }
}

enum ProofStepResult {
    NotStrictlyMonomiallyComparable,
    AllZeroIsRoot,
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_conclusion: PolynomialDisequality,
        successor_conclusion: PolynomialDisequality,
    },
}

fn search_proof_step(
    disequality: PolynomialDisequality,
    previous_split_variable: u32,
) -> ProofStepResult {
    let disequality = disequality.reduce();
    if disequality.has_zero_root() {
        ProofStepResult::AllZeroIsRoot
    } else if disequality.is_in_successor_non_zero_form() {
        ProofStepResult::SuccessorNonZero
    } else if !disequality.is_strictly_monomially_comparable() {
        ProofStepResult::NotStrictlyMonomiallyComparable
    } else {
        let split_variable = next_split_variable(&disequality, previous_split_variable);
        ProofStepResult::Split {
            variable: split_variable,
            zero_conclusion: disequality.at_variable_zero(split_variable),
            successor_conclusion: disequality.into_at_variable_plus_one(split_variable),
        }
    }
}

fn search_proof_as_polynomials(disequality: PolynomialDisequality) -> ProofAttempt {
    let mut proof = ProofInProgress::Hole;
    let proof_search_iter = ProofSearchIterator::new(disequality, &mut proof);

    for _ in proof_search_iter {}

    ProofAttempt::try_from(proof).expect("proof attempt has no holes")
}

fn next_split_variable(disequality: &PolynomialDisequality, previous_variable: u32) -> u32 {
    let mut variables: Vec<u32> = disequality.variables().copied().collect();
    assert!(!variables.is_empty());
    variables.sort();

    let get_first = || {
        variables
            .first()
            .expect("must exist, otherwise previous code would have failed")
    };

    *variables
        .iter()
        .find(|&&v| v > previous_variable)
        .unwrap_or_else(get_first)
}

fn fill_hole_with_split_proof(
    hole: &mut ProofInProgress,
    split_variable: u32,
) -> (&mut ProofInProgress, &mut ProofInProgress) {
    *hole = ProofInProgress::Split {
        variable: split_variable,
        zero_proof: Box::new(ProofInProgress::Hole),
        successor_proof: Box::new(ProofInProgress::Hole),
    };

    let (zero_proof, successor_proof) = hole
        .proofs_mut()
        .expect("this is not None since we just set the hole as a split");
    (zero_proof, successor_proof)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum ProofInProgress {
    NotStrictlyMonomiallyComparable,
    FoundRoot,
    Hole,
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_proof: Box<ProofInProgress>,
        successor_proof: Box<ProofInProgress>,
    },
}

impl ProofInProgress {
    fn proofs_mut(&mut self) -> Option<(&mut Self, &mut Self)> {
        match self {
            ProofInProgress::Split {
                zero_proof,
                successor_proof,
                ..
            } => Some((zero_proof, successor_proof)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProofAttempt {
    NotStrictlyMonomiallyComparable,
    FoundRoot,
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_proof: Box<ProofAttempt>,
        successor_proof: Box<ProofAttempt>,
    },
}

impl TryFrom<ProofInProgress> for ProofAttempt {
    type Error = String;

    fn try_from(value: ProofInProgress) -> Result<Self, Self::Error> {
        enum ProofInProgressType {
            NotStrictlyMonomiallyComparable,
            FoundRoot,
            Hole,
            SuccessorNonZero,
            Split { variable: u32 },
        }
        let mut parameter_stack = vec![value];
        let mut operation_stack = vec![];
        let mut return_stack: Vec<Result<ProofAttempt, String>> = vec![];

        while let Some(value) = parameter_stack.pop() {
            match value {
                ProofInProgress::NotStrictlyMonomiallyComparable => {
                    operation_stack.push(ProofInProgressType::NotStrictlyMonomiallyComparable);
                }
                ProofInProgress::FoundRoot => {
                    operation_stack.push(ProofInProgressType::FoundRoot);
                }
                ProofInProgress::Hole => {
                    operation_stack.push(ProofInProgressType::Hole);
                }
                ProofInProgress::SuccessorNonZero => {
                    operation_stack.push(ProofInProgressType::SuccessorNonZero);
                }
                ProofInProgress::Split {
                    variable,
                    zero_proof,
                    successor_proof,
                } => {
                    operation_stack.push(ProofInProgressType::Split { variable });
                    parameter_stack.push(*zero_proof);
                    parameter_stack.push(*successor_proof);
                }
            };
        }

        while let Some(value) = operation_stack.pop() {
            let result = match value {
                ProofInProgressType::NotStrictlyMonomiallyComparable => {
                    Ok(Self::NotStrictlyMonomiallyComparable)
                }
                ProofInProgressType::FoundRoot => Ok(Self::FoundRoot),
                ProofInProgressType::Hole => Err(String::from("proof is incomplete")),
                ProofInProgressType::SuccessorNonZero => Ok(Self::SuccessorNonZero),
                ProofInProgressType::Split { variable } => {
                    let successor_proof = return_stack.pop().expect("must exist");
                    let zero_proof = return_stack.pop().expect("must exist");
                    match (zero_proof, successor_proof) {
                        (Ok(zero_attempt), Ok(successor_attempt)) => Ok(Self::Split {
                            variable,
                            zero_proof: zero_attempt.into(),
                            successor_proof: successor_attempt.into(),
                        }),
                        _ => Err(String::from("proof is incomplete")),
                    }
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("must exist")
    }
}

impl ProofAttempt {
    fn try_from_proof_in_progress_recursively(value: ProofInProgress) -> Result<Self, String> {
        match value {
            ProofInProgress::NotStrictlyMonomiallyComparable => {
                Ok(Self::NotStrictlyMonomiallyComparable)
            }
            ProofInProgress::FoundRoot => Ok(Self::FoundRoot),
            ProofInProgress::Hole { .. } => Err(String::from("proof is incomplete")),
            ProofInProgress::SuccessorNonZero => Ok(Self::SuccessorNonZero),
            ProofInProgress::Split {
                variable,
                zero_proof,
                successor_proof,
            } => match (
                Self::try_from_proof_in_progress_recursively(*zero_proof),
                Self::try_from_proof_in_progress_recursively(*successor_proof),
            ) {
                (Ok(zero_attempt), Ok(successor_attempt)) => Ok(Self::Split {
                    variable,
                    zero_proof: zero_attempt.into(),
                    successor_proof: successor_attempt.into(),
                }),
                _ => Err(String::from("proof is incomplete")),
            },
        }
    }
}

impl TryFrom<ProofAttempt> for Skeleton {
    type Error = (ProofAttempt, NoProofFoundReason);

    fn try_from(value: ProofAttempt) -> Result<Self, Self::Error> {
        match value {
            ProofAttempt::NotStrictlyMonomiallyComparable => Err((
                value,
                NoProofFoundReason::NotStrictlyMonomiallyComparable {
                    substitution: Substitution::new(),
                },
            )),
            ProofAttempt::FoundRoot => Err((
                value,
                NoProofFoundReason::ExistsRoot {
                    // TODO: replace this with all zero roots
                    substitution: Substitution::new(),
                },
            )),
            ProofAttempt::SuccessorNonZero => Ok(Skeleton::SuccessorNonZero),
            ProofAttempt::Split {
                variable,
                zero_proof,
                successor_proof,
            } => match (
                Self::try_from(*zero_proof),
                Self::try_from(*successor_proof),
            ) {
                (Ok(zero_skeleton), Ok(successor_skeleton)) => Ok(Self::Split {
                    variable,
                    zero_skeleton: zero_skeleton.into(),
                    successor_skeleton: successor_skeleton.into(),
                }),
                (Ok(zero_skeleton), Err((successor_attempt, reason))) => Err((
                    ProofAttempt::Split {
                        variable,
                        zero_proof: Box::new(ProofAttempt::from(zero_skeleton)),
                        successor_proof: Box::new(successor_attempt),
                    },
                    reason.with_updated_substitution(|s| {
                        s.compose(Substitution::from_iter([(
                            variable,
                            Term::S(Box::new(Term::Variable(variable))),
                        )]))
                    }),
                )),
                (Err((zero_attempt, reason)), Ok(successor_skeleton)) => Err((
                    ProofAttempt::Split {
                        variable,
                        zero_proof: Box::new(zero_attempt),
                        successor_proof: Box::new(ProofAttempt::from(successor_skeleton)),
                    },
                    reason.with_updated_substitution(|s| {
                        s.compose(Substitution::from_iter([(variable, Term::Zero)]))
                    }),
                )),
                (Err((zero_attempt, zero_reason)), Err((sucessor_attempt, successor_reason))) => {
                    Err((
                        ProofAttempt::Split {
                            variable,
                            zero_proof: Box::new(zero_attempt),
                            successor_proof: Box::new(sucessor_attempt),
                        },
                        match (zero_reason, successor_reason) {
                            (
                                NoProofFoundReason::NotStrictlyMonomiallyComparable { .. },
                                successor_reason @ NoProofFoundReason::ExistsRoot { .. },
                            ) => successor_reason.with_updated_substitution(|s| {
                                s.compose(Substitution::from_iter([(
                                    variable,
                                    Term::S(Box::new(Term::Variable(variable))),
                                )]))
                            }),
                            (zero_reason, _) => zero_reason.with_updated_substitution(|s| {
                                s.compose(Substitution::from_iter([(variable, Term::Zero)]))
                            }),
                        },
                    ))
                }
            },
        }
    }
}

impl From<Skeleton> for ProofAttempt {
    fn from(skeleton: Skeleton) -> Self {
        match skeleton {
            Skeleton::SuccessorNonZero => ProofAttempt::SuccessorNonZero,
            Skeleton::Split {
                variable,
                zero_skeleton,
                successor_skeleton,
            } => ProofAttempt::Split {
                variable,
                zero_proof: Box::new(ProofAttempt::from(*zero_skeleton)),
                successor_proof: Box::new(ProofAttempt::from(*successor_skeleton)),
            },
        }
    }
}

impl From<Term> for Polynomial {
    fn from(t: Term) -> Self {
        enum TermType {
            Variable(u32),
            Zero,
            S,
            Add,
            Mul,
        }

        let mut parameter_stack = vec![t];
        let mut operation_stack = vec![];
        let mut return_stack = vec![];

        while let Some(term) = parameter_stack.pop() {
            match term {
                Term::Variable(v) => {
                    operation_stack.push(TermType::Variable(v));
                }
                Term::Zero => {
                    operation_stack.push(TermType::Zero);
                }
                Term::S(inner) => {
                    operation_stack.push(TermType::S);
                    parameter_stack.push(*inner);
                }
                Term::Add(left, right) => {
                    operation_stack.push(TermType::Add);
                    parameter_stack.push(*left);
                    parameter_stack.push(*right);
                }
                Term::Mul(left, right) => {
                    operation_stack.push(TermType::Mul);
                    parameter_stack.push(*left);
                    parameter_stack.push(*right);
                }
            }
        }

        while let Some(term) = operation_stack.pop() {
            let result = match term {
                TermType::Variable(v) => Polynomial::from_variable(v),
                TermType::Zero => Polynomial::from(0),
                TermType::S => {
                    let inner = return_stack.pop().expect("must exist");
                    inner + 1
                }
                TermType::Add => {
                    let right = return_stack.pop().expect("must exist");
                    let left = return_stack.pop().expect("must exist");
                    left + right
                }
                TermType::Mul => {
                    let right = return_stack.pop().expect("must exist");
                    let left = return_stack.pop().expect("must exist");
                    left * right
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("must exist")
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
        let monomials: MultisetHashMap<Monomial> = p.into();
        let mut monomials: Vec<Monomial> = monomials.into_iter().collect();
        monomials.sort_by(|m_1, m_2| m_1.cmp(m_2).reverse());
        Self::sum_of_terms(monomials.into_iter().map(Term::from).collect())
    }
}

#[cfg(test)]
mod test {

    use proptest::prelude::*;

    use crate::{
        disequality::TermDisequality,
        multiset::Multiset,
        polynomial::Polynomial,
        proof_search::{is_disequality_provable, search_proof},
        term::Term,
    };

    use super::*;

    #[test]
    fn provability() {
        let left = Term::S(Term::Zero.into());
        let right = Term::Zero;
        assert!(is_disequality_provable(&TermDisequality::from_terms(
            left, right
        )));
    }

    #[test]
    fn golden_ratio_polynomial_provability() {
        let x = || Polynomial::from_variable(0);
        let left = x() * x();
        let right = x() + 1;

        assert!(is_disequality_provable(&TermDisequality::from_terms(
            left, right
        )));
    }

    #[test]
    fn negated_equality_of_same_terms_is_not_provable() {
        assert!(!is_disequality_provable(&TermDisequality::from_terms(
            Term::S(Term::Zero.into()),
            Term::S(Term::Zero.into())
        )));
    }

    #[test]
    fn even_odd_disequality_is_not_provable() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        let left = 2 * x();
        let right = 2 * y() + 1;
        assert!(!is_disequality_provable(&TermDisequality::from_terms(
            left, right
        )))
    }

    #[test]
    fn two_xy_plus_one_not_equal_to_two_x_plus_two_y_is_provable() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        let left = 2 * x() * y() + 1;
        let right = 2 * x() + 2 * y();
        assert!(is_disequality_provable(&TermDisequality::from_terms(
            left, right
        )))
    }

    #[test]
    fn proof_search_does_not_crash_on_term_with_variable_times_zero() {
        #![allow(unused_must_use)]
        use Term::*;
        let left = S(Zero.into());
        let right = Mul(
            Add(
                Variable(1).into(),
                Mul(Variable(0).into(), Zero.into()).into(),
            )
            .into(),
            S(Add(Zero.into(), Zero.into()).into()).into(),
        );

        search_proof(&TermDisequality::from_terms(left, right));
    }

    #[test]
    fn proof_search_does_not_crash_on_unsolvable_equation() {
        #![allow(unused_must_use)]
        use Term::*;
        let left = S(Zero.into());
        let right = Add(Variable(1).into(), Variable(0).into());

        search_proof(&TermDisequality::from_terms(left, right));
    }

    #[test]
    fn term_to_polynomial_works() {
        let term = Term::S(
            Term::Add(
                Term::Variable(0).into(),
                Term::Mul(Term::Zero.into(), Term::Variable(1).into()).into(),
            )
            .into(),
        );
        assert_eq!(Polynomial::from(term), Polynomial::from_variable(0) + 1);
    }

    prop_compose! {
        fn monomial(max_exponent: u32, max_factors: usize)(exponents in prop::collection::vec((0u32..100, 0..=max_exponent), 0..=max_factors)) -> Monomial {
            Monomial(Multiset::from_iter(exponents))
        }
    }

    prop_compose! {
        fn polynomial(max_exponent: u32, max_factors: usize, max_coefficient: u32, max_coefficients: usize)(coefficients in prop::collection::vec((monomial(max_exponent, max_factors), 0..=max_coefficient), 0..=max_coefficients)) -> Polynomial {
            Polynomial(MultisetHashMap::from_iter(coefficients))
        }
    }

    fn term(depth: u32, desired_size: u32) -> BoxedStrategy<Term> {
        let leaf = prop_oneof![Just(Term::Zero), (0u32..5).prop_map(Term::Variable),];
        leaf.prop_recursive(depth, desired_size, 2, |inner| {
            prop_oneof![
                inner.clone().prop_map(|t| Term::S(t.into())),
                (inner.clone(), inner.clone()).prop_map(|(t, u)| Term::Add(t.into(), u.into())),
                (inner.clone(), inner).prop_map(|(t, u)| Term::Mul(t.into(), u.into())),
            ]
        })
        .boxed()
    }

    proptest! {

        #[test]
        fn polynomial_to_term_and_back(p in polynomial(10, 5, 10, 10)) {
            assert_eq!(Polynomial::from(Term::from(p.clone())), p);
        }

        #[test]
        fn proof_search_is_correct(left in term(5, 10000), right in term(5, 10000)) {
            if let ProofSearchResult::ProofFound(proof) = search_proof(&TermDisequality::from_terms(
                left, right
            )) {
                assert!(proof.check())
            }
        }
    }
}
