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

#[derive(PartialEq, Eq, Debug)]
pub enum ProvabilityResult {
    Provable,
    NotProvable(NoProofFoundReason),
}

pub fn decide_provability(disequality: &PolynomialDisequality) -> ProvabilityResult {
    let mut stack = vec![(disequality.clone(), u32::MAX, Substitution::new())];

    while let Some((disequality, previous_split_variable, substitution)) = stack.pop() {
        match search_proof_step(disequality.clone(), previous_split_variable) {
            ProofStepResult::NotStrictlyMonomiallyComparable => {
                return ProvabilityResult::NotProvable(
                    NoProofFoundReason::NotStrictlyMonomiallyComparable { substitution },
                )
            }
            ProofStepResult::AllZeroIsRoot => {
                let variables = disequality.variables();
                let variable_zero_substitution =
                    Substitution::from_iter(variables.map(|v| (*v, Term::Zero)));
                let substitution = substitution.compose(variable_zero_substitution);
                return ProvabilityResult::NotProvable(NoProofFoundReason::ExistsRoot {
                    substitution,
                });
            }
            ProofStepResult::SuccessorNonZero => {}
            ProofStepResult::Split {
                variable,
                zero_conclusion,
                successor_conclusion,
            } => {
                stack.push((
                    zero_conclusion,
                    variable,
                    substitution
                        .clone()
                        .compose(Substitution::from_iter([(variable, Term::Zero)])),
                ));
                stack.push((
                    successor_conclusion,
                    variable,
                    substitution.compose(Substitution::from_iter([(
                        variable,
                        Term::S(Box::new(Term::Variable(variable))),
                    )])),
                ));
            }
        }
    }

    ProvabilityResult::Provable
}

pub fn is_disequality_provable(disequality: &TermDisequality) -> bool {
    matches!(
        decide_provability(&PolynomialDisequality::from(disequality.clone())),
        ProvabilityResult::Provable
    )
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Debug)]
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

#[derive(Serialize, Deserialize)]
pub(crate) enum ProofSearchResult {
    ProofFound(Proof),
    NoProofFound {
        conclusion: TermDisequality,
        attempt: ProofAttempt,
        reason: NoProofFoundReason,
    },
}

#[derive(Serialize, Deserialize)]
pub(crate) enum ProofInProgressSearchResult {
    ProofFound {
        conclusion: PolynomialDisequality,
        proof: ProofInProgress,
    },
    NoProofFound {
        conclusion: PolynomialDisequality,
        attempt: ProofInProgress,
        reason: NoProofFoundReason,
    },
}

pub(crate) fn search_proof(disequality: &TermDisequality) -> ProofSearchResult {
    let polynomial_disequality = PolynomialDisequality::from(disequality.clone());

    let proof_attempt = search_proof_as_polynomials(polynomial_disequality);
    match Skeleton::try_from(proof_attempt) {
        Ok(skeleton) => ProofSearchResult::ProofFound(Proof {
            conclusion: disequality.clone(),
            skeleton,
        }),
        Err((attempt, reason)) => ProofSearchResult::NoProofFound {
            conclusion: disequality.clone(),
            attempt,
            reason,
        },
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
        ProofSearchResult::NoProofFound {
            conclusion,
            attempt,
            reason,
        } => CompletePolynomialProofSearchResult::NoProofFound {
            attempt: CompletePolynomialProof::from((
                attempt,
                PolynomialDisequality::from(conclusion),
            )),
            reason,
        },
    }
}

struct ProofHole<'a> {
    hole: &'a mut ProofInProgress,
    disequality: PolynomialDisequality,
    previous_split_variable: u32,
    depth: u32,
}

struct ProofSearchIterator<'a> {
    holes: VecDeque<ProofHole<'a>>,
}

impl<'a> ProofSearchIterator<'a> {
    fn new(
        disequality: PolynomialDisequality,
        proof: &'a mut ProofInProgress,
        previous_split_variable: u32,
    ) -> ProofSearchIterator<'a> {
        Self {
            holes: VecDeque::from_iter([ProofHole {
                hole: proof,
                disequality,
                previous_split_variable,
                depth: 0,
            }]),
        }
    }
}

impl<'a> Iterator for ProofSearchIterator<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let ProofHole {
            hole,
            disequality,
            previous_split_variable,
            depth,
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
                    depth: depth + 1,
                });
                self.holes.push_back(ProofHole {
                    hole: successor_proof,
                    disequality: successor_conclusion,
                    previous_split_variable: variable,
                    depth: depth + 1,
                });
            }
        }

        Some(depth)
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) enum ProofStepResult {
    NotStrictlyMonomiallyComparable,
    AllZeroIsRoot,
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_conclusion: PolynomialDisequality,
        successor_conclusion: PolynomialDisequality,
    },
}

pub(crate) fn search_proof_step(
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
    let proof_search_iter = ProofSearchIterator::new(disequality, &mut proof, u32::MAX);

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

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
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

pub(crate) fn search_proof_up_to_depth(
    disequality: &PolynomialDisequality,
    depth: u32,
    previous_split_variable: u32,
) -> ProofInProgressSearchResult {
    let mut proof = ProofInProgress::Hole;
    let proof_search_iter =
        ProofSearchIterator::new(disequality.clone(), &mut proof, previous_split_variable);

    for _ in proof_search_iter.take_while(|&d| d <= depth) {}

    match decide_provability(disequality) {
        ProvabilityResult::Provable => ProofInProgressSearchResult::ProofFound {
            conclusion: disequality.clone(),
            proof,
        },
        ProvabilityResult::NotProvable(reason) => ProofInProgressSearchResult::NoProofFound {
            conclusion: disequality.clone(),
            attempt: proof,
            reason,
        },
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
                    let successor_proof = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let zero_proof = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
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

        return_stack.pop().expect("return value was put on stack")
    }
}

impl TryFrom<ProofAttempt> for Skeleton {
    type Error = (ProofAttempt, NoProofFoundReason);

    fn try_from(value: ProofAttempt) -> Result<Self, Self::Error> {
        enum ProofAttemptType {
            NotStrictlyMonomiallyComparable,
            FoundRoot,
            SuccessorNonZero,
            Split { variable: u32 },
        }
        let mut parameter_stack = vec![value];
        let mut operation_stack = vec![];
        while let Some(value) = parameter_stack.pop() {
            match value {
                ProofAttempt::NotStrictlyMonomiallyComparable => {
                    operation_stack.push(ProofAttemptType::NotStrictlyMonomiallyComparable);
                }
                ProofAttempt::FoundRoot => operation_stack.push(ProofAttemptType::FoundRoot),
                ProofAttempt::SuccessorNonZero => {
                    operation_stack.push(ProofAttemptType::SuccessorNonZero)
                }
                ProofAttempt::Split {
                    variable,
                    zero_proof,
                    successor_proof,
                } => {
                    operation_stack.push(ProofAttemptType::Split { variable });
                    parameter_stack.push(*zero_proof);
                    parameter_stack.push(*successor_proof);
                }
            }
        }

        let mut return_stack = vec![];
        while let Some(value) = operation_stack.pop() {
            let result: Result<Skeleton, (ProofAttempt, NoProofFoundReason)> = match value {
                ProofAttemptType::NotStrictlyMonomiallyComparable => Err((
                    ProofAttempt::NotStrictlyMonomiallyComparable,
                    NoProofFoundReason::NotStrictlyMonomiallyComparable {
                        substitution: Substitution::new(),
                    },
                )),
                ProofAttemptType::FoundRoot => Err((
                    ProofAttempt::FoundRoot,
                    NoProofFoundReason::ExistsRoot {
                        substitution: Substitution::new(),
                    },
                )),
                ProofAttemptType::SuccessorNonZero => Ok(Skeleton::SuccessorNonZero),
                ProofAttemptType::Split { variable } => {
                    let successor_proof_skeleton = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let zero_proof_skeleton = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");

                    split_skeleton_from_skeleton_results(
                        variable,
                        zero_proof_skeleton,
                        successor_proof_skeleton,
                    )
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("return value was put on stack")
    }
}

fn split_skeleton_from_skeleton_results(
    variable: u32,
    zero_proof_skeleton: Result<Skeleton, (ProofAttempt, NoProofFoundReason)>,
    successor_proof_skeleton: Result<Skeleton, (ProofAttempt, NoProofFoundReason)>,
) -> Result<Skeleton, (ProofAttempt, NoProofFoundReason)> {
    match (zero_proof_skeleton, successor_proof_skeleton) {
        (Ok(zero_skeleton), Ok(successor_skeleton)) => Ok(Skeleton::Split {
            variable,
            zero_skeleton: Box::new(zero_skeleton),
            successor_skeleton: Box::new(successor_skeleton),
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
        (Err((zero_attempt, zero_reason)), Err((sucessor_attempt, successor_reason))) => Err((
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
        )),
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
                    let inner = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    inner + 1
                }
                TermType::Add => {
                    let right = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let left = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    left + right
                }
                TermType::Mul => {
                    let right = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let left = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    left * right
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("return value was put on stack")
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
    fn decide_provability_finds_correct_substitution() {
        let left = Polynomial::from_variable(0);
        let right = Polynomial::from(0);
        assert_eq!(
            decide_provability(&PolynomialDisequality { left, right }),
            ProvabilityResult::NotProvable(NoProofFoundReason::ExistsRoot {
                substitution: Substitution::from_iter([(0, Term::Zero)])
            })
        );
    }

    #[test]
    fn decide_provability_finds_correct_substitution_for_non_zero_example() {
        let left = Polynomial::from_variable(0) + Polynomial::from_variable(1);
        let right = Polynomial::from(1);
        let disequality = PolynomialDisequality { left, right };
        let ProvabilityResult::NotProvable(NoProofFoundReason::ExistsRoot{substitution}) = decide_provability(&disequality) else {
            panic!();
        };

        let substituted_disequality = TermDisequality::from(disequality).substitute(&substitution);

        assert_eq!(
            PolynomialDisequality::from(substituted_disequality).reduce(),
            PolynomialDisequality {
                left: 0.into(),
                right: 0.into()
            }
        );
    }

    #[test]
    fn decide_provability_finds_correct_substitution_for_non_strictly_monomially_comparable_example(
    ) {
        let left = 2 * Polynomial::from_variable(0);
        let right = 2 * Polynomial::from_variable(1) + 1;
        let disequality = PolynomialDisequality { left, right };
        let ProvabilityResult::NotProvable(NoProofFoundReason::NotStrictlyMonomiallyComparable{substitution}) = decide_provability(&disequality) else {
            panic!("result is strictly monomially comparable");
        };

        let substituted_disequality = TermDisequality::from(disequality).substitute(&substitution);

        assert!(!PolynomialDisequality::from(substituted_disequality)
            .reduce()
            .is_strictly_monomially_comparable());
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

    #[test]
    fn should_not_result_in_stack_overflow() {
        let left = "SS0*x*y + S0".parse().unwrap();
        let right = "SSSSSSSSS0*SSSSSSSSSSSS0*(x + y)".parse().unwrap();
        search_complete_proof(&TermDisequality { left, right });
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
