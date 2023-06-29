use serde::{Deserialize, Serialize};

use crate::{
    multiset::hash_map::MultisetHashMap,
    polynomial::{Monomial, Polynomial},
    proof::CompletePolynomialProof,
    substitution::PolynomialSubstitution,
    term::Term,
};

use std::collections::VecDeque;

use crate::{
    disequality::{PolynomialDisequality, TermDisequality},
    proof::{Proof, Skeleton},
};

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum ProvabilityResultWithoutSubstitution {
    Provable,
    NotStrictlyMonomiallyComparable,
    FoundRoot,
}

pub(crate) fn decide_provability(
    disequality: &PolynomialDisequality,
) -> ProvabilityResultWithoutSubstitution {
    let mut stack = vec![(disequality.clone(), u32::MAX)];
    let mut max_depth = 0;

    while let Some((disequality, previous_split_variable)) = stack.pop() {
        if stack.len() > max_depth {
            max_depth = stack.len();
            println!("new max depth: {max_depth}");
        }
        match search_proof_step(disequality, previous_split_variable) {
            ProofStepResult::NotStrictlyMonomiallyComparable => {
                return ProvabilityResultWithoutSubstitution::NotStrictlyMonomiallyComparable;
            }
            ProofStepResult::AllZeroIsRoot => {
                return ProvabilityResultWithoutSubstitution::FoundRoot;
            }
            ProofStepResult::SuccessorNonZero => {}
            ProofStepResult::Split {
                variable,
                zero_conclusion,
                successor_conclusion,
            } => {
                // note that the tree defined by this splitting is heavily weighted in the
                // direction of the successor branch since setting a variable zero reduces
                // the potential tree size drastically. this is because reducing the amount
                // of variables also reduces the different kinds of splits one has to do.
                // therefore pushing the zero branch last makes sure that it is handled first
                // which has two benefits:
                // - if there is no proof, we will know earlier since we don't check the
                //   long branch first
                // - memory consumption is much smaller since the size of the stack is now the
                //   left-height of the tree which on average is much smaller than the right-height
                stack.push((successor_conclusion, variable));
                stack.push((zero_conclusion, variable));
            }
        }
    }

    ProvabilityResultWithoutSubstitution::Provable
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum ProvabilityResult {
    Provable,
    NotProvable(NoProofFoundReason),
}

pub(crate) fn decide_provability_with_substitution(
    disequality: &PolynomialDisequality,
) -> ProvabilityResult {
    let mut stack = vec![(
        disequality.clone(),
        u32::MAX,
        PolynomialSubstitution::default(),
    )];

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
                    PolynomialSubstitution::from_iter(variables.map(|v| (*v, Polynomial::from(0))));
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
                // note that the tree defined by this splitting is heavily weighted in the
                // direction of the successor branch since setting a variable zero reduces
                // the potential tree size drastically. this is because reducing the amount
                // of variables also reduces the different kinds of splits one has to do.
                // therefore pushing the zero branch last makes sure that it is handled first
                // which has two benefits:
                // - if there is no proof, we will know earlier since we don't check the
                //   long branch first
                // - memory consumption is much smaller since the size of the stack is now the
                //   left-height of the tree which on average is much smaller than the right-height
                stack.push((
                    successor_conclusion,
                    variable,
                    substitution
                        .clone()
                        .compose(PolynomialSubstitution::from_iter([(
                            variable,
                            Polynomial::from_variable(variable) + 1,
                        )])),
                ));
                stack.push((
                    zero_conclusion,
                    variable,
                    substitution.compose(PolynomialSubstitution::from_iter([(
                        variable,
                        Polynomial::from(0),
                    )])),
                ));
            }
        }
    }

    ProvabilityResult::Provable
}

pub fn is_disequality_provable(disequality: &TermDisequality) -> bool {
    matches!(
        decide_provability_with_substitution(&PolynomialDisequality::from(disequality.clone())),
        ProvabilityResult::Provable
    )
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Debug)]
pub(crate) enum NoProofFoundReason {
    NotStrictlyMonomiallyComparable {
        substitution: PolynomialSubstitution,
    },
    ExistsRoot {
        substitution: PolynomialSubstitution,
    },
}

impl NoProofFoundReason {
    fn with_updated_substitution<F: FnOnce(PolynomialSubstitution) -> PolynomialSubstitution>(
        self,
        f: F,
    ) -> Self {
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

#[derive(Serialize, Deserialize)]
pub(crate) enum CompletePolynomialProofSearchResult {
    ProofFound(CompletePolynomialProof),
    NoProofFound {
        attempt: CompletePolynomialProof,
        reason: NoProofFoundReason,
    },
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
        ProvabilityResultWithoutSubstitution::Provable => ProofInProgressSearchResult::ProofFound {
            conclusion: disequality.clone(),
            proof,
        },
        ProvabilityResultWithoutSubstitution::NotStrictlyMonomiallyComparable => {
            ProofInProgressSearchResult::NoProofFound {
                conclusion: disequality.clone(),
                attempt: proof,
                reason: NoProofFoundReason::NotStrictlyMonomiallyComparable {
                    substitution: PolynomialSubstitution::default(),
                },
            }
        }
        ProvabilityResultWithoutSubstitution::FoundRoot => {
            ProofInProgressSearchResult::NoProofFound {
                conclusion: disequality.clone(),
                attempt: proof,
                reason: NoProofFoundReason::ExistsRoot {
                    substitution: PolynomialSubstitution::default(),
                },
            }
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
                        substitution: PolynomialSubstitution::default(),
                    },
                )),
                ProofAttemptType::FoundRoot => Err((
                    ProofAttempt::FoundRoot,
                    NoProofFoundReason::ExistsRoot {
                        substitution: PolynomialSubstitution::default(),
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
                s.compose(PolynomialSubstitution::from_iter([(
                    variable,
                    Polynomial::from_variable(variable) + 1,
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
                s.compose(PolynomialSubstitution::from_iter([(
                    variable,
                    Polynomial::from(0),
                )]))
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
                    s.compose(PolynomialSubstitution::from_iter([(
                        variable,
                        Polynomial::from_variable(variable) + 1,
                    )]))
                }),
                (zero_reason, _) => zero_reason.with_updated_substitution(|s| {
                    s.compose(PolynomialSubstitution::from_iter([(
                        variable,
                        Polynomial::from(0),
                    )]))
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
        disequality::TermDisequality, multiset::Multiset, polynomial::Polynomial,
        proof_search::is_disequality_provable, term::Term,
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
            decide_provability_with_substitution(&PolynomialDisequality { left, right }),
            ProvabilityResult::NotProvable(NoProofFoundReason::ExistsRoot {
                substitution: PolynomialSubstitution::from_iter([(0, Polynomial::from(0))])
            })
        );
    }

    #[test]
    fn decide_provability_finds_correct_substitution_for_non_zero_example() {
        let disequality = PolynomialDisequality {
            left: Polynomial::from_variable(0) + Polynomial::from_variable(1),
            right: Polynomial::from(1),
        };
        let ProvabilityResult::NotProvable(NoProofFoundReason::ExistsRoot{substitution}) = decide_provability_with_substitution(&disequality) else {
            panic!();
        };

        let left = substitution
            .apply_to_polynomial(Polynomial::from_variable(0) + Polynomial::from_variable(1));
        let right = substitution.apply_to_polynomial(Polynomial::from(1));
        assert_eq!(
            PolynomialDisequality::from_polynomials_reduced(left, right),
            PolynomialDisequality {
                left: 0.into(),
                right: 0.into()
            }
        );
    }

    #[test]
    fn decide_provability_finds_correct_substitution_for_non_strictly_monomially_comparable_example(
    ) {
        let disequality = PolynomialDisequality {
            left: 2 * Polynomial::from_variable(0),
            right: 2 * Polynomial::from_variable(1) + 1,
        };
        let ProvabilityResult::NotProvable(NoProofFoundReason::NotStrictlyMonomiallyComparable{substitution}) = decide_provability_with_substitution(&disequality) else {
            panic!("result is strictly monomially comparable");
        };

        let left = substitution.apply_to_polynomial(2 * Polynomial::from_variable(0));
        let right = substitution.apply_to_polynomial(2 * Polynomial::from_variable(1) + 1);
        assert!(
            !PolynomialDisequality::from_polynomials_reduced(left, right)
                .is_strictly_monomially_comparable()
        );
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
    fn decide_provability_does_not_crash_on_term_with_variable_times_zero() {
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

        decide_provability_of_term_disequality(&TermDisequality::from_terms(left, right));
    }

    #[test]
    fn decide_provability_does_not_crash_on_unsolvable_equation() {
        #![allow(unused_must_use)]
        use Term::*;
        let left = S(Zero.into());
        let right = Add(Variable(1).into(), Variable(0).into());

        decide_provability_of_term_disequality(&TermDisequality::from_terms(left, right));
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
        decide_provability_of_term_disequality(&TermDisequality { left, right });
    }

    #[test]
    fn stress_test() {
        let left: Term = "SS0*x*y + S0".parse().unwrap();
        let right: Term = "SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*SS0*(x + y)"
            .parse()
            .unwrap();
        decide_provability(&PolynomialDisequality::from(TermDisequality::from_terms(
            left, right,
        )));
    }

    fn decide_provability_of_term_disequality(
        disequality: &TermDisequality,
    ) -> ProvabilityResultWithoutSubstitution {
        decide_provability(&PolynomialDisequality::from(disequality.clone()))
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

    proptest! {
        #[test]
        fn polynomial_to_term_and_back(p in polynomial(10, 5, 10, 10)) {
            assert_eq!(Polynomial::from(Term::from(p.clone())), p);
        }
    }
}
