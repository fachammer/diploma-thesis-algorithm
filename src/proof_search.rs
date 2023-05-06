use crate::{
    multiset::Multiset,
    polynomial::{Monomial, Polynomial},
    term::Term,
};

use std::collections::VecDeque;

use crate::{
    disequality::{PolynomialDisequality, TermDisequality},
    proof::{Proof, Skeleton},
};

pub(crate) fn is_disequality_provable(disequality: &TermDisequality) -> bool {
    search_proof(disequality).is_ok()
}

pub(crate) fn search_proof(disequality: &TermDisequality) -> Result<Proof, ProofAttempt> {
    let polynomial_disequality = PolynomialDisequality::from(disequality.clone());

    let proof_attempt = search_proof_as_polynomials(polynomial_disequality);
    let skeleton = Skeleton::try_from(proof_attempt)?;
    Ok(Proof {
        conclusion: disequality.clone(),
        skeleton,
    })
}

fn search_proof_as_polynomials(disequality: PolynomialDisequality) -> ProofAttempt {
    let mut proof = ProofInProgress::Hole;

    let mut holes = VecDeque::from_iter([(&mut proof, disequality, u32::MAX)]);

    while let Some((hole, disequality, previous_split_variable)) = holes.pop_front() {
        let disequality = disequality.reduce();
        if let Ok(()) = try_fill_hole_with_non_split_proof(hole, &disequality) {
            continue;
        }

        let split_variable = next_split_variable(&disequality, previous_split_variable);
        let (zero_proof, successor_proof) = fill_hole_with_split_proof(hole, split_variable);
        let zero_disequality = disequality.at_variable_zero(split_variable);
        holes.push_back((zero_proof, zero_disequality, split_variable));

        let successor_disequality = disequality.into_at_variable_plus_one(split_variable);
        holes.push_back((successor_proof, successor_disequality, split_variable));
    }

    ProofAttempt::try_from(proof).expect("proof attempt has no holes")
}

fn try_fill_hole_with_non_split_proof(
    hole: &mut ProofInProgress,
    disequality: &PolynomialDisequality,
) -> Result<(), ()> {
    *hole = if disequality.has_zero_root() {
        ProofInProgress::FoundRoot
    } else if disequality.is_in_successor_non_zero_form() {
        ProofInProgress::SuccessorNonZero
    } else if !disequality.is_strictly_monomially_comparable() {
        ProofInProgress::NotStrictlyMonomiallyComparable
    } else {
        return Err(());
    };
    Ok(())
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

#[derive(Clone, Debug)]
pub(crate) enum ProofAttempt {
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
                Self::try_from(*zero_proof),
                Self::try_from(*successor_proof),
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
    type Error = ProofAttempt;

    fn try_from(value: ProofAttempt) -> Result<Self, Self::Error> {
        match value {
            ProofAttempt::NotStrictlyMonomiallyComparable | ProofAttempt::FoundRoot => Err(value),
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
                (Ok(zero_skeleton), Err(successor_attempt)) => Err(ProofAttempt::Split {
                    variable,
                    zero_proof: Box::new(ProofAttempt::from(zero_skeleton)),
                    successor_proof: Box::new(successor_attempt),
                }),
                (Err(zero_attempt), Ok(successor_skeleton)) => Err(ProofAttempt::Split {
                    variable,
                    zero_proof: Box::new(zero_attempt),
                    successor_proof: Box::new(ProofAttempt::from(successor_skeleton)),
                }),
                (Err(zero_attempt), Err(sucessor_attempt)) => Err(ProofAttempt::Split {
                    variable,
                    zero_proof: Box::new(zero_attempt),
                    successor_proof: Box::new(sucessor_attempt),
                }),
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
        let mut monomials: Vec<Monomial> = monomials.into_iter().collect();
        monomials.sort_by(|m_1, m_2| m_1.cmp(m_2).reverse());
        for monomial in monomials {
            summands.push(monomial.into());
        }

        Self::sum_of_terms(summands)
    }
}

#[cfg(test)]
mod test {

    use proptest::prelude::*;

    use crate::{
        disequality::TermDisequality,
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

    prop_compose! {
        fn monomial(max_exponent: u32, max_factors: usize)(exponents in prop::collection::vec((0u32..100, 0..=max_exponent), 0..=max_factors)) -> Monomial {
            Monomial(Multiset::from_iter(exponents))
        }
    }

    prop_compose! {
        fn polynomial(max_exponent: u32, max_factors: usize, max_coefficient: u32, max_coefficients: usize)(coefficients in prop::collection::vec((monomial(max_exponent, max_factors), 0..=max_coefficient), 0..=max_coefficients)) -> Polynomial {
            Polynomial(Multiset::from_iter(coefficients))
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
            if let Ok(proof) = search_proof(&TermDisequality::from_terms(
                left, right
            )) {
                assert!(proof.check())
            }
        }
    }
}
