use crate::{
    multiset::Multiset,
    polynomial::{Monomial, Polynomial},
    term::Term,
};

pub mod v2 {
    use std::collections::VecDeque;

    use crate::{
        disequality::{PolynomialDisequality, TermDisequality},
        polynomial::{Monomial, Polynomial},
        proof::Proof,
        substitution::Substitution,
        term::Term,
    };

    pub fn is_negated_equality_provable(left: &Term, right: &Term) -> bool {
        search_proof(left, right).is_ok()
    }

    pub fn search_proof(left: &Term, right: &Term) -> Result<Proof, ProofAttempt> {
        let left_poly = Polynomial::from(left.clone());
        let right_poly = Polynomial::from(right.clone());
        let PolynomialDisequality {
            left: left_poly,
            right: right_poly,
        } = PolynomialDisequality::from_polynomials_reduced(left_poly, right_poly);
        let left = Term::from(left_poly);
        let right = Term::from(right_poly);
        let right_free_variables = right.free_variables();
        let left_free_variables = left.free_variables();
        let free_variables = left_free_variables.union(&right_free_variables);

        let mut free_variables: Vec<u32> = free_variables.cloned().collect();
        free_variables.sort();

        let mut proof = Box::new(ProofInProgress::Hole {
            conclusion: TermDisequality::from_terms(left, right),
            next_variables: free_variables.iter().copied().cycle(),
        });
        let mut proof_holes = VecDeque::from_iter(vec![proof.as_mut()]);

        while let Some(hole) = proof_holes.pop_front() {
            let (conclusion, next_variables) = hole.hole().unwrap();
            let left_poly = Polynomial::from(conclusion.left().clone());
            let right_poly = Polynomial::from(conclusion.right().clone());
            let PolynomialDisequality {
                left: left_poly,
                right: right_poly,
            } = PolynomialDisequality::from_polynomials_reduced(left_poly, right_poly);

            if !left_poly.is_strictly_monomially_comparable_to(&right_poly) {
                *hole = ProofInProgress::NotStrictlyMonomiallyComparable;
                continue;
            }

            if left_poly == 0.into() {
                *hole = if right_poly.coefficient(&Monomial::one()) > 0 {
                    ProofInProgress::SuccessorNonZero {
                        term: right_poly.predecessor().into(),
                    }
                } else {
                    ProofInProgress::FoundRoot
                };
                continue;
            } else if right_poly == 0.into() {
                *hole = if left_poly.coefficient(&Monomial::one()) > 0 {
                    ProofInProgress::SuccessorNonZero {
                        term: left_poly.predecessor().into(),
                    }
                } else {
                    ProofInProgress::FoundRoot
                };
                continue;
            }

            let mut next_variables = next_variables.clone();
            let variable = next_variables.next().unwrap();

            let zero_proof = Box::new(ProofInProgress::Hole {
                conclusion: conclusion
                    .substitute(&Substitution::from_iter(vec![(variable, Term::Zero)])),
                next_variables: next_variables.clone(),
            });
            let successor_proof = Box::new(ProofInProgress::Hole {
                conclusion: conclusion.substitute(&Substitution::from_iter(vec![(
                    variable,
                    Term::S(Term::Variable(variable).into()),
                )])),
                next_variables: next_variables.clone(),
            });
            *hole = ProofInProgress::Split {
                variable,
                conclusion: conclusion.clone(),
                zero_proof,
                successor_proof,
            };
            let (zero_proof, successor_proof) = hole.proofs_mut().unwrap();
            proof_holes.push_back(zero_proof);
            proof_holes.push_back(successor_proof);
        }

        let proof_attempt = ProofAttempt::try_from(*proof).expect("proof has no holes");
        Proof::try_from(proof_attempt)
    }

    #[derive(Clone)]
    pub enum ProofInProgress<I: Iterator<Item = u32>> {
        NotStrictlyMonomiallyComparable,
        FoundRoot,
        Hole {
            conclusion: TermDisequality,
            next_variables: I,
        },
        SuccessorNonZero {
            term: Term,
        },
        Split {
            variable: u32,
            conclusion: TermDisequality,
            zero_proof: Box<ProofInProgress<I>>,
            successor_proof: Box<ProofInProgress<I>>,
        },
    }

    impl<I: Iterator<Item = u32>> ProofInProgress<I> {
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

        fn hole(&self) -> Option<(&TermDisequality, &I)> {
            match self {
                ProofInProgress::Hole {
                    conclusion,
                    next_variables,
                } => Some((conclusion, next_variables)),
                _ => None,
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum ProofAttempt {
        NotStrictlyMonomiallyComparable,
        FoundRoot,
        SuccessorNonZero {
            term: Term,
        },
        Split {
            variable: u32,
            conclusion: TermDisequality,
            zero_proof: Box<ProofAttempt>,
            successor_proof: Box<ProofAttempt>,
        },
    }

    impl<I: Iterator<Item = u32>> TryFrom<ProofInProgress<I>> for ProofAttempt {
        type Error = String;

        fn try_from(value: ProofInProgress<I>) -> Result<Self, Self::Error> {
            match value {
                ProofInProgress::NotStrictlyMonomiallyComparable => {
                    Ok(Self::NotStrictlyMonomiallyComparable)
                }
                ProofInProgress::FoundRoot => Ok(Self::FoundRoot),
                ProofInProgress::Hole { .. } => Err(String::from("proof is incomplete")),
                ProofInProgress::SuccessorNonZero { term } => Ok(Self::SuccessorNonZero { term }),
                ProofInProgress::Split {
                    variable,
                    conclusion,
                    zero_proof,
                    successor_proof,
                } => match (
                    Self::try_from(*zero_proof),
                    Self::try_from(*successor_proof),
                ) {
                    (Ok(zero_attempt), Ok(successor_attempt)) => Ok(Self::Split {
                        variable,
                        conclusion,
                        zero_proof: zero_attempt.into(),
                        successor_proof: successor_attempt.into(),
                    }),
                    _ => Err(String::from("proof is incomplete")),
                },
            }
        }
    }

    impl TryFrom<ProofAttempt> for Proof {
        type Error = ProofAttempt;

        fn try_from(value: ProofAttempt) -> Result<Self, Self::Error> {
            match value.clone() {
                ProofAttempt::NotStrictlyMonomiallyComparable | ProofAttempt::FoundRoot => {
                    Err(value)
                }
                ProofAttempt::SuccessorNonZero { term } => Ok(Proof::SuccessorNonZero { term }),
                ProofAttempt::Split {
                    variable,
                    conclusion,
                    zero_proof,
                    successor_proof,
                } => match (
                    Self::try_from(*zero_proof),
                    Self::try_from(*successor_proof),
                ) {
                    (Ok(zero_proof), Ok(successor_proof)) => Ok(Self::Split {
                        variable,
                        conclusion,
                        zero_proof: zero_proof.into(),
                        successor_proof: successor_proof.into(),
                    }),
                    _ => Err(value),
                },
            }
        }
    }
}

mod v1 {
    use crate::{
        disequality::{PolynomialDisequality, TermDisequality},
        polynomial::{Monomial, Polynomial},
        proof::Proof,
        substitution::Substitution,
        term::Term,
    };

    pub fn is_negated_equality_provable(left: &Term, right: &Term) -> bool {
        search_proof(left, right).is_some()
    }

    pub fn search_proof(left: &Term, right: &Term) -> Option<Proof> {
        let right_free_variables = right.free_variables();
        let left_free_variables = left.free_variables();
        let free_variables = left_free_variables.union(&right_free_variables);
        let left_poly = Polynomial::from(left.clone());
        let right_poly = Polynomial::from(right.clone());

        let PolynomialDisequality {
            left: left_poly,
            right: right_poly,
        } = PolynomialDisequality::from_polynomials_reduced(left_poly, right_poly);

        if !left_poly.is_strictly_monomially_comparable_to(&right_poly) {
            return None;
        }

        if left_poly == 0.into() {
            if right_poly.coefficient(&Monomial::one()) > 0 {
                return Some(Proof::SuccessorNonZero {
                    term: right_poly.predecessor().into(),
                });
            }
            return None;
        } else if right_poly == 0.into() {
            if left_poly.coefficient(&Monomial::one()) > 0 {
                return Some(Proof::SuccessorNonZero {
                    term: left_poly.predecessor().into(),
                });
            }
            return None;
        }

        let mut free_variables: Vec<u32> = free_variables.cloned().collect();
        free_variables.sort();
        let (proof_structure, substitutions) = split_substitutions(free_variables.into_iter());
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
                TermDisequality::from_terms(left.clone(), right.clone()),
                &proof_structure,
                &proofs,
                &Substitution::new(),
            )
            .expect("there are base proofs for every substitution"),
        )
    }

    fn proof_with_holes_to_proof(
        conclusion: TermDisequality,
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
                    conclusion.substitute(&zero_sub),
                    zero_proof,
                    base_proofs,
                    &substitution.clone().compose(zero_sub),
                )?;

                let s_sub = Substitution::from_iter(vec![(
                    *variable,
                    Term::S(Term::Variable(*variable).into()),
                )]);
                let successor_proof = proof_with_holes_to_proof(
                    conclusion.substitute(&s_sub),
                    successor_proof,
                    base_proofs,
                    &substitution.clone().compose(s_sub),
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

    pub fn split_substitutions(
        mut variables: impl Iterator<Item = u32>,
    ) -> (ProofWithHoles, Vec<Substitution>) {
        let Some(variable) = variables.next() else {
        return (ProofWithHoles::Hole, vec![Substitution::new()]);
    };

        let (proof, subs) = split_substitutions(variables);
        let mut output = Vec::new();

        for sub in subs {
            let mut zero_sub = sub.clone();
            zero_sub.extend(vec![(variable, Term::Zero)]);
            output.push(zero_sub);
            let mut s_sub = sub;
            s_sub.extend(vec![(variable, Term::S(Term::Variable(variable).into()))]);
            output.push(s_sub);
        }

        (
            ProofWithHoles::Split {
                variable,
                zero_proof: Box::new(proof.clone()),
                successor_proof: Box::new(proof),
            },
            output,
        )
    }

    #[derive(Clone)]
    pub enum ProofWithHoles {
        Hole,
        Split {
            variable: u32,
            zero_proof: Box<ProofWithHoles>,
            successor_proof: Box<ProofWithHoles>,
        },
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
            summands.push(monomial.clone().into())
        }

        Self::sum_of_terms(summands)
    }
}

#[cfg(test)]
mod test {
    use proptest::{
        prelude::prop,
        prop_compose, prop_oneof, proptest,
        strategy::{BoxedStrategy, Just, Strategy, ValueTree},
        test_runner::{Config, TestRunner},
    };

    use crate::{
        polynomial::Polynomial,
        proof_search::v1::{is_negated_equality_provable, search_proof, split_substitutions},
        substitution::Substitution,
        term::Term,
    };

    use super::*;

    #[test]
    fn provability() {
        let t = Term::S(Term::Zero.into());
        let u = Term::Zero;
        assert!(is_negated_equality_provable(&t, &u));
    }

    #[test]
    fn golden_ratio_polynomial_provability() {
        let x = || Polynomial::from_variable(0);
        let left = x() * x();
        let right = x() + 1;

        assert!(is_negated_equality_provable(&left.into(), &right.into()));
    }

    #[test]
    fn negated_equality_of_same_terms_is_not_provable() {
        assert!(!is_negated_equality_provable(
            &Term::S(Term::Zero.into()),
            &Term::S(Term::Zero.into())
        ));
    }

    #[test]
    fn even_odd_disequality_is_not_provable() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        let left = 2 * x();
        let right = 2 * y() + 1;
        assert!(!is_negated_equality_provable(&left.into(), &right.into()))
    }

    #[test]
    fn two_xy_plus_one_not_equal_to_two_x_plus_two_y_is_provable() {
        let x = || Polynomial::from_variable(0);
        let y = || Polynomial::from_variable(1);
        let left = 2 * x() * y() + 1;
        let right = 2 * x() + 2 * y();
        assert!(is_negated_equality_provable(&left.into(), &right.into()))
    }

    #[test]
    fn split_substitutions_with_two_variables() {
        assert_eq!(
            split_substitutions(vec![0, 1].into_iter()).1,
            (vec![
                Substitution::from_iter(vec![(0, Term::Zero), (1, Term::Zero)]),
                Substitution::from_iter(vec![
                    (0, Term::S(Term::Variable(0).into())),
                    (1, Term::Zero)
                ]),
                Substitution::from_iter(vec![
                    (0, Term::Zero),
                    (1, Term::S(Term::Variable(1).into()))
                ]),
                Substitution::from_iter(vec![
                    (0, Term::S(Term::Variable(0).into())),
                    (1, Term::S(Term::Variable(1).into()))
                ]),
            ])
        );
    }

    #[test]
    fn proof_search_v2_does_not_crash_on_term_with_variable_times_zero() {
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

        crate::proof_search::v2::search_proof(&left, &right);
    }

    #[test]
    fn proof_search_v2_does_not_crash_on_unsolvable_equation() {
        use Term::*;
        let left = S(Zero.into());
        let right = Add(Variable(1).into(), Variable(0).into());

        crate::proof_search::v2::search_proof(&left, &right);
    }

    prop_compose! {
        fn monomial(max_exponent: u32, max_factors: usize)(exponents in prop::collection::vec((0u32..100, 0..=max_exponent), 0..=max_factors)) -> Monomial {
            Monomial::from_exponents(exponents)
        }
    }

    prop_compose! {
        fn polynomial(max_exponent: u32, max_factors: usize, max_coefficient: u32, max_coefficients: usize)(coefficients in prop::collection::vec((monomial(max_exponent, max_factors), 0..=max_coefficient), 0..=max_coefficients)) -> Polynomial {
            Polynomial::from_coefficients(coefficients)
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

    #[test]
    fn performance_test() {
        let mut runner = TestRunner::deterministic();
        let left = term(20, 400);
        let right = term(20, 400);

        runner
            .run(&(left, right), |(left, right)| {
                crate::proof_search::v2::search_proof(&left, &right);
                Ok(())
            })
            .unwrap();
    }

    proptest! {
        #[test]
        fn polynomial_to_term_and_back(p in polynomial(10, 5, 10, 10)) {
            assert_eq!(Polynomial::from(Term::from(p.clone())), p);
        }

        #[test]
        fn proof_search_does_not_crash(left in term(80, 200), right in term(80, 200)) {
            search_proof(&left, &right);
        }

        #[test]
        fn v2_preserves_provability_to_v1(left in term(30, 200), right in term(30, 200)) {
            assert_eq!(is_negated_equality_provable(&left, &right), crate::proof_search::v2::is_negated_equality_provable(&left, &right))
        }

        #[test]
        fn proof_search_v2_does_not_crash(left in term(30, 200), right in term(30, 200)) {
            crate::proof_search::v2::search_proof(&left, &right);
        }
    }
}
