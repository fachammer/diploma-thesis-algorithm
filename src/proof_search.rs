use crate::{
    disequality::{PolynomialDisequality, TermDisequality},
    multiset::Multiset,
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

fn split_substitutions(
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
enum ProofWithHoles {
    Hole,
    Split {
        variable: u32,
        zero_proof: Box<ProofWithHoles>,
        successor_proof: Box<ProofWithHoles>,
    },
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
    use crate::{polynomial::Polynomial, term::Term};

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
}
