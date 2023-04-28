use std::{collections::HashMap, fmt::Display};

use crate::{
    polynomial::{reduce, Polynomial},
    term::Term,
};

#[derive(Clone, Debug)]
pub enum Proof {
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

    pub fn check(&self) -> bool {
        match self {
            Proof::SuccessorNonZero { conclusion, term } => {
                Self::are_equivalent(conclusion, &(Term::S(term.clone().into()), Term::Zero))
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

                Self::are_equivalent(
                    &(conclusion_at_zero_left, conclusion_at_zero_right),
                    zero_conclusion,
                ) && Self::are_equivalent(
                    &(conclusion_at_s_left, conclusion_at_s_right),
                    successor_conclusion,
                )
            }
        }
    }

    fn are_equivalent(left: &(Term, Term), right: &(Term, Term)) -> bool {
        let left_polys: (Polynomial, Polynomial) = (left.0.clone().into(), left.1.clone().into());
        let left = reduce(left_polys.0, left_polys.1);
        let right_polys: (Polynomial, Polynomial) =
            (right.0.clone().into(), right.1.clone().into());
        let right = reduce(right_polys.0, right_polys.1);
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

#[cfg(test)]
mod test {
    use super::*;
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
        let x = || Polynomial::from_variable(0);
        let proof = Proof::Split {
            conclusion: ((x() * x()).into(), (x() + 1).into()),
            variable: 0,
            zero_proof: Proof::SuccessorNonZero {
                conclusion: (Term::Zero, Term::S(Term::Zero.into())),
                term: Term::Zero,
            }
            .into(),
            successor_proof: Proof::Split {
                conclusion: ((x() * x() + 2 * x() + 1).into(), (x() + 2).into()),
                variable: 0,
                zero_proof: Proof::SuccessorNonZero {
                    conclusion: (Term::Zero, Term::S(Term::Zero.into())),
                    term: Term::Zero,
                }
                .into(),
                successor_proof: Proof::SuccessorNonZero {
                    conclusion: ((x() * x() + 3 * x() + 1).into(), Term::Zero),
                    term: (x() * x() + 3 * x()).into(),
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
}
