use std::{collections::HashMap, fmt::Display};

use crate::{disequality::TermDisequality, term::Term};

#[derive(Clone, Debug)]
pub enum Proof {
    SuccessorNonZero {
        term: Term,
    },
    Split {
        conclusion: TermDisequality,
        variable: u32,
        zero_proof: Box<Proof>,
        successor_proof: Box<Proof>,
    },
}

impl Proof {
    fn conclusion(&self) -> TermDisequality {
        match self {
            Proof::SuccessorNonZero { term } => {
                TermDisequality::from_terms(Term::S(term.clone().into()), Term::Zero)
            }
            Proof::Split { conclusion, .. } => conclusion.clone(),
        }
    }

    pub fn check(&self) -> bool {
        match self {
            Proof::SuccessorNonZero { .. } => true,
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
                let conclusion_at_zero = conclusion.substitute(&zero_sub);

                let s_sub = HashMap::from_iter(vec![(
                    *variable,
                    Term::S(Term::Variable(*variable).into()),
                )]);
                let conclusion_at_s = conclusion.substitute(&s_sub);

                conclusion_at_zero.is_equivalent_to(&zero_conclusion)
                    && conclusion_at_s.is_equivalent_to(&successor_conclusion)
            }
        }
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
            Proof::SuccessorNonZero { term } => {
                write!(f, "snz: {} != 0", Term::S(term.clone().into()))
            }
            Proof::Split {
                conclusion,
                variable,
                zero_proof,
                successor_proof,
            } => {
                writeln!(f, "split on x_{} for {}:", variable, conclusion)?;
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
    use crate::polynomial::Polynomial;

    use super::*;

    #[test]
    fn check_proof_for_golden_ratio_polynomial_is_valid() {
        let x = || Polynomial::from_variable(0);
        let proof = Proof::Split {
            conclusion: TermDisequality::from_terms(x() * x(), x() + 1),
            variable: 0,
            zero_proof: Proof::SuccessorNonZero { term: Term::Zero }.into(),
            successor_proof: Proof::Split {
                conclusion: TermDisequality::from_terms(x() * x() + 2 * x() + 1, x() + 2),
                variable: 0,
                zero_proof: Proof::SuccessorNonZero { term: Term::Zero }.into(),
                successor_proof: Proof::SuccessorNonZero {
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
            conclusion: TermDisequality::from_terms(x, Term::Zero),
            variable: 0,
            zero_proof: Proof::SuccessorNonZero { term: Term::Zero }.into(),
            successor_proof: Proof::SuccessorNonZero { term: Term::Zero }.into(),
        };
        assert!(!proof.check())
    }
}
