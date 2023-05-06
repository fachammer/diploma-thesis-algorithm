use std::fmt::Display;

use crate::{
    disequality::PolynomialDisequality, disequality::TermDisequality, polynomial::Polynomial,
    substitution::Substitution, term::Term,
};

#[derive(Clone, Debug)]
pub(crate) enum Skeleton {
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_skeleton: Box<Skeleton>,
        successor_skeleton: Box<Skeleton>,
    },
}
#[derive(Clone, Debug)]
pub struct Proof {
    pub(crate) skeleton: Skeleton,
    pub(crate) conclusion: TermDisequality,
}

impl Proof {
    pub fn check(&self) -> bool {
        match &self.skeleton {
            Skeleton::SuccessorNonZero => {
                let left = self.conclusion.left();
                let right = self.conclusion.right();
                let left = Polynomial::from(left.clone());
                let right = Polynomial::from(right.clone());
                let PolynomialDisequality { left, right } =
                    PolynomialDisequality::from_polynomials_reduced(left, right);
                let left = Term::from(left);
                let right = Term::from(right);

                left == Term::Zero && right != Term::Zero
                    || right == Term::Zero && left != Term::Zero
            }
            Skeleton::Split {
                variable,
                zero_skeleton,
                successor_skeleton,
            } => {
                let zero_sub = Substitution::from_iter([(*variable, Term::Zero)]);
                let zero_disequality = self.conclusion.substitute(&zero_sub);
                let successor_sub = Substitution::from_iter([(
                    *variable,
                    Term::S(Term::Variable(*variable).into()),
                )]);
                let successor_disequality = self.conclusion.substitute(&successor_sub);

                let zero_proof = Proof {
                    // TODO: avoid this clone
                    skeleton: *zero_skeleton.clone(),
                    conclusion: zero_disequality,
                };

                let successor_proof = Proof {
                    // TODO: avoid this clone
                    skeleton: *successor_skeleton.clone(),
                    conclusion: successor_disequality,
                };

                zero_proof.check() && successor_proof.check()
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
        match &self.proof.skeleton {
            Skeleton::SuccessorNonZero => {
                write!(f, "snz: {}", self.proof.conclusion)
            }
            Skeleton::Split {
                variable,
                zero_skeleton,
                successor_skeleton,
            } => {
                writeln!(f, "split on x_{} for {}:", variable, self.proof.conclusion)?;
                self.indent(f)?;
                let zero_sub = Substitution::from_iter([(*variable, Term::Zero)]);
                let zero_conclusion = self.proof.conclusion.substitute(&zero_sub);
                let zero_proof = &Proof {
                    // TODO: avoid this clone
                    skeleton: *zero_skeleton.clone(),
                    conclusion: zero_conclusion,
                };
                writeln!(
                    f,
                    "  x_{variable} -> 0: {}",
                    ProofDisplay {
                        proof: zero_proof,
                        indentation: self.indentation + 1
                    }
                )?;
                self.indent(f)?;

                let successor_sub = Substitution::from_iter([(
                    *variable,
                    Term::S(Term::Variable(*variable).into()),
                )]);
                let successor_conclusion = self.proof.conclusion.substitute(&successor_sub);
                let successor_proof = Proof {
                    // TODO: avoid this clone
                    skeleton: *successor_skeleton.clone(),
                    conclusion: successor_conclusion,
                };
                write!(
                    f,
                    "  x_{variable} -> s(x_{variable}): {}",
                    ProofDisplay {
                        proof: &successor_proof,
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
    use crate::{polynomial::Polynomial, proof::Skeleton};

    use super::*;

    #[test]
    fn check_proof_for_golden_ratio_polynomial_is_valid() {
        let x = || Polynomial::from_variable(0);
        let conclusion = TermDisequality::from_terms(x() * x(), x() + 1);
        let skeleton = Skeleton::Split {
            variable: 0,
            zero_skeleton: Skeleton::SuccessorNonZero.into(),
            successor_skeleton: Skeleton::Split {
                variable: 0,
                zero_skeleton: Skeleton::SuccessorNonZero.into(),
                successor_skeleton: Skeleton::SuccessorNonZero.into(),
            }
            .into(),
        };
        let proof = Proof {
            skeleton,
            conclusion,
        };
        assert!(proof.check())
    }

    #[test]
    fn check_invalid_split_proof() {
        let x = Polynomial::from_variable(0);
        let proof = Proof {
            conclusion: TermDisequality::from_terms(x, Term::Zero),
            skeleton: Skeleton::Split {
                variable: 0,
                zero_skeleton: Skeleton::SuccessorNonZero.into(),
                successor_skeleton: Skeleton::SuccessorNonZero.into(),
            },
        };
        assert!(!proof.check())
    }
}
