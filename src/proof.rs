use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::{
    disequality::PolynomialDisequality, disequality::TermDisequality, polynomial::Polynomial,
    proof_search::ProofAttempt, substitution::Substitution, term::Term,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) enum Skeleton {
    SuccessorNonZero,
    Split {
        variable: u32,
        zero_skeleton: Box<Skeleton>,
        successor_skeleton: Box<Skeleton>,
    },
}

#[derive(Serialize, Deserialize, Clone)]
pub enum CompletePolynomialProof {
    SuccessorNonZero {
        conclusion: PolynomialDisequality,
    },
    FoundRoot {
        conclusion: PolynomialDisequality,
    },
    NotStrictlyMonomiallyComparable {
        conclusion: PolynomialDisequality,
    },
    Split {
        variable: u32,
        conclusion: PolynomialDisequality,
        zero_proof: Box<CompletePolynomialProof>,
        successor_proof: Box<CompletePolynomialProof>,
    },
}

impl From<Proof> for CompletePolynomialProof {
    fn from(proof: Proof) -> Self {
        enum ProofType {
            SuccessorNonZero {
                conclusion: TermDisequality,
            },
            Split {
                variable: u32,
                conclusion: TermDisequality,
            },
        }
        let mut parameter_stack = vec![proof];
        let mut operation_stack = vec![];

        while let Some(proof) = parameter_stack.pop() {
            match proof.skeleton {
                Skeleton::SuccessorNonZero => {
                    operation_stack.push(ProofType::SuccessorNonZero {
                        conclusion: proof.conclusion,
                    });
                }
                Skeleton::Split {
                    variable,
                    zero_skeleton,
                    successor_skeleton,
                } => {
                    let zero_proof = Proof {
                        skeleton: *zero_skeleton,
                        conclusion: proof
                            .conclusion
                            .substitute(&Substitution::from_iter([(variable, Term::Zero)])),
                    };
                    parameter_stack.push(zero_proof);

                    let successor_proof = Proof {
                        skeleton: *successor_skeleton,
                        conclusion: proof.conclusion.substitute(&Substitution::from_iter([(
                            variable,
                            Term::S(Term::Variable(variable).into()),
                        )])),
                    };
                    parameter_stack.push(successor_proof);

                    operation_stack.push(ProofType::Split {
                        variable,
                        conclusion: proof.conclusion,
                    });
                }
            }
        }

        let mut return_stack = vec![];

        while let Some(proof) = operation_stack.pop() {
            let result = match proof {
                ProofType::SuccessorNonZero { conclusion } => Self::SuccessorNonZero {
                    conclusion: PolynomialDisequality::from(conclusion),
                },
                ProofType::Split {
                    conclusion,
                    variable,
                } => {
                    let conclusion = PolynomialDisequality::from(conclusion);
                    let successor_proof = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let zero_proof = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");

                    Self::Split {
                        variable,
                        conclusion,
                        zero_proof: Box::new(Self::from(zero_proof)),
                        successor_proof: Box::new(Self::from(successor_proof)),
                    }
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("return value was put on stack")
    }
}

impl From<(ProofAttempt, PolynomialDisequality)> for CompletePolynomialProof {
    fn from(attempt: (ProofAttempt, PolynomialDisequality)) -> Self {
        match attempt.0 {
            ProofAttempt::NotStrictlyMonomiallyComparable => {
                CompletePolynomialProof::NotStrictlyMonomiallyComparable {
                    conclusion: attempt.1,
                }
            }
            ProofAttempt::FoundRoot => CompletePolynomialProof::FoundRoot {
                conclusion: attempt.1,
            },
            ProofAttempt::SuccessorNonZero => CompletePolynomialProof::SuccessorNonZero {
                conclusion: attempt.1,
            },
            ProofAttempt::Split {
                variable,
                zero_proof,
                successor_proof,
            } => CompletePolynomialProof::Split {
                variable,
                conclusion: attempt.1.clone(),
                zero_proof: Box::new(Self::from((
                    *zero_proof,
                    attempt.1.at_variable_zero(variable),
                ))),
                successor_proof: Box::new(Self::from((
                    *successor_proof,
                    attempt.1.into_at_variable_plus_one(variable),
                ))),
            },
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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
