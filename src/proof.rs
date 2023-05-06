use std::fmt::Display;

use crate::{disequality::TermDisequality, substitution::Substitution, term::Term};

pub mod v2 {
    use std::fmt::Display;

    use crate::{
        disequality::{PolynomialDisequality, TermDisequality},
        polynomial::Polynomial,
        substitution::Substitution,
        term::Term,
    };

    #[derive(Clone, Debug)]
    pub enum Skeleton {
        SuccessorNonZero,
        Split {
            variable: u32,
            zero_skeleton: Box<Skeleton>,
            successor_skeleton: Box<Skeleton>,
        },
    }
    #[derive(Clone, Debug)]
    pub struct Proof {
        pub skeleton: Skeleton,
        pub conclusion: TermDisequality,
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
                            proof: &zero_proof,
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
}

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

                let zero_sub = Substitution::from_iter(vec![(*variable, Term::Zero)]);
                let conclusion_at_zero = conclusion.substitute(&zero_sub);

                let s_sub = Substitution::from_iter(vec![(
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
    use crate::{polynomial::Polynomial, proof::v2::Skeleton};

    use super::*;

    #[test]
    fn check_proof_v2_for_golden_ratio_polynomial_is_valid() {
        let x = || Polynomial::from_variable(0);
        let conclusion = TermDisequality::from_terms(x() * x(), x() + 1);
        let skeleton = v2::Skeleton::Split {
            variable: 0,
            zero_skeleton: Skeleton::SuccessorNonZero.into(),
            successor_skeleton: Skeleton::Split {
                variable: 0,
                zero_skeleton: Skeleton::SuccessorNonZero.into(),
                successor_skeleton: Skeleton::SuccessorNonZero.into(),
            }
            .into(),
        };
        let proof = v2::Proof {
            skeleton,
            conclusion,
        };
        assert!(proof.check())
    }

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
