use std::{collections::HashSet, fmt::Display};

use serde::{Deserialize, Serialize};

use crate::{polynomial::Polynomial, substitution::Substitution};

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum Term {
    Variable(u32),
    Zero,
    S(Box<Term>),
    Add(Box<Term>, Box<Term>),
    Mul(Box<Term>, Box<Term>),
}

impl Term {
    pub(crate) fn sum_of_terms(mut terms: Vec<Self>) -> Self {
        let mut sum_term = Self::Zero;
        terms.reverse();
        for term in terms.into_iter() {
            sum_term = Self::Add(term.into(), sum_term.into());
        }

        sum_term
    }

    pub(crate) fn product_of_terms(terms: Vec<Self>) -> Self {
        let Some((first, rest)) = terms.split_first() else {
            return Self::S(Self::Zero.into());
        };

        Self::Mul(
            first.clone().into(),
            Self::product_of_terms(rest.into()).into(),
        )
    }

    pub(crate) fn substitute(&self, substitution: &Substitution) -> Self {
        enum TermType<'a> {
            Variable(&'a u32),
            Zero,
            S,
            Add,
            Mul,
        }

        let mut parameter_stack = vec![(self, substitution)];
        let mut operation_stack = vec![];

        while let Some((term, substitution)) = parameter_stack.pop() {
            match term {
                Term::Variable(v) => operation_stack.push(TermType::Variable(v)),
                Term::Zero => {
                    operation_stack.push(TermType::Zero);
                }
                Term::S(inner) => {
                    operation_stack.push(TermType::S);
                    parameter_stack.push((inner, substitution));
                }
                Term::Add(left, right) => {
                    operation_stack.push(TermType::Add);
                    parameter_stack.push((left, substitution));
                    parameter_stack.push((right, substitution));
                }
                Term::Mul(left, right) => {
                    operation_stack.push(TermType::Mul);
                    parameter_stack.push((left, substitution));
                    parameter_stack.push((right, substitution));
                }
            }
        }

        let mut return_stack = vec![];
        while let Some(operation) = operation_stack.pop() {
            let result = match operation {
                TermType::Variable(v) => substitution.get(v).cloned().unwrap_or(Term::Variable(*v)),
                TermType::Zero => Term::Zero,
                TermType::S => {
                    let inner: Term = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    Term::S(Box::new(inner))
                }
                TermType::Add => {
                    let right = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let left = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    Term::Add(Box::new(left), Box::new(right))
                }
                TermType::Mul => {
                    let right = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    let left = return_stack
                        .pop()
                        .expect("argument was put on stack in previous iteration");
                    Term::Mul(Box::new(left), Box::new(right))
                }
            };
            return_stack.push(result);
        }

        return_stack.pop().expect("result was put on stack")
    }

    pub(crate) fn free_varaiables(&self) -> HashSet<u32> {
        match self {
            Term::Variable(x) => HashSet::from_iter([*x]),
            Term::Zero => HashSet::new(),
            Term::S(t) => t.free_varaiables(),
            Term::Add(left, right) | Term::Mul(left, right) => {
                let mut variables = left.free_varaiables();
                variables.extend(right.free_varaiables());
                variables
            }
        }
    }
}

struct PolynomialDisplay<'a>(&'a Term);

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        PolynomialDisplay(self).fmt(f)
    }
}

impl<'a> Display for PolynomialDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Polynomial::from(self.0.clone()).fmt(f)
    }
}
