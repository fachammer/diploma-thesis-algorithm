mod multiset;
mod polynomial;
mod proof;
mod proof_search;
mod term;

use std::{collections::HashMap, vec};

use multiset::Multiset;
use polynomial::{Monomial, Polynomial};
use term::Term;

use crate::proof_search::search_proof;

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
        let mut monomials: Vec<Monomial> = monomials.into_monomials_iter().collect();
        monomials.sort_by(|m_1, m_2| m_1.cmp(m_2).reverse());
        for monomial in monomials {
            summands.push(monomial.clone().into())
        }

        Self::sum_of_terms(summands)
    }
}

pub type Substitution = HashMap<u32, Term>;

pub fn compose_substitutions(left: &Substitution, right: &Substitution) -> Substitution {
    let mut substitution = left.clone();
    for (v, t) in right.iter() {
        substitution
            .entry(*v)
            .and_modify(|term| *term = term.substitute(right))
            .or_insert_with(|| t.clone());
    }
    substitution
}

fn main() {
    println!(
        "print polynomial: {p:?}",
        p = Polynomial::from_coefficients(
            vec![
                (Monomial::from_variable(0, 0), 1),
                (Monomial::from_variable(0, 1), 2),
                (Monomial::from_variable(0, 2), 3)
            ]
            .into_iter()
        )
    );

    let x = Polynomial::from_variable(0);
    let y = Polynomial::from_variable(1);
    let left = 2 * &x * &y + 1;
    let right = 2 * &x + 2 * &y;
    println!(
        "print proof:\n{}",
        search_proof(&left.into(), &right.into()).unwrap()
    )
}
