mod multiset;
mod polynomial;
mod proof;
mod proof_search;
mod term;

use std::{collections::HashMap, vec};

use polynomial::{Monomial, Polynomial};
use term::Term;

use crate::proof_search::search_proof;

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
