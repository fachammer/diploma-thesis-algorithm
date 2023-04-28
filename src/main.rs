mod multiset;
mod polynomial;
mod proof;
mod proof_search;
mod substitution;
mod term;

use std::vec;

use polynomial::{Monomial, Polynomial};
use term::Term;

use crate::proof_search::search_proof;

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
