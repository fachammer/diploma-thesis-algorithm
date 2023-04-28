mod multiset;
mod polynomial;
mod proof;
mod proof_search;
mod substitution;
mod term;

use std::vec;

use polynomial::{Monomial, Polynomial};
use term::Term;

use crate::proof_search::{is_negated_equality_provable, search_proof};

fn main() {
    println!(
        "print polynomial: {p}",
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
    assert!(is_negated_equality_provable(
        &left.clone().into(),
        &right.clone().into()
    ));
    let proof = search_proof(&left.into(), &right.into());
    let proof = proof.expect("proof should be found");

    println!("print proof:\n{}", proof);
    let proof_correct = proof.check();
    println!("proof is correct: {}", proof_correct);
    assert!(proof_correct);
}
