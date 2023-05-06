mod disequality;
mod multiset;
mod polynomial;
mod proof;
mod proof_search;
mod substitution;
mod term;

use polynomial::Polynomial;
use term::Term;

use crate::{
    disequality::TermDisequality,
    proof_search::{is_disequality_provable, search_proof},
};

fn main() {
    let x = || Polynomial::from_variable(0);
    println!("print polynomial: {}", 3 * x() * x() + 2 * x() + 1);

    let x = || Polynomial::from_variable(0);
    let y = || Polynomial::from_variable(1);
    let left = 2 * x() * y() + 1;
    let right = 2 * x() + 2 * y();
    let disequality = TermDisequality::from_terms(left, right);
    assert!(is_disequality_provable(&disequality));
    let proof = search_proof(&disequality).expect("should find proof");

    println!("print proof :\n{}", proof);
    let proof_correct = proof.check();
    println!("proof is correct: {}", proof_correct);
    assert!(proof_correct);
}
