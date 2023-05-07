mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use disequality::TermDisequality;
use proof_search::search_proof;
use term::Term;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn greet() {
    let s = "Hello, world";
    web_sys::console::log_1(&JsValue::from_str(s));
}

#[wasm_bindgen]
pub fn search() -> Result<JsValue, JsValue> {
    match search_proof(&TermDisequality::from_terms(
        Term::Mul(Term::S(Term::Zero.into()).into(), Term::Variable(0).into()),
        Term::S(Term::Zero.into()),
    )) {
        Ok(proof) => Ok(serde_wasm_bindgen::to_value(&proof)?),
        Err(attempt) => Err(serde_wasm_bindgen::to_value(&attempt)?),
    }
}
