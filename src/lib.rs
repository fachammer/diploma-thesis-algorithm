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
use web_sys::{console, HtmlElement};

#[wasm_bindgen(start)]
pub fn main() {
    let window = web_sys::window().expect("window must exist");
    let document = window.document().expect("document must exist");
    let left_input: HtmlElement = document
        .get_element_by_id("left-term")
        .expect("left-term must exist")
        .unchecked_into();
    let right_input: HtmlElement = document
        .get_element_by_id("right-term")
        .expect("right-term must exist")
        .unchecked_into();
    let left_input_on_change: Closure<dyn FnMut()> =
        Closure::wrap(Box::new(|| console::log_1(&"on left-input change".into())));
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input_on_change: Closure<dyn FnMut()> =
        Closure::wrap(Box::new(|| console::log_1(&"on right-input change".into())));
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    web_sys::console::log_1(&"main ended".into());
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
