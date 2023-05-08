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
use web_sys::{console, Document, HtmlInputElement, InputEvent};

fn unchecked_document() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document().expect("document must exist")
}

fn unchecked_input_by_id(id: &str) -> HtmlInputElement {
    unchecked_document()
        .get_element_by_id(id)
        .unwrap_or_else(|| {
            panic!(
                "{}",
                format_args!("element with id '{id}' must exist")
                    .as_str()
                    .unwrap()
                    .to_string()
            )
        })
        .unchecked_into()
}

fn left_input() -> HtmlInputElement {
    unchecked_input_by_id("left-term")
}

fn right_input() -> HtmlInputElement {
    unchecked_input_by_id("right-term")
}

fn oninput(event: InputEvent) {
    let left_value = left_input().value();
    let rigth_value = right_input().value();

    console::log_3(&left_value.into(), &"≠".into(), &rigth_value.into())
}

#[wasm_bindgen(start)]
pub fn main() {
    let left_input = left_input();
    let left_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = right_input();
    let right_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    console::log_1(&"main ended".into());
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
