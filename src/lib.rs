mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use std::str::FromStr;

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

fn oninput(_event: InputEvent) {
    let left_value = left_input().value();
    let right_value = right_input().value();

    console::log_4(
        &"as strings: ".into(),
        &left_value.as_str().into(),
        &"≠".into(),
        &right_value.as_str().into(),
    );

    let left: Result<Term, _> = left_value.parse();
    let right: Result<Term, _> = right_value.parse();

    let left = left.expect("left term must be valid");
    let right = right.expect("right term must be valid");

    console::log_4(
        &"as terms: ".into(),
        &serde_wasm_bindgen::to_value(&left).unwrap(),
        &"≠".into(),
        &serde_wasm_bindgen::to_value(&right).unwrap(),
    );

    let disequality = TermDisequality::from_terms(left, right);

    match search_proof(&disequality) {
        Ok(proof) => {
            let proof = serde_wasm_bindgen::to_value(&proof).expect("serialize must succeed");
            console::log_2(&"found proof: ".into(), &proof)
        }
        Err(proof_attempt) => {
            let proof_attempt =
                serde_wasm_bindgen::to_value(&proof_attempt).expect("serialize must succeed");
            console::log_2(&"proof attempt: ".into(), &proof_attempt)
        }
    }
}

impl FromStr for Term {
    type Err = InvalidCharacter;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TermParser::parse(s)
    }
}

enum TermToken {
    Variable { name: char },
    Whitespace,
    LeftParenthesis,
    RightParenthesis,
    Zero,
    Successor,
    Plus,
    Star,
}

#[derive(Debug)]
pub struct InvalidCharacter(char);

fn tokens(input: &str) -> Result<impl Iterator<Item = TermToken> + '_, InvalidCharacter> {
    let mut tokens = Vec::with_capacity(input.len());
    for c in input.chars() {
        tokens.push(match c {
            'a'..='z' => TermToken::Variable { name: c },
            '(' => TermToken::LeftParenthesis,
            ')' => TermToken::RightParenthesis,
            '0' => TermToken::Zero,
            'S' => TermToken::Successor,
            '+' => TermToken::Plus,
            '*' => TermToken::Star,
            c if c.is_whitespace() => TermToken::Whitespace,
            c => return Err(InvalidCharacter(c)),
        });
    }
    Ok(tokens.into_iter())
}

struct TermParser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> TermParser<'a> {
    fn parse(input: &'a str) -> Result<Term, InvalidCharacter> {
        let mut parser = Self { input, index: 0 };

        let tokens = tokens(input)?;

        Ok(Term::Zero)
    }
}

#[wasm_bindgen(start)]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

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
