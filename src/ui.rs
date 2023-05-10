use disequality::TermDisequality;
use proof_search::search_proof;
use term::Term;
use wasm_bindgen::prelude::*;
use web_sys::{console, Document, Element, HtmlElement, HtmlInputElement, InputEvent, Node};

use crate::{disequality, proof_search, term};

fn unchecked_document() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document().expect("document must exist")
}

fn unchecked_input_by_id(id: &str) -> HtmlInputElement {
    unchecked_element_by_id(id).unchecked_into()
}

fn unchecked_element_by_id(id: &str) -> web_sys::Element {
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
}

fn left_input() -> HtmlInputElement {
    unchecked_input_by_id("left-term")
}

fn right_input() -> HtmlInputElement {
    unchecked_input_by_id("right-term")
}

fn left_term_display() -> HtmlElement {
    unchecked_element_by_id("left-term-display").unchecked_into()
}

fn right_term_display() -> HtmlElement {
    unchecked_element_by_id("right-term-display").unchecked_into()
}

pub(crate) fn setup() {
    let left_input = left_input();
    let left_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = right_input();
    let right_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    render(left_input.value(), right_input.value());
}

fn oninput(_event: InputEvent) {
    let left_value = left_input().value();
    let right_value = right_input().value();

    render(left_value, right_value);
}

fn render(left_value: String, right_value: String) {
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

    let left_display: Element = left_term_display().into();
    left_display.set_text_content(None);
    left_display.append_child_unchecked(&TermTreeView(&left).render(&unchecked_document()));
    let right_display = right_term_display();
    right_display.set_text_content(None);
    right_display.append_child_unchecked(&TermTreeView(&right).render(&unchecked_document()));

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

trait NodeUnchecked {
    fn append_child_unchecked(&self, child: &Node) -> Node;
}

impl NodeUnchecked for Node {
    fn append_child_unchecked(&self, child: &Node) -> Node {
        self.append_child(child).expect("append child must work")
    }
}

trait DocumentUnchecked {
    fn create_element_unchecked(&self, element: &str) -> Element;
}

impl DocumentUnchecked for Document {
    fn create_element_unchecked(&self, element: &str) -> Element {
        self.create_element(element)
            .expect("create element must work")
    }
}

trait RenderNode {
    fn render(&self, document: &Document) -> Node;
}

struct TermTreeView<'a>(&'a Term);

impl<'a> RenderNode for TermTreeView<'a> {
    fn render(&self, document: &Document) -> Node {
        let term = self;
        match term.0 {
            Term::Variable(x) => document
                .create_text_node(&format!(
                    "{}",
                    char::try_from(*x).expect("must be a valid char value")
                ))
                .into(),
            Term::Zero => document.create_text_node("0").into(),
            Term::S(inner) => {
                let node = document.create_element_unchecked("span");
                node.append_child_unchecked(&document.create_text_node("S"));

                let list = document.create_element_unchecked("ul");
                node.append_child_unchecked(&list);

                let inner_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&inner_item);

                let inner_node = TermTreeView(inner).render(document);
                inner_item.append_child_unchecked(&inner_node);

                node.into()
            }
            Term::Add(left, right) => {
                let node = document.create_element_unchecked("span");
                node.append_child_unchecked(&document.create_text_node("+"));

                let list = document.create_element_unchecked("ul");
                node.append_child_unchecked(&list);

                let left_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&left_item);

                let left_node = TermTreeView(left).render(document);
                left_item.append_child_unchecked(&left_node);

                let right_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&right_item);

                let right_node = TermTreeView(right).render(document);
                right_item.append_child_unchecked(&right_node);

                node.into()
            }
            Term::Mul(left, right) => {
                let node = document.create_element_unchecked("span");
                node.append_child_unchecked(&document.create_text_node("*"));

                let list = document.create_element_unchecked("ul");
                node.append_child_unchecked(&list);

                let left_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&left_item);

                let left_node = TermTreeView(left).render(document);
                left_item.append_child_unchecked(&left_node);

                let right_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&right_item);

                let right_node = TermTreeView(right).render(document);
                right_item.append_child_unchecked(&right_node);

                node.into()
            }
        }
    }
}
