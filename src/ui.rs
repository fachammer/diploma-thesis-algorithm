use disequality::TermDisequality;
use proof_search::search_proof;
use term::Term;
use wasm_bindgen::prelude::*;
use web_sys::{
    console, Document, Element, HtmlElement, HtmlInputElement, InputEvent, Node, Window,
};

use crate::{disequality, proof_search, term};

fn unchecked_document() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document_unchecked()
}

pub(crate) fn setup() {
    let document = unchecked_document();
    let left_input = document.input_by_id_unchecked("left-term");
    let left_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = document.input_by_id_unchecked("right-term");
    let right_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(oninput));
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    render(&document, left_input.value(), right_input.value());
}

fn oninput(_event: InputEvent) {
    let document = unchecked_document();
    let left_value = document.input_by_id_unchecked("left-term").value();
    let right_value = document.input_by_id_unchecked("right-term").value();

    render(&document, left_value, right_value);
}

fn render(document: &Document, left_value: String, right_value: String) {
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

    let left_term_view = document.html_element_by_id_unchecked("left-term-view");
    left_term_view.set_text_content(None);
    left_term_view.append_child_unchecked(&TermTreeView(&left).render(&unchecked_document()));
    let right_term_view = document.html_element_by_id_unchecked("right-term-view");
    right_term_view.set_text_content(None);
    right_term_view.append_child_unchecked(&TermTreeView(&right).render(&unchecked_document()));

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

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement;

    fn input_by_id_unchecked(&self, id: &str) -> HtmlInputElement {
        self.html_element_by_id_unchecked(id).unchecked_into()
    }
}

impl DocumentUnchecked for Document {
    fn create_element_unchecked(&self, element: &str) -> Element {
        self.create_element(element)
            .expect("create element must work")
    }

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement {
        self.get_element_by_id(id)
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
}

trait WindowUnchecked {
    fn document_unchecked(&self) -> Document;
}

impl WindowUnchecked for Window {
    fn document_unchecked(&self) -> Document {
        self.document().expect("document must exist")
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
