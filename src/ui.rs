use std::{cell::RefCell, collections::HashSet, fmt::Display, rc::Rc};

use disequality::TermDisequality;
use serde::{Deserialize, Serialize};
use term::Term;
use wasm_bindgen::{memory, prelude::*};
use web_sys::{
    console, window, Document, Element, Event, HtmlElement, HtmlInputElement, InputEvent,
    MessageEvent, Node, Window, Worker,
};

use crate::{
    disequality::{self, PolynomialDisequality},
    parse,
    polynomial::{ExponentDisplayStyle, Polynomial, PolynomialDisplay},
    proof::CompletePolynomialProof,
    proof_search::CompletePolynomialProofSearchResult,
    term,
};

fn unchecked_document() -> Document {
    let window = web_sys::window().expect("window must exist");
    window.document_unchecked()
}

pub(crate) fn setup() {
    let worker = Rc::new(RefCell::new(
        Worker::new("./worker.js").expect("worker must exist"),
    ));
    let document = unchecked_document();

    console::log_1(&"hello".into());

    let left_input = document.input_by_id_unchecked("left-term-input");
    let left_input_on_change = oninput_handler(worker.clone());
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = document.input_by_id_unchecked("right-term-input");
    let right_input_on_change = oninput_handler(worker.clone());
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    document
        .query_selector("body")
        .expect("query selector must succeed")
        .expect("there must be a body")
        .set_attribute_unchecked("style", "opacity: 1; margin-top: 0px");

    let worker_clone = worker.clone();
    let worker_callback = Closure::wrap(Box::new(move |event: MessageEvent| {
        console::log_1(&"got ready message".into());
        assert_eq!(event.data(), "ready");
        worker_clone.borrow().set_onmessage(None);

        update(worker_clone.clone());
    }) as Box<dyn Fn(_)>);
    worker
        .borrow()
        .set_onmessage(Some(worker_callback.as_ref().unchecked_ref()));
    worker
        .borrow()
        .post_message(&memory())
        .expect("post message memory should work");
    worker_callback.forget();
}

fn unchecked_now() -> f64 {
    window()
        .expect("there should be a window")
        .performance()
        .expect("there shold be performance")
        .now()
}

fn oninput_handler(worker: Rc<RefCell<Worker>>) -> Closure<dyn Fn(InputEvent)> {
    Closure::wrap(Box::new(move |_| {
        update(worker.clone());
    }))
}

#[derive(Serialize, Deserialize)]
pub struct SearchProof {
    pub(crate) disequality: TermDisequality,
}

enum ValidationResult {
    Valid { left: Term, right: Term },
    Invalid { left: bool, right: bool },
}

fn validate(
    left: Result<Term, parse::Error>,
    right: Result<Term, parse::Error>,
) -> ValidationResult {
    match (left, right) {
        (Ok(left), Ok(right)) => {
            let left_uses_valid_variables =
                left.free_varaiables().is_subset(&HashSet::from_iter([
                    'u' as u32, 'v' as u32, 'w' as u32, 'x' as u32, 'y' as u32, 'z' as u32,
                ]));
            let right_uses_valid_variables =
                right.free_varaiables().is_subset(&HashSet::from_iter([
                    'u' as u32, 'v' as u32, 'w' as u32, 'x' as u32, 'y' as u32, 'z' as u32,
                ]));
            if left_uses_valid_variables && right_uses_valid_variables {
                ValidationResult::Valid { left, right }
            } else {
                ValidationResult::Invalid {
                    left: left_uses_valid_variables,
                    right: right_uses_valid_variables,
                }
            }
        }
        (left, right) => ValidationResult::Invalid {
            left: left.is_ok(),
            right: right.is_ok(),
        },
    }
}

fn update(worker: Rc<RefCell<Worker>>) {
    let document = unchecked_document();
    let left_term_element = document.input_by_id_unchecked("left-term-input");
    let right_term_element = document.input_by_id_unchecked("right-term-input");
    let polynomial_view = document.html_element_by_id_unchecked("polynomial-view");
    let proof_view = document.html_element_by_id_unchecked("proof-view");
    proof_view.set_text_content(None);
    let validation_messages_element = document.html_element_by_id_unchecked("validation-messages");
    let left_term_validation_message_element =
        document.html_element_by_id_unchecked("left-term-validation-message");
    let right_term_validation_message_element =
        document.html_element_by_id_unchecked("right-term-validation-message");
    let proof_search_status_view =
        document.html_element_by_id_unchecked("proof-search-status-view");
    let proof_search_status = document.html_element_by_id_unchecked("proof-search-status");
    proof_search_status.set_text_content(Some("in progress..."));

    let left: Result<Term, _> = left_term_element.text_content().unwrap_or_default().parse();
    let right: Result<Term, _> = right_term_element
        .text_content()
        .unwrap_or_default()
        .parse();

    let validation_result = validate(left, right);

    match validation_result {
        ValidationResult::Valid { left, right } => {
            left_term_element.set_attribute_unchecked("data-valid", "true");
            right_term_element.set_attribute_unchecked("data-valid", "true");
            validation_messages_element.set_attribute_unchecked("data-valid", "true");
            left_term_validation_message_element.set_attribute_unchecked("data-valid", "true");
            right_term_validation_message_element.set_attribute_unchecked("data-valid", "true");
            proof_search_status_view.set_attribute_unchecked("data-visible", "true");
            polynomial_view.set_attribute_unchecked("data-visible", "true");
            proof_view.set_attribute_unchecked("data-visible", "true");

            let left_polynomial_view = document.html_element_by_id_unchecked("left-polynomial");
            left_polynomial_view.set_text_content(None);
            let left_polynomial = Polynomial::from(left.clone());
            left_polynomial_view.append_child_unchecked(&left_polynomial.render(&document));

            let right_polynomial_view = document.html_element_by_id_unchecked("right-polynomial");
            right_polynomial_view.set_text_content(None);
            let right_polynomial = Polynomial::from(right.clone());
            right_polynomial_view.append_child_unchecked(&right_polynomial.render(&document));

            let disequality = TermDisequality::from_terms(left, right);

            worker
                .borrow()
                .post_message(
                    &serde_wasm_bindgen::to_value(&SearchProof { disequality })
                        .expect("to value should work"),
                )
                .expect("post message should work");

            let worker_callback = Closure::wrap(Box::new(move |event: MessageEvent| {
                let document = unchecked_document();
                let start_time = unchecked_now();

                let proof_view = document.html_element_by_id_unchecked("proof-view");
                proof_view.set_text_content(None);
                let proof_result_pointer =
                    event.data().as_f64().expect("data must be a number") as u32;
                let proof_result = unsafe {
                    Box::from_raw(proof_result_pointer as *mut CompletePolynomialProofSearchResult)
                };

                let proof_search_status =
                    document.html_element_by_id_unchecked("proof-search-status");
                match *proof_result {
                    CompletePolynomialProofSearchResult::ProofFound(proof) => {
                        proof_view.append_child_unchecked(
                            &ProofView {
                                proof,
                                current_depth: 0,
                                max_depth: 4,
                            }
                            .render(&document),
                        );
                        proof_search_status.set_text_content(Some("found a proof"));
                    }
                    CompletePolynomialProofSearchResult::NoProofFound { attempt, reason } => {
                        proof_view.append_child_unchecked(
                            &ProofView {
                                proof: attempt,
                                current_depth: 0,
                                max_depth: 4,
                            }
                            .render(&document),
                        );
                        let reason_text = match reason {
                            crate::proof_search::NoProofFoundReason::NotStrictlyMonomiallyComparable { .. } => "≸",
                            crate::proof_search::NoProofFoundReason::ExistsRoot { .. } => "exists a root",
                        };
                        proof_search_status
                            .set_text_content(Some(&format!("there is no proof: {reason_text}")));
                    }
                }
                let end_time = unchecked_now();
                console::log_1(&format!("elapsed time: {} ms", end_time - start_time).into());
                let window_width = window()
                    .expect("window must exist")
                    .inner_width()
                    .expect("inner width must exist");
                let window_width = window_width.unchecked_into_f64();
                let proof_view_scroll_width: f64 = proof_view.scroll_width().into();
                proof_view
                    .scroll_to_with_x_and_y((proof_view_scroll_width - window_width) / 2.0, 0.0);
            }) as Box<dyn FnMut(_)>);
            worker
                .borrow()
                .set_onmessage(Some(worker_callback.as_ref().unchecked_ref()));

            // TODO: fix this leakage
            worker_callback.forget();
        }
        ValidationResult::Invalid { left, right } => {
            validation_messages_element.set_attribute_unchecked("data-valid", "false");
            left_term_element.set_attribute_unchecked("data-valid", &left.to_string());
            right_term_element.set_attribute_unchecked("data-valid", &right.to_string());
            left_term_validation_message_element
                .set_attribute_unchecked("data-valid", &left.to_string());
            right_term_validation_message_element
                .set_attribute_unchecked("data-valid", &right.to_string());
            polynomial_view.set_attribute_unchecked("data-visible", "false");
            proof_search_status_view.set_attribute_unchecked("data-visible", "false");
            proof_view.set_attribute_unchecked("data-visible", "false");
        }
    };
}

trait NodeUnchecked {
    fn append_child_unchecked(&self, child: &Node) -> Node;
}

impl NodeUnchecked for Node {
    fn append_child_unchecked(&self, child: &Node) -> Node {
        self.append_child(child).expect("append child must work")
    }
}

trait ElementUnchecked {
    fn set_attribute_unchecked(&self, name: &str, value: &str);
}

impl ElementUnchecked for Element {
    fn set_attribute_unchecked(&self, name: &str, value: &str) {
        self.set_attribute(name, value)
            .expect("set attribute must succeed");
    }
}

trait DocumentUnchecked {
    fn create_element_unchecked(&self, element: &str) -> Element;

    fn html_element_by_id_unchecked(&self, id: &str) -> HtmlElement;

    fn input_by_id_unchecked(&self, id: &str) -> HtmlInputElement {
        self.html_element_by_id_unchecked(id).unchecked_into()
    }

    fn create_div_unchecked(&self) -> HtmlElement {
        self.create_element_unchecked("div").unchecked_into()
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
    fn render(self, document: &Document) -> Node;
}

impl RenderNode for Polynomial {
    fn render(self, document: &Document) -> Node {
        let node = document
            .create_element("span")
            .expect("create span element should succeed");
        node.set_inner_html(&format!(
            "{}",
            PolynomialDisplay {
                polynomial: &self,
                variable_mapping: &|v| String::from(
                    char::try_from(v).expect("variable must be a valid char value")
                ),
                number_of_largest_monomials: 5,
                number_of_smallest_monomials: 5,
                exponent_display_style: ExponentDisplayStyle::SuperscriptTag
            }
        ));
        node.into()
    }
}

struct ProofView {
    proof: CompletePolynomialProof,
    current_depth: u32,
    max_depth: u32,
}

impl RenderNode for ProofView {
    fn render(self, document: &Document) -> Node {
        match self.proof {
            CompletePolynomialProof::SuccessorNonZero { conclusion } => {
                render_proof_leaf(document, &conclusion, ProofLeaf::SuccessorNonZero)
            }
            CompletePolynomialProof::FoundRoot { conclusion } => {
                render_proof_leaf(document, &conclusion, ProofLeaf::FoundRoot)
            }
            CompletePolynomialProof::NotStrictlyMonomiallyComparable { conclusion } => {
                render_proof_leaf(
                    document,
                    &conclusion,
                    ProofLeaf::NotStrictlyMonomiallyComparable,
                )
            }
            CompletePolynomialProof::Split {
                variable,
                conclusion,
                zero_proof,
                successor_proof,
            } => {
                let conclusion = conclusion.reduced();
                let conclusion_text = document
                    .create_element("span")
                    .expect("create span should work");
                conclusion_text.set_inner_html(&format!(
                    "{} ≠ {}",
                    PolynomialDisplay {
                        polynomial: &conclusion.left,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5,
                        exponent_display_style: ExponentDisplayStyle::SuperscriptTag
                    },
                    PolynomialDisplay {
                        polynomial: &conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5,
                        exponent_display_style: ExponentDisplayStyle::SuperscriptTag
                    },
                ));
                let conclusion_node = document.create_div_unchecked();
                conclusion_node.append_child_unchecked(&conclusion_text);
                conclusion_node.append_child_unchecked(&create_phantom_height(document));
                conclusion_node.set_attribute_unchecked("class", "conclusion");

                let inference_text = document.create_text_node(&format!(
                    "split on {}",
                    char::try_from(variable).expect("must be a valid char")
                ));
                let inference_node = document.create_div_unchecked();
                inference_node.set_attribute_unchecked("class", "inference");
                inference_node.append_child_unchecked(&inference_text);

                let subproofs_toggle = document.create_div_unchecked();
                subproofs_toggle.set_attribute_unchecked("class", "subproofs-toggle");

                let internal_proof_node = document.create_div_unchecked();
                internal_proof_node.set_attribute_unchecked("class", "proof-node");
                internal_proof_node.append_child_unchecked(&conclusion_node);
                internal_proof_node.append_child_unchecked(&inference_node);
                internal_proof_node.append_child_unchecked(&subproofs_toggle);

                let proof_node = document.create_div_unchecked();
                proof_node.set_attribute_unchecked("class", "proof");
                proof_node.set_attribute_unchecked("data-inference-type", "split");
                proof_node.append_child_unchecked(&internal_proof_node);

                let proof_node_clone = proof_node.clone();

                if self.current_depth < self.max_depth {
                    // TODO: remove this repitition to callback
                    let zero_subproof_node = ProofView {
                        proof: *zero_proof,
                        current_depth: self.current_depth + 1,
                        max_depth: self.max_depth,
                    }
                    .render(document);
                    let successor_subproof_node = ProofView {
                        proof: *successor_proof,
                        current_depth: self.current_depth + 1,
                        max_depth: self.max_depth,
                    }
                    .render(document);
                    let subproofs_node = document.create_div_unchecked();
                    subproofs_node.set_attribute_unchecked("class", "subproofs");
                    subproofs_node.append_child_unchecked(&zero_subproof_node);
                    subproofs_node.append_child_unchecked(&successor_subproof_node);
                    proof_node.append_child_unchecked(&subproofs_node);
                    proof_node.set_attribute_unchecked("data-expanded", "");

                    let expand_button_callback = Closure::wrap(Box::new(move |_| {
                        proof_node_clone
                            .toggle_attribute("data-expanded")
                            .expect("toggle attribute should work");
                    })
                        as Box<dyn FnMut(Event)>);
                    subproofs_toggle
                        .add_event_listener_with_callback(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        )
                        .expect("add event listener should work");
                    // TODO: fix this leakage
                    expand_button_callback.forget();
                } else {
                    let expand_button_callback = Closure::wrap(Box::new(move |_| {
                        if let Ok(None) = proof_node_clone.query_selector(".subproofs") {
                            let document = unchecked_document();
                            let zero_subproof_node = ProofView {
                                proof: *zero_proof.clone(),
                                current_depth: 0,
                                max_depth: 4,
                            }
                            .render(&document);
                            let successor_subproof_node = ProofView {
                                proof: *successor_proof.clone(),
                                current_depth: 0,
                                max_depth: 4,
                            }
                            .render(&document);
                            let subproofs_node = document.create_div_unchecked();
                            subproofs_node.set_attribute_unchecked("class", "subproofs");
                            subproofs_node.append_child_unchecked(&zero_subproof_node);
                            subproofs_node.append_child_unchecked(&successor_subproof_node);
                            proof_node_clone.append_child_unchecked(&subproofs_node);
                        }

                        proof_node_clone
                            .toggle_attribute("data-expanded")
                            .expect("toggle attribute should work");
                    })
                        as Box<dyn FnMut(Event)>);
                    subproofs_toggle
                        .add_event_listener_with_callback(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        )
                        .expect("add event listener should work");
                    // TODO: fix this leakage
                    expand_button_callback.forget();
                }

                proof_node.into()
            }
        }
    }
}

enum ProofLeaf {
    SuccessorNonZero,
    FoundRoot,
    NotStrictlyMonomiallyComparable,
}

impl ProofLeaf {
    fn inference_type(&self) -> String {
        String::from(match self {
            ProofLeaf::SuccessorNonZero => "successor-non-zero",
            ProofLeaf::FoundRoot => "found-root",
            ProofLeaf::NotStrictlyMonomiallyComparable => "not-strictly-monomially-comparable",
        })
    }
}

impl Display for ProofLeaf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProofLeaf::SuccessorNonZero => write!(f, "✓"),
            ProofLeaf::FoundRoot => write!(f, "✘"),
            ProofLeaf::NotStrictlyMonomiallyComparable => {
                write!(f, "✘")
            }
        }
    }
}

fn render_proof_leaf(
    document: &Document,
    conclusion: &PolynomialDisequality,
    proof_leaf: ProofLeaf,
) -> Node {
    let conclusion = conclusion.reduced();
    let conclusion_text = document
        .create_element("span")
        .expect("create span should work");
    conclusion_text.set_inner_html(&format!(
        "{} ≠ {}",
        PolynomialDisplay {
            polynomial: &conclusion.left,
            variable_mapping: &|v| String::from(char::try_from(v).expect("must be a valid char")),
            number_of_largest_monomials: 1,
            number_of_smallest_monomials: 5,
            exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
        },
        PolynomialDisplay {
            polynomial: &conclusion.right,
            variable_mapping: &|v| String::from(char::try_from(v).expect("must be a valid char")),
            number_of_largest_monomials: 1,
            number_of_smallest_monomials: 5,
            exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
        },
    ));

    let conclusion_node = document.create_div_unchecked();
    conclusion_node.set_attribute_unchecked("class", "conclusion");
    conclusion_node.append_child_unchecked(&conclusion_text);
    conclusion_node.append_child_unchecked(&create_phantom_height(document));

    let inference_text = document.create_text_node(&proof_leaf.to_string());
    let inference_node = document.create_div_unchecked();
    inference_node.set_attribute_unchecked("class", "inference");
    inference_node.append_child_unchecked(&inference_text);

    let subproofs_toggle = document.create_div_unchecked();
    subproofs_toggle.set_attribute_unchecked("class", "subproofs-toggle");
    subproofs_toggle.append_child_unchecked(&create_phantom_height(document));

    let internal_proof_node = document.create_div_unchecked();
    internal_proof_node.set_attribute_unchecked("class", "proof-node");
    internal_proof_node.append_child_unchecked(&conclusion_node);
    internal_proof_node.append_child_unchecked(&inference_node);
    internal_proof_node.append_child_unchecked(&subproofs_toggle);

    let proof_node = document.create_div_unchecked();
    proof_node.set_attribute_unchecked("class", "proof");
    proof_node.set_attribute_unchecked("data-inference-type", &proof_leaf.inference_type());
    proof_node.set_attribute_unchecked("data-expanded", "");
    proof_node.append_child_unchecked(&internal_proof_node);

    proof_node.into()
}

fn create_phantom_height(document: &Document) -> Element {
    let phantom_height = document.create_element_unchecked("span");
    phantom_height.set_attribute_unchecked("class", "phantom-height");
    phantom_height.set_inner_html("M<sup>M</sup>");
    phantom_height
}
