use std::{cell::RefCell, fmt::Display, rc::Rc};

use disequality::TermDisequality;
use serde::{Deserialize, Serialize};
use term::Term;
use wasm_bindgen::prelude::*;
use web_sys::{console, window, Document, Element, Event, InputEvent, MessageEvent, Node, Worker};

use crate::{
    disequality::{self, PolynomialDisequality},
    polynomial::{ExponentDisplayStyle, Polynomial, PolynomialDisplay},
    proof::CompletePolynomialProof,
    proof_search::CompletePolynomialProofSearchResult,
    term,
    web_unchecked::{
        document_unchecked, unchecked_now, DocumentUnchecked, ElementUnchecked, NodeUnchecked,
    },
    worker::setup_worker,
};

pub(crate) fn setup() {
    let document = document_unchecked();
    let worker = setup_worker(update);

    let left_input = document.input_by_id_unchecked("left-term-input");
    let left_input_on_change = oninput_handler(worker.clone());
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = document.input_by_id_unchecked("right-term-input");
    let right_input_on_change = oninput_handler(worker);
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    document
        .body_unchecked()
        .set_attribute_unchecked("data-webassembly-ready", "");
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
    Valid {
        left_term: Term,
        right_term: Term,
    },
    Invalid {
        left_is_valid: bool,
        right_is_valid: bool,
    },
}

const VALID_VARIABLES: [char; 6] = ['u', 'v', 'w', 'x', 'y', 'z'];
fn validate(left_term_text: &str, right_term_text: &str) -> ValidationResult {
    let left_term: Result<Term, _> = left_term_text.parse();
    let right_term: Result<Term, _> = right_term_text.parse();
    match (left_term, right_term) {
        (Ok(left_term), Ok(right_term)) => {
            let valid_variables = VALID_VARIABLES.iter().map(|x| *x as u32).collect();
            let left_is_valid = left_term.free_varaiables().is_subset(&valid_variables);
            let right_is_valid = right_term.free_varaiables().is_subset(&valid_variables);
            if left_is_valid && right_is_valid {
                ValidationResult::Valid {
                    left_term,
                    right_term,
                }
            } else {
                ValidationResult::Invalid {
                    left_is_valid,
                    right_is_valid,
                }
            }
        }
        (left, right) => ValidationResult::Invalid {
            left_is_valid: left.is_ok(),
            right_is_valid: right.is_ok(),
        },
    }
}

fn update(worker: Rc<RefCell<Worker>>) {
    let window = window().expect("window must exist");
    let document = document_unchecked();
    let left_term_element = document.input_by_id_unchecked("left-term-input");
    let right_term_element = document.input_by_id_unchecked("right-term-input");
    let polynomial_view = document.html_element_by_id_unchecked("polynomial-view");
    let proof_view = document.html_element_by_id_unchecked("proof-view");
    let validation_messages_element = document.html_element_by_id_unchecked("validation-messages");
    let left_term_validation_message_element =
        document.html_element_by_id_unchecked("left-term-validation-message");
    let right_term_validation_message_element =
        document.html_element_by_id_unchecked("right-term-validation-message");
    let proof_search_status_view =
        document.html_element_by_id_unchecked("proof-search-status-view");
    let proof_search_status =
        document_unchecked().html_element_by_id_unchecked("proof-search-status");
    proof_search_status.set_attribute_unchecked("data-proof-search-status", "in-progress");
    let set_proof_search_status_callback = Closure::wrap(Box::new(|| {
        let document = document_unchecked();
        let proof_search_status = document.html_element_by_id_unchecked("proof-search-status");
        let proof_view = document.html_element_by_id_unchecked("proof-view");
        if proof_search_status
            .get_attribute("data-proof-search-status")
            .unwrap_or(String::default())
            == "in-progress"
        {
            proof_view.set_text_content(None);
            proof_search_status.set_text_content(Some("in progress..."));
        }
    }) as Box<dyn FnMut()>);
    if let Some(Ok(timeout_id)) = proof_search_status
        .get_attribute("data-timeout-id")
        .map(|s| s.parse::<i32>())
    {
        window.clear_timeout_with_handle(timeout_id);
    }
    let timeout_id = window
        .set_timeout_with_callback_and_timeout_and_arguments_0(
            set_proof_search_status_callback.as_ref().unchecked_ref(),
            50,
        )
        .expect("set timeout should work");
    proof_search_status.set_attribute_unchecked("data-timeout-id", &timeout_id.to_string());
    set_proof_search_status_callback.forget();

    let left_text_content = &left_term_element.text_content().unwrap_or_default();
    let right_text_content = &right_term_element.text_content().unwrap_or_default();
    let validation_result = validate(left_text_content, right_text_content);

    match validation_result {
        ValidationResult::Valid {
            left_term: left,
            right_term: right,
        } => {
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
                let document = document_unchecked();
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
                        proof_search_status
                            .set_attribute_unchecked("data-proof-search-status", "found-proof");
                        proof_search_status.set_text_content(Some("found proof"));
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
                        let (reason_attribute_value, reason_text) = match reason {
                            crate::proof_search::NoProofFoundReason::NotStrictlyMonomiallyComparable { .. } => ("not-strictly-monomially-comparable", "no proof found: is not smc"),
                            crate::proof_search::NoProofFoundReason::ExistsRoot { .. } => ("found-root", "no proof found: found root"),
                        };
                        proof_search_status.set_attribute_unchecked(
                            "data-proof-search-status",
                            reason_attribute_value,
                        );
                        proof_search_status.set_text_content(Some(reason_text));
                    }
                }
                let end_time = unchecked_now();
                console::log_1(&format!("elapsed time: {} ms", end_time - start_time).into());
            }) as Box<dyn FnMut(_)>);
            worker
                .borrow()
                .set_onmessage(Some(worker_callback.as_ref().unchecked_ref()));

            // TODO: fix this leakage
            worker_callback.forget();
        }
        ValidationResult::Invalid {
            left_is_valid: left,
            right_is_valid: right,
        } => {
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
                        number_of_smallest_monomials: 2,
                        exponent_display_style: ExponentDisplayStyle::SuperscriptTag
                    },
                    PolynomialDisplay {
                        polynomial: &conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 2,
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
                            let document = document_unchecked();
                            let zero_subproof_node = ProofView {
                                proof: *zero_proof.clone(),
                                current_depth: 0,
                                max_depth: 0,
                            }
                            .render(&document);
                            let successor_subproof_node = ProofView {
                                proof: *successor_proof.clone(),
                                current_depth: 0,
                                max_depth: 0,
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
            number_of_smallest_monomials: 2,
            exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
        },
        PolynomialDisplay {
            polynomial: &conclusion.right,
            variable_mapping: &|v| String::from(char::try_from(v).expect("must be a valid char")),
            number_of_largest_monomials: 1,
            number_of_smallest_monomials: 2,
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
