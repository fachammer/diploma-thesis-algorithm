use std::{cell::RefCell, fmt::Display, rc::Rc};

use disequality::TermDisequality;
use js_sys::encode_uri_component;
use serde::{Deserialize, Serialize};
use term::Term;
use wasm_bindgen::prelude::*;
use web_sys::{
    console, Document, Element, Event, HtmlElement, HtmlInputElement, InputEvent, MessageEvent,
    Node, Url,
};

use crate::{
    disequality::{self, PolynomialDisequality},
    polynomial::{ExponentDisplayStyle, Polynomial, PolynomialDisplay},
    proof::CompletePolynomialProof,
    proof_search::CompletePolynomialProofSearchResult,
    term,
    web_unchecked::{
        document_unchecked, window_unchecked, DocumentUnchecked, ElementUnchecked, NodeUnchecked,
        UrlUnchecked,
    },
    worker::ProofSearchWorker,
};

#[derive(Clone)]
struct UrlParameters {
    left_term: Option<String>,
    right_term: Option<String>,
    max_initial_proof_depth: Option<u32>,
    hide_intro: Option<bool>,
}

impl UrlParameters {
    fn from_url(url: Url) -> Self {
        let search_params = url.search_params();
        let left_term = search_params.get("left");
        let right_term = search_params.get("right");
        let max_initial_proof_depth = search_params
            .get("max-initial-proof-depth")
            .and_then(|m| m.parse::<u32>().ok());
        let hide_intro = search_params
            .get("hide-intro")
            .and_then(|h| h.parse::<bool>().ok());
        console::log_1(&hide_intro.into());

        Self {
            left_term,
            right_term,
            max_initial_proof_depth,
            hide_intro,
        }
    }
}

pub(crate) async fn setup() {
    let url = String::from(window_unchecked().location().to_locale_string());
    let parameters = UrlParameters::from_url(Url::new_unchecked(&url));

    let document = document_unchecked();
    let worker = ProofSearchWorker::new().await;
    let worker = Rc::new(RefCell::new(worker));

    update(worker.clone(), &parameters);

    let left_input = document.html_element_by_id_unchecked("left-term-input");
    left_input.set_text_content(Some(
        &parameters.left_term.clone().unwrap_or(String::from("x*x")),
    ));
    let parameters_clone = parameters.clone();
    let left_input_on_change: Closure<dyn Fn(InputEvent)> =
        oninput_handler(worker.clone(), parameters_clone);
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = document.html_element_by_id_unchecked("right-term-input");
    right_input.set_text_content(Some(
        &parameters.right_term.clone().unwrap_or(String::from("Sx")),
    ));
    let right_input_on_change = oninput_handler(worker, parameters.clone());
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    let hide_intro = parameters.hide_intro.unwrap_or(false);
    if hide_intro {
        document
            .html_element_by_id_unchecked("introduction")
            .set_attribute_unchecked("hidden", "true");
    }

    document
        .body_unchecked()
        .set_attribute_unchecked("data-webassembly-ready", "");
}

fn oninput_handler(
    worker: Rc<RefCell<ProofSearchWorker>>,
    parameters: UrlParameters,
) -> Closure<dyn Fn(InputEvent)> {
    Closure::wrap(Box::new(move |_| {
        update(worker.clone(), &parameters.clone());
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

struct TermView {
    left_term: HtmlInputElement,
    right_term: HtmlInputElement,
}

struct PolynomialView {
    root: HtmlElement,
    left: HtmlElement,
    right: HtmlElement,
}

struct ProofSearchStatusView {
    status_text: HtmlElement,
}

struct ProofView {
    root: HtmlElement,
}

struct ValidationMessagesView {
    root: HtmlElement,
    left_term: HtmlElement,
    right_term: HtmlElement,
}

struct UIElements {
    term_view: TermView,
    polynomial_view: PolynomialView,
    proof_search_status_view: ProofSearchStatusView,
    proof_view: ProofView,
    validation_messages: ValidationMessagesView,
}

impl UIElements {
    fn get_in(document: &Document) -> Self {
        Self {
            term_view: {
                TermView {
                    left_term: document.input_by_id_unchecked("left-term-input"),
                    right_term: document.input_by_id_unchecked("right-term-input"),
                }
            },
            polynomial_view: PolynomialView {
                root: document.html_element_by_id_unchecked("polynomial-view"),
                left: document.html_element_by_id_unchecked("left-polynomial"),
                right: document.html_element_by_id_unchecked("right-polynomial"),
            },
            proof_search_status_view: ProofSearchStatusView {
                status_text: document.html_element_by_id_unchecked("proof-search-status"),
            },
            proof_view: ProofView {
                root: document.html_element_by_id_unchecked("proof-view"),
            },
            validation_messages: ValidationMessagesView {
                root: document.html_element_by_id_unchecked("validation-messages"),
                left_term: document.html_element_by_id_unchecked("left-term-validation-message"),
                right_term: document.html_element_by_id_unchecked("right-term-validation-message"),
            },
        }
    }

    fn left_term_text(&self) -> String {
        self.term_view.left_term.text_content().unwrap_or_default()
    }

    fn right_term_text(&self) -> String {
        self.term_view.right_term.text_content().unwrap_or_default()
    }

    fn validate(&self) -> ValidationResult {
        validate(&self.left_term_text(), &self.right_term_text())
    }

    fn set_valid(&self) {
        self.term_view
            .left_term
            .set_attribute_unchecked("data-valid", "true");
        self.term_view
            .right_term
            .set_attribute_unchecked("data-valid", "true");
        self.validation_messages
            .root
            .set_attribute_unchecked("data-valid", "true");
        self.validation_messages
            .left_term
            .set_attribute_unchecked("data-valid", "true");
        self.validation_messages
            .right_term
            .set_attribute_unchecked("data-valid", "true");
        self.proof_search_status_view
            .status_text
            .set_attribute_unchecked("data-visible", "true");
        self.polynomial_view
            .root
            .set_attribute_unchecked("data-visible", "true");
        self.proof_view
            .root
            .set_attribute_unchecked("data-visible", "true");
    }

    fn set_invalid(&self, left_is_valid: bool, right_is_valid: bool) {
        self.validation_messages
            .root
            .set_attribute_unchecked("data-valid", "false");
        self.term_view
            .left_term
            .set_attribute_unchecked("data-valid", &left_is_valid.to_string());
        self.term_view
            .right_term
            .set_attribute_unchecked("data-valid", &right_is_valid.to_string());
        self.validation_messages
            .left_term
            .set_attribute_unchecked("data-valid", &left_is_valid.to_string());
        self.validation_messages
            .right_term
            .set_attribute_unchecked("data-valid", &right_is_valid.to_string());
        self.polynomial_view
            .root
            .set_attribute_unchecked("data-visible", "false");
        self.proof_search_status_view
            .status_text
            .set_attribute_unchecked("data-visible", "false");
        self.proof_view
            .root
            .set_attribute_unchecked("data-visible", "false");
    }

    fn update_proof_view(
        &self,
        document: &Document,
        proof_result: CompletePolynomialProofSearchResult,
        max_initial_proof_depth: u32,
    ) {
        let proof_view = &self.proof_view.root;
        proof_view.set_text_content(None);
        let proof_search_status = &self.proof_search_status_view.status_text;
        match proof_result {
            CompletePolynomialProofSearchResult::ProofFound(proof) => {
                proof_view.append_child_unchecked(
                    &ProofDisplay {
                        proof,
                        current_depth: 0,
                        max_depth: max_initial_proof_depth,
                    }
                    .render(document),
                );
                proof_search_status
                    .set_attribute_unchecked("data-proof-search-status", "found-proof");
                proof_search_status.set_text_content(Some("found proof"));
            }
            CompletePolynomialProofSearchResult::NoProofFound { attempt, reason } => {
                proof_view.append_child_unchecked(
                    &ProofDisplay {
                        proof: attempt,
                        current_depth: 0,
                        max_depth: max_initial_proof_depth,
                    }
                    .render(document),
                );
                let (reason_attribute_value, reason_text) = match reason {
                    crate::proof_search::NoProofFoundReason::NotStrictlyMonomiallyComparable {
                        ..
                    } => (
                        "not-strictly-monomially-comparable",
                        "no proof found: not strictly monomially comparable",
                    ),
                    crate::proof_search::NoProofFoundReason::ExistsRoot { .. } => {
                        ("found-root", "no proof found: found root")
                    }
                };
                proof_search_status
                    .set_attribute_unchecked("data-proof-search-status", reason_attribute_value);
                proof_search_status.set_text_content(Some(reason_text));
            }
        }
    }

    fn update(
        &self,
        document: &Document,
        worker: Rc<RefCell<ProofSearchWorker>>,
        url_parameters: &UrlParameters,
    ) {
        self.update_history(url_parameters);
        let validation_result = self.validate();

        match validation_result {
            ValidationResult::Valid {
                left_term: left,
                right_term: right,
            } => {
                self.set_valid();

                self.polynomial_view.left.set_text_content(None);
                let left_polynomial = Polynomial::from(left.clone());
                self.polynomial_view.left.set_text_content(None);
                self.polynomial_view
                    .left
                    .append_child_unchecked(&left_polynomial.render(document));

                let right_polynomial: Polynomial = Polynomial::from(right.clone());
                self.polynomial_view.right.set_text_content(None);
                self.polynomial_view
                    .right
                    .append_child_unchecked(&right_polynomial.render(document));

                let disequality = TermDisequality::from_terms(left, right);

                worker
                    .borrow()
                    .worker
                    .post_message(
                        &serde_wasm_bindgen::to_value(&SearchProof { disequality })
                            .expect("to value should work"),
                    )
                    .expect("post message should work");
                setup_proof_search_status_update_debounce(
                    &self.proof_search_status_view.status_text,
                );

                let max_initial_proof_depth = url_parameters.max_initial_proof_depth.unwrap_or(4);
                let worker_callback = Closure::wrap(Box::new(move |event: MessageEvent| {
                    let proof_result = extract_proof_result_from_worker_message(event);
                    let document = document_unchecked();
                    let ui_elements = UIElements::get_in(&document);
                    ui_elements.update_proof_view(&document, proof_result, max_initial_proof_depth);
                }) as Box<dyn FnMut(_)>);
                worker
                    .borrow()
                    .worker
                    .set_onmessage(Some(worker_callback.as_ref().unchecked_ref()));

                // TODO: fix this leakage
                worker_callback.forget();
            }
            ValidationResult::Invalid {
                left_is_valid,
                right_is_valid,
            } => self.set_invalid(left_is_valid, right_is_valid),
        };
    }

    fn update_history(&self, url_parameters: &UrlParameters) {
        let left_term_input = self.left_term_text();
        let right_term_input = self.right_term_text();
        let max_initial_depth_param = url_parameters
            .max_initial_proof_depth
            .map(|m| {
                format!(
                    "&max-initial-proof-depth={}",
                    encode_uri_component(&m.to_string())
                )
            })
            .unwrap_or_default();
        let hide_intro_param = url_parameters
            .hide_intro
            .map(|h| format!("&hide-intro={}", encode_uri_component(&h.to_string())))
            .unwrap_or_default();

        window_unchecked()
            .history()
            .expect("history should exist")
            .replace_state_with_url(
                &JsValue::TRUE,
                "",
                Some(&format!(
                    "?left={}&right={}{max_initial_depth_param}{hide_intro_param}",
                    encode_uri_component(&left_term_input),
                    encode_uri_component(&right_term_input)
                )),
            )
            .expect("push state should not fail");
    }
}

fn update(worker: Rc<RefCell<ProofSearchWorker>>, parameters: &UrlParameters) {
    let document = document_unchecked();
    let ui_elements = UIElements::get_in(&document);
    ui_elements.update(&document, worker, parameters);
}

fn extract_proof_result_from_worker_message(
    event: MessageEvent,
) -> CompletePolynomialProofSearchResult {
    let proof_result_pointer = event.data().as_f64().expect("data must be a number") as u32;
    let proof_result_pointer = proof_result_pointer as *mut CompletePolynomialProofSearchResult;
    *unsafe { Box::from_raw(proof_result_pointer) }
}

fn setup_proof_search_status_update_debounce(proof_search_status: &HtmlElement) {
    proof_search_status.set_attribute_unchecked("data-proof-search-status", "in-progress");
    let set_proof_search_status_callback = Closure::wrap(Box::new(|| {
        let document = document_unchecked();
        let proof_search_status = document.html_element_by_id_unchecked("proof-search-status");
        let proof_view = document.html_element_by_id_unchecked("proof-view");
        let proof_search_status_attribute = proof_search_status
            .get_attribute("data-proof-search-status")
            .unwrap_or_default();
        if proof_search_status_attribute == "in-progress" {
            proof_view.set_text_content(None);
            proof_search_status.set_text_content(Some("in progress..."));
        }
    }) as Box<dyn FnMut()>);
    let window = window_unchecked();
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

struct ProofDisplay {
    proof: CompletePolynomialProof,
    current_depth: u32,
    max_depth: u32,
}

impl RenderNode for ProofDisplay {
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
                        number_of_smallest_monomials: 3,
                        exponent_display_style: ExponentDisplayStyle::SuperscriptTag
                    },
                    PolynomialDisplay {
                        polynomial: &conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 3,
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
                let inference_tooltip = document.create_div_unchecked();
                inference_tooltip.set_attribute_unchecked("class", "tooltip");
                let inference_node = document.create_div_unchecked();
                inference_node.set_attribute_unchecked("class", "inference");
                inference_node.set_title("show subproofs");
                inference_node.append_child_unchecked(&inference_text);
                inference_node.append_child_unchecked(&inference_tooltip);

                let internal_proof_node = document.create_div_unchecked();
                internal_proof_node.set_attribute_unchecked("class", "proof-node");
                internal_proof_node.append_child_unchecked(&conclusion_node);
                internal_proof_node.append_child_unchecked(&inference_node);

                let proof_node = document.create_div_unchecked();
                proof_node.set_attribute_unchecked("class", "proof");
                proof_node.set_attribute_unchecked("data-inference-type", "split");
                proof_node.append_child_unchecked(&internal_proof_node);

                let proof_node_clone = proof_node.clone();
                let inference_node_clone = inference_node.clone();

                if self.current_depth < self.max_depth {
                    // TODO: remove this repitition to callback
                    let zero_subproof_node = ProofDisplay {
                        proof: *zero_proof,
                        current_depth: self.current_depth + 1,
                        max_depth: self.max_depth,
                    }
                    .render(document);
                    let successor_subproof_node = ProofDisplay {
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
                    inference_node.set_title("hide subproofs");

                    let expand_button_callback = Closure::wrap(Box::new(move |_| {
                        proof_node_clone
                            .toggle_attribute("data-expanded")
                            .expect("toggle attribute should work");
                        if proof_node_clone.has_attribute("data-expanded") {
                            inference_node_clone.set_title("hide subproofs");
                        } else {
                            inference_node_clone.set_title("show subproofs");
                        }
                    })
                        as Box<dyn FnMut(Event)>);
                    inference_node
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
                            let zero_subproof_node = ProofDisplay {
                                proof: *zero_proof.clone(),
                                current_depth: 0,
                                max_depth: 0,
                            }
                            .render(&document);
                            let successor_subproof_node = ProofDisplay {
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
                        if proof_node_clone.has_attribute("data-expanded") {
                            inference_node_clone.set_title("hide subproofs");
                        } else {
                            inference_node_clone.set_title("show subproofs");
                        }
                    })
                        as Box<dyn FnMut(Event)>);
                    inference_node
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
            number_of_smallest_monomials: 3,
            exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
        },
        PolynomialDisplay {
            polynomial: &conclusion.right,
            variable_mapping: &|v| String::from(char::try_from(v).expect("must be a valid char")),
            number_of_largest_monomials: 1,
            number_of_smallest_monomials: 3,
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

    let internal_proof_node = document.create_div_unchecked();
    internal_proof_node.set_attribute_unchecked("class", "proof-node");
    internal_proof_node.append_child_unchecked(&conclusion_node);
    internal_proof_node.append_child_unchecked(&inference_node);

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
