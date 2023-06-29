use std::{cell::RefCell, rc::Rc};

use futures::{
    pin_mut, select_biased,
    stream::{AbortHandle, Abortable, Aborted},
    FutureExt,
};
use js_sys::encode_uri_component;

use term::Term;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::spawn_local;
use web_sys::{console, Document, HtmlElement, HtmlInputElement, InputEvent, Node, Url};

use crate::{
    callback::callback_async,
    disequality::PolynomialDisequality,
    log::measure,
    polynomial::{ExponentDisplayStyle, Polynomial, PolynomialDisplay},
    proof_search::ProofInProgressSearchResult,
    term,
    timeout::timeout,
    web_unchecked::{
        document_unchecked, window_unchecked, DocumentUnchecked, ElementUnchecked, NodeUnchecked,
        UrlUnchecked,
    },
    worker::ProofSearchWorkerPool,
};

use self::proof_display::ProofDisplay;

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
    let document = document_unchecked();
    document
        .body_unchecked()
        .set_attribute_unchecked("data-webassembly-ready", "");

    let url = String::from(window_unchecked().location().to_locale_string());
    let parameters = UrlParameters::from_url(Url::new_unchecked(&url));

    let worker_abort_handle = Rc::new(RefCell::new(None));
    let worker_pool = Rc::new(RefCell::new(
        measure! {ProofSearchWorkerPool::new(4).await}.expect("worker pool must be ok"),
    ));

    let left_input = document.html_element_by_id_unchecked("left-term-input");
    left_input.set_text_content(Some(
        &parameters.left_term.clone().unwrap_or(String::from("x*x")),
    ));
    let parameters_clone = parameters.clone();
    let worker_abort_handle_clone = worker_abort_handle.clone();
    let worker_pool_clone = worker_pool.clone();
    let left_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(move |_| {
        spawn_local(update(
            parameters_clone.clone(),
            worker_abort_handle_clone.clone(),
            worker_pool_clone.clone(),
        ));
    }));
    left_input.set_oninput(Some(left_input_on_change.as_ref().unchecked_ref()));
    left_input_on_change.forget();

    let right_input = document.html_element_by_id_unchecked("right-term-input");
    right_input.set_text_content(Some(
        &parameters.right_term.clone().unwrap_or(String::from("Sx")),
    ));
    let parameters_clone = parameters.clone();
    let worker_abort_handle_clone = worker_abort_handle.clone();
    let worker_pool_clone = worker_pool.clone();
    let right_input_on_change: Closure<dyn Fn(InputEvent)> = Closure::wrap(Box::new(move |_| {
        spawn_local(update(
            parameters_clone.clone(),
            worker_abort_handle_clone.clone(),
            worker_pool_clone.clone(),
        ));
    }));
    right_input.set_oninput(Some(right_input_on_change.as_ref().unchecked_ref()));
    right_input_on_change.forget();

    let hide_intro = parameters.hide_intro.unwrap_or(false);
    if hide_intro {
        document
            .html_element_by_id_unchecked("introduction")
            .set_attribute_unchecked("hidden", "true");
    }

    update(parameters, worker_abort_handle, worker_pool).await;
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
        proof_result: ProofInProgressSearchResult,
        max_initial_proof_depth: u32,
        worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    ) {
        let proof_view = &self.proof_view.root;
        proof_view.set_text_content(None);
        let proof_search_status = &self.proof_search_status_view.status_text;
        match proof_result {
            ProofInProgressSearchResult::ProofFound { conclusion, proof } => {
                proof_view.append_child_unchecked(
                    &ProofDisplay {
                        proof,
                        current_depth: 0,
                        max_depth: max_initial_proof_depth,
                        conclusion,
                        previous_split_variable: u32::MAX,
                        worker_pool,
                    }
                    .render(document),
                );
                proof_search_status
                    .set_attribute_unchecked("data-proof-search-status", "found-proof");
                proof_search_status.set_text_content(Some("found proof"));
            }
            ProofInProgressSearchResult::NoProofFound {
                conclusion,
                attempt,
                reason,
            } => {
                proof_view.append_child_unchecked(
                    &ProofDisplay {
                        proof: attempt,
                        current_depth: 0,
                        max_depth: max_initial_proof_depth,
                        conclusion,
                        previous_split_variable: u32::MAX,
                        worker_pool,
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

    async fn update(
        &self,
        document: &Document,
        url_parameters: &UrlParameters,
        worker_abort_handle: Rc<RefCell<Option<AbortHandle>>>,
        worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    ) {
        if let Some(handle) = worker_abort_handle.borrow().as_ref() {
            handle.abort();
        }
        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let _ = worker_abort_handle.borrow_mut().insert(abort_handle);

        self.update_history(url_parameters);
        let validation_result = self.validate();

        let (left, right) = match validation_result {
            ValidationResult::Invalid {
                left_is_valid,
                right_is_valid,
            } => {
                self.set_invalid(left_is_valid, right_is_valid);
                return;
            }
            ValidationResult::Valid {
                left_term,
                right_term,
            } => {
                self.set_valid();
                (left_term, right_term)
            }
        };

        let left_polynomial = Polynomial::from(left.clone());
        let right_polynomial: Polynomial = Polynomial::from(right.clone());
        let disequality = PolynomialDisequality::from_polynomials_reduced(
            left_polynomial.clone(),
            right_polynomial.clone(),
        );

        self.polynomial_view.left.set_text_content(None);
        self.polynomial_view
            .left
            .append_child_unchecked(&left_polynomial.render(document));

        let right_polynomial: Polynomial = Polynomial::from(right.clone());
        self.polynomial_view.right.set_text_content(None);
        self.polynomial_view
            .right
            .append_child_unchecked(&right_polynomial.render(document));

        let mut worker_pool_borrow_mut = worker_pool.borrow_mut();
        let search_proof_result = worker_pool_borrow_mut.search_proof(disequality, 10, u32::MAX);

        let abortable_search_proof_result =
            Abortable::new(search_proof_result, abort_registration).fuse();
        pin_mut!(abortable_search_proof_result);

        let show_progress_timeout = timeout(50).fuse();
        pin_mut!(show_progress_timeout);

        let cancel_button_pressed =
            callback_async(&self.proof_search_status_view.status_text, "click").fuse();
        pin_mut!(cancel_button_pressed);

        loop {
            select_biased! {
                result = abortable_search_proof_result => match result {
                    Ok(Ok(result)) => {
                        let max_initial_proof_depth = url_parameters.max_initial_proof_depth.unwrap_or(4);
                        self.update_proof_view(document, result, max_initial_proof_depth, worker_pool.clone());
                        break;
                    },
                    Ok(Err(_)) => {
                        self.proof_search_status_view
                            .status_text
                            .set_attribute_unchecked("data-proof-search-status", "error");
                        self.proof_search_status_view
                            .status_text
                            .set_text_content(Some("error while searching proof"));
                        break;
                    },
                    Err(Aborted) => {
                        break;
                    }
                },
                _ = cancel_button_pressed => {
                    self.proof_search_status_view
                        .status_text
                        .set_attribute_unchecked("data-proof-search-status", "cancelled");
                    self.proof_search_status_view
                        .status_text
                        .set_text_content(Some("cancelled"));
                    break;
                },
                _ = show_progress_timeout => {
                    self.proof_view.root.set_text_content(None);
                    self.proof_search_status_view
                        .status_text
                        .set_text_content(Some("in progress..."));
                    continue;
                }
            }
        }
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

async fn update(
    parameters: UrlParameters,
    worker_abort_handle: Rc<RefCell<Option<AbortHandle>>>,
    worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
) {
    let document = document_unchecked();
    let ui_elements = UIElements::get_in(&document);
    ui_elements
        .update(&document, &parameters, worker_abort_handle, worker_pool)
        .await;
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

pub(crate) mod proof_display {
    use std::{cell::RefCell, fmt::Display, rc::Rc};

    use wasm_bindgen::{prelude::Closure, JsCast};
    use wasm_bindgen_futures::spawn_local;
    use web_sys::{Document, Element, Event, HtmlElement, Node};

    use crate::{
        disequality::PolynomialDisequality,
        polynomial::{ExponentDisplayStyle, PolynomialDisplay},
        proof_search::ProofInProgress,
        web_unchecked::{
            document_unchecked, DocumentUnchecked, ElementUnchecked, EventTargetUnchecked,
            NodeUnchecked,
        },
        worker::ProofSearchWorkerPool,
    };

    use super::RenderNode;

    pub(crate) struct ProofDisplay {
        pub(crate) proof: ProofInProgress,
        pub(crate) current_depth: u32,
        pub(crate) max_depth: u32,
        pub(crate) conclusion: PolynomialDisequality,
        pub(crate) previous_split_variable: u32,
        pub(crate) worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    }

    impl RenderNode for ProofDisplay {
        fn render(self, document: &Document) -> Node {
            match self.proof {
                ProofInProgress::SuccessorNonZero => {
                    render_proof_leaf(document, &self.conclusion, ProofLeaf::SuccessorNonZero)
                }
                ProofInProgress::FoundRoot => {
                    render_proof_leaf(document, &self.conclusion, ProofLeaf::FoundRoot)
                }
                ProofInProgress::NotStrictlyMonomiallyComparable => render_proof_leaf(
                    document,
                    &self.conclusion,
                    ProofLeaf::NotStrictlyMonomiallyComparable,
                ),
                ProofInProgress::Hole => {
                    let node: HtmlElement =
                        render_proof_leaf(document, &self.conclusion, ProofLeaf::InProgress)
                            .unchecked_into();
                    let node_clone = node.clone();
                    spawn_local(async move {
                        let document = document_unchecked();
                        let mut worker_pool = self.worker_pool.borrow_mut();
                        let result = worker_pool
                            .search_proof(self.conclusion, 5, self.previous_split_variable)
                            .await
                            .expect("result must be ok");
                        match result {
                            crate::proof_search::ProofInProgressSearchResult::ProofFound {
                                conclusion,
                                proof,
                            }
                            | crate::proof_search::ProofInProgressSearchResult::NoProofFound {
                                conclusion,
                                attempt: proof,
                                ..
                            } => {
                                let proof_display = ProofDisplay {
                                    proof,
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion,
                                    previous_split_variable: self.previous_split_variable,
                                    worker_pool: self.worker_pool.clone(),
                                };
                                let proof_node = proof_display.render(&document);
                                node_clone
                                    .replace_with_with_node_1(&proof_node)
                                    .expect("replace with with node should work");
                            }
                        }
                    });
                    node.into()
                }
                ProofInProgress::Split {
                    variable,
                    zero_proof,
                    successor_proof,
                } => {
                    let conclusion = self.conclusion.reduced();
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
                        let zero_subproof_node = ProofDisplay {
                            proof: *zero_proof,
                            current_depth: self.current_depth + 1,
                            max_depth: self.max_depth,
                            conclusion: self.conclusion.at_variable_zero(variable),
                            previous_split_variable: variable,
                            worker_pool: self.worker_pool.clone(),
                        }
                        .render(document);
                        let successor_subproof_node = ProofDisplay {
                            proof: *successor_proof,
                            current_depth: self.current_depth + 1,
                            max_depth: self.max_depth,
                            conclusion: self.conclusion.into_at_variable_plus_one(variable),
                            previous_split_variable: variable,
                            worker_pool: self.worker_pool,
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
                        inference_node.add_event_listener_with_callback_unchecked(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        );
                        expand_button_callback.forget();
                    } else {
                        let expand_button_callback = Closure::wrap(Box::new(move |_| {
                            if let Ok(None) = proof_node_clone.query_selector(".subproofs") {
                                let document = document_unchecked();
                                let zero_subproof_node = ProofDisplay {
                                    proof: *zero_proof.clone(),
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion: self.conclusion.at_variable_zero(variable),
                                    previous_split_variable: variable,
                                    worker_pool: self.worker_pool.clone(),
                                }
                                .render(&document);
                                let successor_subproof_node = ProofDisplay {
                                    proof: *successor_proof.clone(),
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion: self
                                        .conclusion
                                        .clone()
                                        .into_at_variable_plus_one(variable),
                                    previous_split_variable: variable,
                                    worker_pool: self.worker_pool.clone(),
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
                        inference_node.add_event_listener_with_callback_unchecked(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        );
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
        InProgress,
    }

    impl ProofLeaf {
        fn inference_type(&self) -> String {
            String::from(match self {
                ProofLeaf::SuccessorNonZero => "successor-non-zero",
                ProofLeaf::FoundRoot => "found-root",
                ProofLeaf::NotStrictlyMonomiallyComparable => "not-strictly-monomially-comparable",
                ProofLeaf::InProgress => "in-progress",
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
                ProofLeaf::InProgress => {
                    write!(f, "...")
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
                variable_mapping: &|v| String::from(
                    char::try_from(v).expect("must be a valid char")
                ),
                number_of_largest_monomials: 1,
                number_of_smallest_monomials: 3,
                exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
            },
            PolynomialDisplay {
                polynomial: &conclusion.right,
                variable_mapping: &|v| String::from(
                    char::try_from(v).expect("must be a valid char")
                ),
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
}

mod complete_polynomial_proof_display {
    use std::fmt::Display;

    use wasm_bindgen::{prelude::Closure, JsCast};
    use web_sys::{Document, Element, Event, Node};

    use crate::{
        disequality::PolynomialDisequality,
        polynomial::{ExponentDisplayStyle, PolynomialDisplay},
        proof::CompletePolynomialProof,
        web_unchecked::{
            document_unchecked, DocumentUnchecked, ElementUnchecked, EventTargetUnchecked,
            NodeUnchecked,
        },
    };

    use super::RenderNode;

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
                        inference_node.add_event_listener_with_callback_unchecked(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        );
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
                        inference_node.add_event_listener_with_callback_unchecked(
                            "click",
                            expand_button_callback.as_ref().unchecked_ref(),
                        );
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
                variable_mapping: &|v| String::from(
                    char::try_from(v).expect("must be a valid char")
                ),
                number_of_largest_monomials: 1,
                number_of_smallest_monomials: 3,
                exponent_display_style: ExponentDisplayStyle::SuperscriptTag,
            },
            PolynomialDisplay {
                polynomial: &conclusion.right,
                variable_mapping: &|v| String::from(
                    char::try_from(v).expect("must be a valid char")
                ),
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
}
