use std::{cell::RefCell, future::pending, mem, rc::Rc};

use futures::{
    never::Never,
    pin_mut, select_biased,
    stream::{unfold, AbortHandle, AbortRegistration, Abortable, Aborted, StreamExt},
    FutureExt, Stream,
};
use js_sys::encode_uri_component;

use term::Term;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::spawn_local;
use web_sys::{Document, Event, HtmlElement, Node, Url};

use crate::{
    callback::callback_async,
    disequality::{PolynomialDisequality, TermDisequality},
    log::{measure, now},
    polynomial::{ExponentDisplayStyle, Polynomial, PolynomialDisplay},
    proof_search::ProofInProgressSearchResult,
    term,
    timeout::timeout,
    ui::proof_search_status::InProgress,
    web_unchecked::{
        document_unchecked, window_unchecked, DocumentUnchecked, ElementUnchecked, NodeUnchecked,
        UrlUnchecked,
    },
    worker::{ProofSearchWorker, ProofSearchWorkerPool},
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
    let url_parameters = UrlParameters::from_url(Url::new_unchecked(&url));

    let worker_pool = Rc::new(RefCell::new(
        measure! {ProofSearchWorkerPool::new(4).await}.expect("worker pool must be ok"),
    ));
    let ui_elements = UIElements::get_in(&document);

    let hide_intro = url_parameters.hide_intro.unwrap_or(false);
    if hide_intro {
        ui_elements
            .introduction
            .set_attribute_unchecked("hidden", "true");
    }

    let left_input = url_parameters.left_term.as_deref().unwrap_or("x*x");
    let right_input = url_parameters.right_term.as_deref().unwrap_or("Sx");
    ui_elements.term_view.set_inputs(left_input, right_input);

    let borrowed_term_view = ui_elements.term_view;
    let inputs = borrowed_term_view.current_and_future_inputs();
    pin_mut!(inputs);
    let main_loop_abort_handle: Rc<RefCell<Option<AbortHandle>>> = Rc::new(RefCell::new(None));

    while let Some((left_term_input, right_term_input)) = inputs.next().await {
        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let previous_abort_handle = main_loop_abort_handle.borrow_mut().replace(abort_handle);
        if let Some(handle) = previous_abort_handle {
            handle.abort();
        }

        let worker_pool = worker_pool.clone();
        let document = document.clone();
        let url_parameters = url_parameters.clone();
        let ui_elements = UIElements::get_in(&document);
        spawn_local(async move {
            let main_loop = MainLoop {
                worker_pool: worker_pool.clone(),
                document: document.clone(),
                ui_elements: UIElements::get_in(&document),
                url_parameters: url_parameters.clone(),
                left_term_input,
                right_term_input,
            };

            match main_loop.run(abort_registration).await {
                UiAction::ShowInProgress => {
                    todo!()
                }
                UiAction::ShowFinished { result, duration } => {
                    ui_elements.proof_search_completed(
                        &document,
                        &url_parameters,
                        worker_pool,
                        duration,
                        result,
                    );
                }
                UiAction::ShowErrored => todo!(),
                UiAction::ShowCancelled { duration } => {
                    ui_elements
                        .proof_search_status_view
                        .set_cancelled(&document, duration);
                }
                UiAction::ShowInvalid {
                    left_is_valid,
                    right_is_valid,
                } => ui_elements.set_invalid(left_is_valid, right_is_valid),
                UiAction::DoNothing => {}
            };
        });
    }
}

struct MainLoop {
    worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    document: Document,
    ui_elements: UIElements,
    url_parameters: UrlParameters,
    left_term_input: String,
    right_term_input: String,
}

enum UiAction {
    ShowInProgress,
    ShowFinished {
        result: Result<ProofInProgressSearchResult, Event>,
        duration: f64,
    },
    ShowErrored,
    ShowCancelled {
        duration: f64,
    },
    ShowInvalid {
        left_is_valid: bool,
        right_is_valid: bool,
    },
    DoNothing,
}

impl MainLoop {
    async fn run(self, abort_registration: AbortRegistration) -> UiAction {
        self.ui_elements.update_history(
            &self.left_term_input,
            &self.right_term_input,
            &self.url_parameters,
        );

        let term_disequality = match self.validate_terms() {
            Ok(term_disequality) => term_disequality,
            Err((left_is_valid, right_is_valid)) => {
                return UiAction::ShowInvalid {
                    left_is_valid,
                    right_is_valid,
                };
            }
        };
        let disequality = PolynomialDisequality::from(term_disequality).reduce();
        self.show_polynomial_disequality(&disequality);

        let proof_search_start_time = now();
        let abort_signal = Abortable::new(pending::<Never>(), abort_registration).fuse();
        pin_mut!(abort_signal);
        let (inner_abort_handle, inner_abort_registration) = AbortHandle::new_pair();
        let abortable_search_proof_result = search_proof_up_to_depth_abortable(
            self.worker_pool.clone(),
            disequality,
            10,
            u32::MAX,
            inner_abort_registration,
        )
        .fuse();
        pin_mut!(abortable_search_proof_result);

        let duration = || now() - proof_search_start_time;

        let in_progress = select_biased! {
            _ = abort_signal => {
                inner_abort_handle.abort();
                abortable_search_proof_result.await.expect_err("should return Err since abort handle was triggered");
                return UiAction::DoNothing;
            }
            result = abortable_search_proof_result => {
                let Ok(result) = result else {
                    return UiAction::DoNothing;
                };

                return UiAction::ShowFinished {result, duration: duration()};
            },
            _ = timeout(15).fuse() => {
                self.ui_elements.proof_view.root.set_text_content(None);
                self
                    .ui_elements
                    .proof_search_status_view
                    .set_in_progress(&self.document, proof_search_start_time)
            }
        };

        select_biased! {
            _ = abort_signal => {
                inner_abort_handle.abort();
                abortable_search_proof_result.await.expect_err("should return Err since abort handle was triggered");
                UiAction::DoNothing
            }
            result = abortable_search_proof_result => {
                let Ok(result) = result else {
                    return UiAction::DoNothing;
                };

                UiAction::ShowFinished {result, duration: duration()}
            },
            _ = callback_async(in_progress.cancel_button(), "click").fuse() => {
                inner_abort_handle.abort();
                abortable_search_proof_result.await.expect_err("should return Err since abort handle was triggered");

                UiAction::ShowCancelled {duration: duration()}
            },
        }
    }

    fn show_polynomial_disequality(&self, disequality: &PolynomialDisequality) {
        self.ui_elements
            .polynomial_view
            .set_polynomial_disequality(disequality);
    }

    fn validate_terms(&self) -> Result<TermDisequality, (bool, bool)> {
        let validation_result = validate(&self.left_term_input, &self.right_term_input);
        let (left, right) = match validation_result {
            ValidationResult::Invalid {
                left_is_valid,
                right_is_valid,
            } => {
                return Err((left_is_valid, right_is_valid));
            }
            ValidationResult::Valid {
                left_term,
                right_term,
            } => {
                self.ui_elements.set_valid();
                (left_term, right_term)
            }
        };
        Ok(TermDisequality::from_terms(left, right))
    }
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
    left_term: HtmlElement,
    right_term: HtmlElement,
}

impl TermView {
    fn get_current_input(&self) -> (String, String) {
        let left_input = self.left_term.text_content().unwrap_or_default();
        let right_input = self.right_term.text_content().unwrap_or_default();
        (left_input, right_input)
    }
    fn current_and_future_inputs(&self) -> impl Stream<Item = (String, String)> + '_ {
        futures::stream::once(async { self.get_current_input() }).chain(unfold(
            (),
            move |_| async move {
                let next_left_input = callback_async(&self.left_term, "input").fuse();
                pin_mut!(next_left_input);
                let next_right_input = callback_async(&self.right_term, "input").fuse();
                pin_mut!(next_right_input);
                select_biased! {
                    _ = next_left_input => Some((self.get_current_input(), ())),
                    _ = next_right_input => Some((self.get_current_input(), ())),
                }
            },
        ))
    }

    fn set_inputs(&self, left: &str, right: &str) {
        self.left_term.set_text_content(Some(left));
        self.right_term.set_text_content(Some(right));
    }
}

struct PolynomialView {
    document: Document,
    root: HtmlElement,
    left: HtmlElement,
    right: HtmlElement,
}

impl PolynomialView {
    fn set_polynomial_disequality(&self, disequality: &PolynomialDisequality) {
        self.left
            .replace_children_with_node_1(&disequality.left.clone().render(&self.document));
        self.right
            .replace_children_with_node_1(&disequality.right.clone().render(&self.document));
    }
}

struct ProofSearchStatusView {
    root: HtmlElement,
    parent: HtmlElement,
}

impl ProofSearchStatusView {
    fn set_in_progress(&self, document: &Document, proof_search_start_time: f64) -> InProgress {
        let root = document.clone_template_by_id_unchecked("proof-search-in-progress-status-view");
        let cancel_button = root.query_selector_unchecked("#cancel-button");
        let duration_text_element = root.query_selector_unchecked("#duration-text");

        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let parent = self.root.clone();
        spawn_local(async move {
            let abort_message =
                Abortable::new(std::future::pending::<Never>(), abort_registration).fuse();
            pin_mut!(abort_message);

            parent.set_attribute_unchecked("data-proof-search-status", "in-progress");
            parent.replace_children_with_node_1(&root);

            loop {
                let duration = now() - proof_search_start_time;
                let duration_text = format_duration(duration);
                duration_text_element.set_text_content(Some(&format!(
                    "proof search is running for {duration_text}"
                )));

                select_biased! {
                    _ = abort_message => break,
                    _ = timeout(8).fuse() => {},
                };
            }
        });

        InProgress::new(abort_handle, cancel_button)
    }

    fn set_errored(&self, document: &Document, proof_search_duration: f64) {
        self.root
            .set_attribute_unchecked("data-proof-search-status", "errored");
        let formatted_duration = format_duration(proof_search_duration);
        let root = document.clone_template_by_id_unchecked("proof-search-errored-status-view");
        let duration_text = root.query_selector_unchecked("#duration-text");
        duration_text.set_text_content(Some(&formatted_duration));
        self.root.replace_children_with_node_1(&root);
    }

    fn set_cancelled(&self, document: &Document, proof_search_duration: f64) {
        let formatted_duration = format_duration(proof_search_duration);
        let root = document.clone_template_by_id_unchecked("proof-search-cancelled-status-view");
        let duration_text = root.query_selector_unchecked("#duration-text");
        duration_text.set_text_content(Some(&formatted_duration));
        self.root
            .set_attribute_unchecked("data-proof-search-status", "cancelled");
        self.root.replace_children_with_node_1(&root);
    }

    fn set_found_root(&self, duration: f64, document: &Document) {
        let formatted_duration = format_duration(duration);
        self.root
            .set_attribute_unchecked("data-proof-search-status", "found-root");
        let root = document
            .clone_template_by_id_unchecked("proof-search-unsuccessful-found-root-status-view");
        let duration_text = root.query_selector_unchecked("#duration-text");
        duration_text.set_text_content(Some(&formatted_duration));
        self.root.replace_children_with_node_1(&root);
    }

    fn set_not_strictly_monomially_comparable(&self, duration: &f64, document: &Document) {
        let formatted_duration = format_duration(*duration);
        self.root.set_attribute_unchecked(
            "data-proof-search-status",
            "not-strictly-monomially-comparable",
        );
        let root = document.clone_template_by_id_unchecked(
            "proof-search-unsuccessful-not-strictly-monomially-comparable-status-view",
        );
        let duration_text = root.query_selector_unchecked("#duration-text");
        duration_text.set_text_content(Some(&formatted_duration));
        self.root.replace_children_with_node_1(&root);
    }

    fn set_found_proof(&self, document: &Document, duration: f64) {
        let formatted_duration = format_duration(duration);
        self.root
            .set_attribute_unchecked("data-proof-search-status", "found-proof");
        let root = document.clone_template_by_id_unchecked("proof-search-successful-status-view");
        let duration_text = root.query_selector_unchecked("#duration-text");
        duration_text.set_text_content(Some(&formatted_duration));
        self.root.replace_children_with_node_1(&root);
    }
}

mod proof_search_status {
    use futures::stream::AbortHandle;
    use web_sys::{Element, EventTarget};

    pub(crate) struct InProgress {
        abort_handle: AbortHandle,
        cancel_button: Element,
    }

    impl InProgress {
        pub(crate) fn new(abort_handle: AbortHandle, cancel_button: Element) -> Self {
            Self {
                abort_handle,
                cancel_button,
            }
        }

        pub(crate) fn cancel_button(&self) -> &EventTarget {
            &self.cancel_button
        }
    }

    impl Drop for InProgress {
        fn drop(&mut self) {
            self.abort_handle.abort();
        }
    }
}

struct ProofView {
    root: HtmlElement,
}

struct ValidationMessagesView {
    root: HtmlElement,
    left_term: HtmlElement,
    right_term: HtmlElement,
}

fn format_duration(duration_in_millis: f64) -> String {
    assert!(duration_in_millis >= 0.0);
    match duration_in_millis {
        d if (0.0..1_000.0).contains(&d) => format!("{d:.1} ms"),
        d if (1_000.0..(60.0 * 1_000.0)).contains(&d) => format!("{:.2} s", d / 1000.0),
        d => {
            let minutes = (d / (60.0 * 1_000.0)).floor();
            let remaining_seconds = (d - minutes * (60.0 * 1_000.0)) / 1_000.0;
            format!("{}m {}s", minutes as u32, remaining_seconds as u32)
        }
    }
}

struct UIElements {
    term_view: TermView,
    polynomial_view: PolynomialView,
    proof_search_status_view: ProofSearchStatusView,
    proof_view: ProofView,
    validation_messages: ValidationMessagesView,
    introduction: HtmlElement,
}

impl UIElements {
    fn get_in(document: &Document) -> Self {
        Self {
            term_view: {
                TermView {
                    left_term: document.html_element_by_id_unchecked("left-term-input"),
                    right_term: document.html_element_by_id_unchecked("right-term-input"),
                }
            },
            polynomial_view: PolynomialView {
                root: document.html_element_by_id_unchecked("polynomial-view"),
                left: document.html_element_by_id_unchecked("left-polynomial"),
                right: document.html_element_by_id_unchecked("right-polynomial"),
                document: document.clone(),
            },
            proof_search_status_view: ProofSearchStatusView {
                parent: document.html_element_by_id_unchecked("proof-search-status-view"),
                root: document.html_element_by_id_unchecked("proof-search-status"),
            },
            proof_view: ProofView {
                root: document.html_element_by_id_unchecked("proof-view"),
            },
            validation_messages: ValidationMessagesView {
                root: document.html_element_by_id_unchecked("validation-messages"),
                left_term: document.html_element_by_id_unchecked("left-term-validation-message"),
                right_term: document.html_element_by_id_unchecked("right-term-validation-message"),
            },
            introduction: document.html_element_by_id_unchecked("introduction"),
        }
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
            .parent
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
            .parent
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
        duration: f64,
    ) {
        let proof_view = &self.proof_view.root;
        proof_view.set_text_content(None);
        match proof_result {
            ProofInProgressSearchResult::ProofFound { conclusion, proof } => {
                let proof_display = ProofDisplay {
                    proof: &proof,
                    current_depth: 0,
                    max_depth: max_initial_proof_depth,
                    conclusion,
                    previous_split_variable: u32::MAX,
                    worker_pool,
                }
                .render(document);
                proof_view.append_child_unchecked(&proof_display);
                self.proof_search_status_view
                    .set_found_proof(document, duration);
            }
            ProofInProgressSearchResult::NoProofFound {
                conclusion,
                attempt,
                reason,
            } => {
                let proof_display = ProofDisplay {
                    proof: &attempt,
                    current_depth: 0,
                    max_depth: max_initial_proof_depth,
                    conclusion,
                    previous_split_variable: u32::MAX,
                    worker_pool,
                }
                .render(document);
                proof_view.append_child_unchecked(&proof_display);
                match reason {
                    crate::proof_search::NoProofFoundReason::NotStrictlyMonomiallyComparable {
                        ..
                    } => {
                        self.proof_search_status_view
                            .set_not_strictly_monomially_comparable(&duration, document);
                    }
                    crate::proof_search::NoProofFoundReason::ExistsRoot { .. } => {
                        self.proof_search_status_view
                            .set_found_root(duration, document);
                    }
                };
            }
        }
    }

    fn proof_search_completed(
        &self,
        document: &Document,
        url_parameters: &UrlParameters,
        worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
        duration: f64,
        result: Result<ProofInProgressSearchResult, Event>,
    ) {
        match result {
            Ok(result) => {
                self.update_proof_view(
                    document,
                    result,
                    url_parameters.max_initial_proof_depth.unwrap_or(4),
                    worker_pool,
                    duration,
                );
            }
            Err(_) => {
                self.proof_search_status_view
                    .set_errored(document, duration);
            }
        }
    }

    fn update_history(
        &self,
        left_term_input: &str,
        right_term_input: &str,
        url_parameters: &UrlParameters,
    ) {
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
                    encode_uri_component(left_term_input),
                    encode_uri_component(right_term_input)
                )),
            )
            .expect("push state should not fail");
    }
}

trait RenderNode {
    fn render(&self, document: &Document) -> Node;
}

impl RenderNode for Polynomial {
    fn render(&self, document: &Document) -> Node {
        let node = document
            .create_element("span")
            .expect("create span element should succeed");
        node.set_inner_html(&format!(
            "{}",
            PolynomialDisplay {
                polynomial: self,
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

async fn search_proof_up_to_depth_abortable(
    worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    disequality: PolynomialDisequality,
    depth: u32,
    previous_split_variable: u32,
    abort_registration: AbortRegistration,
) -> Result<Result<ProofInProgressSearchResult, Event>, Aborted> {
    let worker = worker_pool.as_ref().borrow_mut().pop_worker();
    let worker = match worker {
        Some(worker) => worker,
        None => match ProofSearchWorker::new().await {
            Ok(worker) => worker,
            Err(error) => return Ok(Err(error)),
        },
    };

    let abortable_task = Abortable::new(
        worker.search_proof(disequality, depth, previous_split_variable),
        abort_registration,
    );

    let result = abortable_task.await;
    match result {
        Ok(Ok(_)) => {
            // if worker succeeds, reuse it by giving it back to the pool
            worker_pool.as_ref().borrow_mut().push_worker(worker);
        }
        Err(Aborted) | Ok(Err(_)) => {
            spawn_local(async move {
                // if worker errored or was aborted, we cannot reuse it since it most likely encountered
                // an out of memory error or is long-running
                // therefore we drop the worker and put a new worker in its place
                mem::drop(worker);
                if let Ok(new_worker) = ProofSearchWorker::new().await {
                    worker_pool.as_ref().borrow_mut().push_worker(new_worker);
                }
                // if we cannot create a worker right now,
                // we try again when searching for a proof the next time
                // however, creating a worker most likely won't fail
            });
        }
    }

    result
}

pub(crate) mod proof_display {
    use std::{cell::RefCell, fmt::Display, rc::Rc};

    use futures::stream::AbortHandle;
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

    use super::{search_proof_up_to_depth_abortable, RenderNode};

    pub(crate) struct ProofDisplay<'a> {
        pub(crate) proof: &'a ProofInProgress,
        pub(crate) current_depth: u32,
        pub(crate) max_depth: u32,
        pub(crate) conclusion: PolynomialDisequality,
        pub(crate) previous_split_variable: u32,
        pub(crate) worker_pool: Rc<RefCell<ProofSearchWorkerPool>>,
    }

    impl<'a> RenderNode for ProofDisplay<'a> {
        fn render(&self, document: &Document) -> Node {
            match &self.proof {
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
                    let worker_pool_clone = self.worker_pool.clone();
                    let conclusion_clone = self.conclusion.clone();
                    let previous_split_variable = self.previous_split_variable;
                    spawn_local(async move {
                        let document = document_unchecked();
                        let (_, abort_registration) = AbortHandle::new_pair();
                        let result = search_proof_up_to_depth_abortable(
                            worker_pool_clone.clone(),
                            conclusion_clone,
                            5,
                            previous_split_variable,
                            abort_registration,
                        )
                        .await
                        .expect("result must be ok")
                        .expect("is not aborted");

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
                                    proof: &proof,
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion,
                                    previous_split_variable,
                                    worker_pool: worker_pool_clone,
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
                        char::try_from(*variable).expect("must be a valid char")
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
                            proof: zero_proof.as_ref(),
                            current_depth: self.current_depth + 1,
                            max_depth: self.max_depth,
                            conclusion: self.conclusion.at_variable_zero(*variable),
                            previous_split_variable: *variable,
                            worker_pool: self.worker_pool.clone(),
                        }
                        .render(document);
                        let successor_subproof_node = ProofDisplay {
                            proof: successor_proof.as_ref(),
                            current_depth: self.current_depth + 1,
                            max_depth: self.max_depth,
                            conclusion: self
                                .conclusion
                                .clone()
                                .into_at_variable_plus_one(*variable),
                            previous_split_variable: *variable,
                            worker_pool: self.worker_pool.clone(),
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
                        let zero_proof = zero_proof.clone();
                        let successor_proof = successor_proof.clone();
                        let worker_pool = self.worker_pool.clone();
                        let conclusion = self.conclusion.clone();
                        let variable = *variable;
                        let expand_button_callback = Closure::wrap(Box::new(move |_| {
                            if let Ok(None) = proof_node_clone.query_selector(".subproofs") {
                                let document = document_unchecked();

                                let zero_subproof_node = ProofDisplay {
                                    proof: zero_proof.as_ref(),
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion: conclusion.at_variable_zero(variable),
                                    previous_split_variable: variable,
                                    worker_pool: worker_pool.clone(),
                                }
                                .render(&document);
                                let successor_subproof_node = ProofDisplay {
                                    proof: successor_proof.as_ref(),
                                    current_depth: 0,
                                    max_depth: 0,
                                    conclusion: conclusion
                                        .clone()
                                        .into_at_variable_plus_one(variable),
                                    previous_split_variable: variable,
                                    worker_pool: worker_pool.clone(),
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
