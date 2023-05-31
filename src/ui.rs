use std::{cell::RefCell, rc::Rc};

use disequality::TermDisequality;
use serde::{Deserialize, Serialize};
use term::Term;
use wasm_bindgen::{memory, prelude::*};
use web_sys::{
    console, window, Document, Element, HtmlElement, HtmlInputElement, InputEvent, MessageEvent,
    Node, Window, Worker,
};

use crate::{
    disequality::{self, PolynomialDisequality},
    polynomial::{Polynomial, PolynomialDisplay},
    proof::{CompletePolynomialProof, Skeleton},
    proof_search::ProofAttempt,
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
    let polynomial_view = document.html_element_by_id_unchecked("polynomial-view");
    let proof_view = document.html_element_by_id_unchecked("proof-view");

    let worker_clone = worker.clone();
    let worker_callback = Closure::wrap(Box::new(move |event: MessageEvent| {
        console::log_1(&"got ready message".into());
        assert_eq!(event.data(), "ready");
        worker_clone.borrow().set_onmessage(None);

        update(
            &document,
            worker_clone.clone(),
            left_input.clone(),
            right_input.clone(),
            polynomial_view.clone(),
            proof_view.clone(),
        );
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
        let document = unchecked_document();
        let left_term_element = document.input_by_id_unchecked("left-term-input");
        let right_term_element = document.input_by_id_unchecked("right-term-input");
        let polynomial_view = document.html_element_by_id_unchecked("polynomial-view");
        let proof_view = document.html_element_by_id_unchecked("proof-view");

        update(
            &document,
            worker.clone(),
            left_term_element,
            right_term_element,
            polynomial_view,
            proof_view,
        );
    }))
}

#[derive(Serialize, Deserialize)]
pub struct SearchProof {
    pub(crate) disequality: TermDisequality,
}

fn update(
    document: &Document,
    worker: Rc<RefCell<Worker>>,
    left_term_element: HtmlInputElement,
    right_term_element: HtmlInputElement,
    polynomial_view: HtmlElement,
    proof_view: HtmlElement,
) {
    let left: Result<Term, _> = left_term_element.value().parse();
    let right: Result<Term, _> = right_term_element.value().parse();

    match (left, right) {
        (Ok(left), Ok(right)) => {
            left_term_element
                .set_attribute("data-valid", "true")
                .expect("set attribute should not fail");
            right_term_element
                .set_attribute("data-valid", "true")
                .expect("set attribute should not fail");
            polynomial_view
                .set_attribute("data-visible", "true")
                .expect("set attribute should not fail");
            proof_view
                .set_attribute("data-visible", "true")
                .expect("set attribute should not fail");

            let left_polynomial_view = document.html_element_by_id_unchecked("left-polynomial");
            left_polynomial_view.set_text_content(None);
            let left_polynomial = Polynomial::from(left.clone());
            left_polynomial_view.append_child_unchecked(&left_polynomial.render(document));

            let right_polynomial_view = document.html_element_by_id_unchecked("right-polynomial");
            right_polynomial_view.set_text_content(None);
            let right_polynomial = Polynomial::from(right.clone());
            right_polynomial_view.append_child_unchecked(&right_polynomial.render(document));

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
                    Box::from_raw(
                        proof_result_pointer as *mut Result<CompletePolynomialProof, ProofAttempt>,
                    )
                };

                match *proof_result {
                    Ok(proof) => {
                        proof_view.append_child_unchecked(&proof.render(&document));
                    }
                    Err(proof_attempt) => {
                        let proof_attempt = serde_wasm_bindgen::to_value(&proof_attempt)
                            .expect("serialize must succeed");
                        console::log_2(&"proof attempt: ".into(), &proof_attempt)
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
        (left, right) => {
            left_term_element
                .set_attribute("data-valid", &format!("{}", left.is_ok()))
                .expect("set attribute should not fail");
            right_term_element
                .set_attribute("data-valid", &format!("{}", right.is_ok()))
                .expect("set attribute should not fail");
            polynomial_view
                .set_attribute("data-visible", "false")
                .expect("set attribute should not fail");
            proof_view
                .set_attribute("data-visible", "false")
                .expect("set attribute should not fail");
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

impl RenderNode for Polynomial {
    fn render(&self, document: &Document) -> Node {
        document
            .create_text_node(&format!(
                "{}",
                PolynomialDisplay {
                    polynomial: self,
                    variable_mapping: &|v| String::from(
                        char::try_from(v).expect("variable must be a valid char value")
                    ),
                    number_of_largest_monomials: 5,
                    number_of_smallest_monomials: 5
                }
            ))
            .into()
    }
}

struct ProofTreeView<'a> {
    skeleton: &'a Skeleton,
    polynomial_conclusion: PolynomialDisequality,
}

impl<'a> RenderNode for ProofTreeView<'a> {
    fn render(&self, document: &Document) -> Node {
        match self.skeleton {
            crate::proof::Skeleton::SuccessorNonZero => document
                .create_text_node(&format!(
                    "{} ≠ {}: successor non zero",
                    PolynomialDisplay {
                        polynomial: &self.polynomial_conclusion.left,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                    PolynomialDisplay {
                        polynomial: &self.polynomial_conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                ))
                .into(),
            crate::proof::Skeleton::Split {
                variable,
                zero_skeleton,
                successor_skeleton,
            } => {
                let node = document.create_element_unchecked("span");
                node.append_child_unchecked(&document.create_text_node(&format!(
                    "{} ≠ {}: split on {}",
                    PolynomialDisplay {
                        polynomial: &self.polynomial_conclusion.left,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                    PolynomialDisplay {
                        polynomial: &self.polynomial_conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                    char::try_from(*variable).expect("must be a valid char")
                )));

                let list = document.create_element_unchecked("ul");
                node.append_child_unchecked(&list);

                let left_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&left_item);

                let left_node = ProofTreeView {
                    skeleton: &zero_skeleton.clone(),
                    polynomial_conclusion: self
                        .polynomial_conclusion
                        .at_variable_zero(*variable)
                        .reduce(),
                }
                .render(document);
                left_item.append_child_unchecked(&left_node);

                let right_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&right_item);

                let right_node = ProofTreeView {
                    skeleton: &successor_skeleton.clone(),
                    polynomial_conclusion: self
                        .polynomial_conclusion
                        .clone()
                        .into_at_variable_plus_one(*variable)
                        .reduce(),
                }
                .render(document);
                right_item.append_child_unchecked(&right_node);

                node.into()
            }
        }
    }
}

impl RenderNode for CompletePolynomialProof {
    fn render(&self, document: &Document) -> Node {
        match self {
            crate::proof::CompletePolynomialProof::SuccessorNonZero { conclusion } => {
                console::log_1(
                    &format!(
                        "left polynomial monomials: {}",
                        conclusion.left.0.support().count()
                    )
                    .into(),
                );

                document
                    .create_text_node(&format!(
                        "{} ≠ {}: successor non zero",
                        PolynomialDisplay {
                            polynomial: &conclusion.left,
                            variable_mapping: &|v| String::from(
                                char::try_from(v).expect("must be a valid char")
                            ),
                            number_of_largest_monomials: 1,
                            number_of_smallest_monomials: 5
                        },
                        PolynomialDisplay {
                            polynomial: &conclusion.right,
                            variable_mapping: &|v| String::from(
                                char::try_from(v).expect("must be a valid char")
                            ),
                            number_of_largest_monomials: 1,
                            number_of_smallest_monomials: 5
                        },
                    ))
                    .into()
            }
            crate::proof::CompletePolynomialProof::Split {
                variable,
                conclusion,
                zero_proof,
                successor_proof,
            } => {
                let node = document.create_element_unchecked("span");
                node.append_child_unchecked(&document.create_text_node(&format!(
                    "{} ≠ {}: split on {}",
                    PolynomialDisplay {
                        polynomial: &conclusion.left,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                    PolynomialDisplay {
                        polynomial: &conclusion.right,
                        variable_mapping: &|v| String::from(
                            char::try_from(v).expect("must be a valid char")
                        ),
                        number_of_largest_monomials: 1,
                        number_of_smallest_monomials: 5
                    },
                    char::try_from(*variable).expect("must be a valid char")
                )));

                let list = document.create_element_unchecked("ul");
                node.append_child_unchecked(&list);

                let left_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&left_item);

                let left_node = zero_proof.render(document);
                left_item.append_child_unchecked(&left_node);

                let right_item = document.create_element_unchecked("li");
                list.append_child_unchecked(&right_item);

                let right_node = successor_proof.render(document);
                right_item.append_child_unchecked(&right_node);

                node.into()
            }
        }
    }
}
