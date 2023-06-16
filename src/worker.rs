use std::{cell::RefCell, future::Future, rc::Rc, task::Poll};

use wasm_bindgen::{memory, prelude::Closure, JsCast};
use web_sys::{console, MessageEvent, Worker, WorkerOptions};

use crate::{
    disequality::TermDisequality, proof_search::CompletePolynomialProofSearchResult,
    ui::SearchProof,
};

#[derive(Clone)]
pub(crate) struct ProofSearchWorker {
    pub(crate) worker: Worker,
}

impl ProofSearchWorker {
    pub(crate) async fn new() -> Self {
        let mut worker_options = WorkerOptions::new();
        worker_options.type_(web_sys::WorkerType::Module);
        let worker =
            Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist");
        worker
            .post_message(&memory())
            .expect("post message memory should work");
        let message = WorkerMessage::new(&worker).await;
        assert_eq!(message.data(), "ready");
        Self { worker }
    }

    pub(crate) async fn search_proof(
        &self,
        disequality: TermDisequality,
    ) -> CompletePolynomialProofSearchResult {
        let search_proof_request = SearchProof { disequality };
        let search_proof_request =
            serde_wasm_bindgen::to_value(&search_proof_request).expect("to value should work");
        self.worker
            .post_message(&search_proof_request)
            .expect("post message should work");

        let message = WorkerMessage::new(&self.worker).await;
        let proof_result_pointer = message.data().as_f64().expect("data must be a number") as u32;
        let proof_result_pointer = proof_result_pointer as *mut CompletePolynomialProofSearchResult;
        *unsafe { Box::from_raw(proof_result_pointer) }
    }
}

struct WorkerMessage<'a> {
    worker: &'a Worker,
    message: Rc<RefCell<Option<MessageEvent>>>,
    worker_message_callback: Option<Closure<dyn FnMut(MessageEvent)>>,
}

impl<'a> WorkerMessage<'a> {
    fn new(worker: &'a Worker) -> WorkerMessage {
        Self {
            worker,
            message: Rc::new(RefCell::new(None)),
            worker_message_callback: None,
        }
    }
}

impl Drop for WorkerMessage<'_> {
    fn drop(&mut self) {
        if let Some(callback) = &self.worker_message_callback {
            self.worker
                .remove_event_listener_with_callback("message", callback.as_ref().unchecked_ref())
                .expect("remove event listener should work");
        }
    }
}

impl Future for WorkerMessage<'_> {
    type Output = MessageEvent;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        let message = self.message.borrow().clone();
        if let Some(message) = message {
            Poll::Ready(message)
        } else {
            if self.worker_message_callback.is_none() {
                let waker = cx.waker().clone();
                let message_clone = self.message.clone();
                let worker_callback = Closure::once(Box::new(move |event: MessageEvent| {
                    console::log_1(&"got ready message from async worker".into());
                    message_clone.borrow_mut().replace(event);
                    waker.wake();
                }) as Box<dyn FnOnce(_)>);
                self.worker
                    .add_event_listener_with_callback(
                        "message",
                        worker_callback.as_ref().unchecked_ref(),
                    )
                    .expect("add event listener should not fail");
                self.worker_message_callback = Some(worker_callback);
            }

            Poll::Pending
        }
    }
}
