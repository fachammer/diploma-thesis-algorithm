use std::{
    cell::RefCell,
    future::Future,
    rc::Rc,
    task::{Poll, Waker},
};

use wasm_bindgen::{memory, prelude::Closure, JsCast, JsValue};
use web_sys::{console, MessageEvent, Worker, WorkerOptions};

use crate::{
    disequality::TermDisequality,
    proof_search::CompletePolynomialProofSearchResult,
    ui::SearchProof,
    web_unchecked::{EventTargetUnchecked, WorkerUnchecked},
};

pub(crate) struct ProofSearchWorker {
    pub(crate) worker: AsyncWorker,
}

impl ProofSearchWorker {
    pub(crate) async fn new() -> Self {
        let mut worker_options = WorkerOptions::new();
        worker_options.type_(web_sys::WorkerType::Module);
        let worker =
            Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist");
        let worker = AsyncWorker { worker };
        let message = worker.post_message_response(&memory()).await;
        assert_eq!(message.data(), "ready");
        Self { worker }
    }

    pub(crate) async fn search_proof(
        &self,
        disequality: TermDisequality,
    ) -> CompletePolynomialProofSearchResult {
        let search_proof_request = serde_wasm_bindgen::to_value(&SearchProof { disequality })
            .expect("to value should work");

        let message = self
            .worker
            .post_message_response(&search_proof_request)
            .await;
        let proof_result_pointer = message.data().as_f64().expect("data must be a number") as u32;
        let proof_result_pointer = proof_result_pointer as *mut CompletePolynomialProofSearchResult;
        *unsafe { Box::from_raw(proof_result_pointer) }
    }
}

pub(crate) struct AsyncWorker {
    worker: Worker,
}

impl AsyncWorker {
    async fn post_message_response(&self, message: &JsValue) -> MessageEvent {
        self.worker.post_message_unchecked(message);
        WorkerMessageFuture::new(&self.worker).await
    }
}

impl Drop for AsyncWorker {
    fn drop(&mut self) {
        self.worker.terminate();
    }
}

struct WorkerMessageFuture<'a> {
    worker: &'a Worker,
    message: Rc<RefCell<Option<MessageEvent>>>,
    message_callback: Closure<dyn FnMut(MessageEvent)>,
    waker: Rc<RefCell<Option<Waker>>>,
}

impl<'a> WorkerMessageFuture<'a> {
    fn new(worker: &'a Worker) -> Self {
        let message = Rc::new(RefCell::new(None));
        let message_clone = message.clone();
        let waker: Rc<RefCell<Option<Waker>>> = Rc::new(RefCell::new(None));
        let waker_clone = waker.clone();
        let message_callback = Closure::once(Box::new(move |message: MessageEvent| {
            console::log_2(&"got message from async worker".into(), &message);
            message_clone.borrow_mut().replace(message);
            if let Some(waker) = waker_clone.borrow_mut().take() {
                waker.wake();
            };
        }) as Box<dyn FnOnce(_)>);
        worker.add_event_listener_with_callback_unchecked(
            "message",
            message_callback.as_ref().unchecked_ref(),
        );
        Self {
            worker,
            message,
            message_callback,
            waker,
        }
    }
}

impl Drop for WorkerMessageFuture<'_> {
    fn drop(&mut self) {
        self.worker.remove_event_listener_with_callback_unchecked(
            "message",
            self.message_callback.as_ref().unchecked_ref(),
        )
    }
}

impl Future for WorkerMessageFuture<'_> {
    type Output = MessageEvent;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        context: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        if let Some(message) = self.message.borrow_mut().take() {
            Poll::Ready(message)
        } else {
            self.waker.borrow_mut().replace(context.waker().clone());
            Poll::Pending
        }
    }
}
