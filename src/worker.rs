use std::{
    cell::RefCell,
    future::Future,
    rc::Rc,
    task::{Poll, Waker},
};

use js_sys::Uint8Array;
use wasm_bindgen::{prelude::Closure, JsCast, JsValue};
use web_sys::{console, Event, MessageEvent, Worker, WorkerOptions};

use crate::{
    disequality::TermDisequality,
    log::measure,
    proof_search::ProofSearchResult,
    ui::SearchProof,
    web_unchecked::{EventTargetUnchecked, WorkerUnchecked},
};

pub(crate) struct ProofSearchWorker {
    pub(crate) worker: AsyncWorker,
}

impl ProofSearchWorker {
    pub(crate) async fn new() -> Result<Self, Event> {
        let mut worker_options = WorkerOptions::new();
        worker_options.type_(web_sys::WorkerType::Module);
        let worker =
            Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist");
        let worker = AsyncWorker { worker };
        let message = worker
            .post_message_response(&"awaiting ready".into())
            .await?;
        assert_eq!(message.data(), "ready");
        Ok(Self { worker })
    }

    pub(crate) async fn search_proof(
        &self,
        disequality: TermDisequality,
    ) -> Result<ProofSearchResult, Event> {
        let search_proof_request =
            bincode::serialize(&SearchProof { disequality }).expect("serialize should work");
        let search_proof_request = Uint8Array::from(&search_proof_request[..]);

        let message = self
            .worker
            .post_message_response(&search_proof_request)
            .await?;

        let proof_result_buffer: Uint8Array = message
            .data()
            .try_into()
            .expect("message data should be an Uint8Array");
        let proof_result_buffer: &[u8] = &proof_result_buffer.to_vec();
        let result =
            measure! {bincode::deserialize(proof_result_buffer).expect("deserialize should work")};
        Ok(result)
    }
}

pub(crate) struct AsyncWorker {
    worker: Worker,
}

impl AsyncWorker {
    async fn post_message_response(&self, message: &JsValue) -> Result<MessageEvent, Event> {
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
    error: Rc<RefCell<Option<Event>>>,
    message_callback: Closure<dyn FnMut(MessageEvent)>,
    error_callback: Closure<dyn FnMut(Event)>,
    waker: Rc<RefCell<Option<Waker>>>,
}

impl<'a> WorkerMessageFuture<'a> {
    fn new(worker: &'a Worker) -> Self {
        let waker: Rc<RefCell<Option<Waker>>> = Rc::new(RefCell::new(None));
        let message = Rc::new(RefCell::new(None));
        let error = Rc::new(RefCell::new(None));

        let waker_clone = waker.clone();
        let message_clone = message.clone();
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

        let waker_clone = waker.clone();
        let error_clone = error.clone();
        let error_callback = Closure::once(Box::new(move |error: Event| {
            console::log_2(&"got error from async worker".into(), &error);
            error_clone.borrow_mut().replace(error);
            if let Some(waker) = waker_clone.borrow_mut().take() {
                waker.wake();
            };
        }) as Box<dyn FnOnce(_)>);
        worker.add_event_listener_with_callback_unchecked(
            "error",
            error_callback.as_ref().unchecked_ref(),
        );

        Self {
            worker,
            message,
            message_callback,
            error_callback,
            waker,
            error,
        }
    }
}

impl Drop for WorkerMessageFuture<'_> {
    fn drop(&mut self) {
        self.worker.remove_event_listener_with_callback_unchecked(
            "message",
            self.message_callback.as_ref().unchecked_ref(),
        );
        self.worker.remove_event_listener_with_callback_unchecked(
            "error",
            self.error_callback.as_ref().unchecked_ref(),
        );
    }
}

impl Future for WorkerMessageFuture<'_> {
    type Output = Result<MessageEvent, Event>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        context: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        if let Some(error) = self.error.borrow_mut().take() {
            Poll::Ready(Err(error))
        } else if let Some(message) = self.message.borrow_mut().take() {
            Poll::Ready(Ok(message))
        } else {
            self.waker.borrow_mut().replace(context.waker().clone());
            Poll::Pending
        }
    }
}
