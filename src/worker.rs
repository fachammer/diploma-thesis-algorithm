use std::{
    cell::RefCell,
    future::Future,
    ops::{Deref, DerefMut},
    rc::Rc,
    task::{Poll, Waker},
};

use futures::future::join_all;
use js_sys::Uint8Array;
use wasm_bindgen::{prelude::Closure, JsCast, JsValue};
use wasm_bindgen_futures::spawn_local;
use web_sys::{console, Event, MessageEvent, Worker, WorkerOptions};

use crate::{
    disequality::PolynomialDisequality,
    log::measure,
    proof_search::ProofInProgressSearchResult,
    web_unchecked::{EventTargetUnchecked, WorkerUnchecked},
    SearchProofRequest,
};

pub(crate) struct ProofSearchWorkerPool {
    pool: Vec<ProofSearchWorker>,
    desired_size: usize,
}

impl ProofSearchWorkerPool {
    pub(crate) async fn new(size: usize) -> Result<Self, Event> {
        assert!(size > 0);
        let worker_futures = (0..size).map(|_| ProofSearchWorker::new());
        let worker_results = join_all(worker_futures).await;

        let mut pool = Vec::with_capacity(size);
        for worker in worker_results {
            pool.push(worker?);
        }

        Ok(Self {
            pool,
            desired_size: size,
        })
    }
}
#[derive(Clone)]
pub(crate) struct ProofSearchWorkerPoolHandle {
    pool_handle: Rc<RefCell<ProofSearchWorkerPool>>,
}

pub(crate) struct ProofSearchWorkerHandle {
    pool_handle: ProofSearchWorkerPoolHandle,
    worker: Option<ProofSearchWorker>,
}

impl ProofSearchWorkerHandle {
    fn new(pool_handle: ProofSearchWorkerPoolHandle, worker: ProofSearchWorker) -> Self {
        Self {
            pool_handle,
            worker: Some(worker),
        }
    }
}

impl Deref for ProofSearchWorkerHandle {
    type Target = ProofSearchWorker;

    fn deref(&self) -> &Self::Target {
        self.worker.as_ref().expect("worker is not None since there are no constructors which make the worker None and the worker is never .changed, except in drop")
    }
}

impl DerefMut for ProofSearchWorkerHandle {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.worker.as_mut().expect("worker is not None since there are no constructors which make the worker None and the worker is never changed, except in drop")
    }
}

impl Drop for ProofSearchWorkerHandle {
    fn drop(&mut self) {
        if let Some(worker) = self.worker.take() {
            self.pool_handle.give_worker(worker);
        }
    }
}

impl ProofSearchWorkerPoolHandle {
    pub(crate) async fn take_worker(&mut self) -> Result<ProofSearchWorkerHandle, Event> {
        console::log_2(
            &"acquiring worker, before in pool:".into(),
            &self.pool_handle.borrow().pool.len().into(),
        );
        let worker = self.pool_handle.borrow_mut().pool.pop();

        let worker = match worker {
            Some(worker) => worker,
            None => {
                console::log_1(&"creating new worker".into());
                ProofSearchWorker::new().await?
            }
        };

        console::log_2(
            &"acquiring worker, after in pool:".into(),
            &self.pool_handle.borrow().pool.len().into(),
        );

        Ok(ProofSearchWorkerHandle::new(self.clone(), worker))
    }

    pub(crate) fn give_worker(&mut self, worker: ProofSearchWorker) {
        if self.pool_handle.borrow().pool.len() == self.pool_handle.borrow().desired_size {
            return;
        }

        match worker.status() {
            ProofSearchWorkerStatus::Ready => {
                // if worker is ready again, we can reuse it
                self.pool_handle.borrow_mut().pool.push(worker);
                console::log_2(
                    &"pushed worker, now in pool:".into(),
                    &self.pool_handle.borrow().pool.len().into(),
                );
            }
            ProofSearchWorkerStatus::Running | ProofSearchWorkerStatus::Errored => {
                // if worker is not ready, we create a new one in its place
                std::mem::drop(worker);
                let mut pool_handle = self.clone();
                spawn_local(async move {
                    if let Ok(worker) = ProofSearchWorker::new().await {
                        pool_handle.give_worker(worker);
                    }
                });
            }
        }
    }
}

impl From<Rc<RefCell<ProofSearchWorkerPool>>> for ProofSearchWorkerPoolHandle {
    fn from(pool_handle: Rc<RefCell<ProofSearchWorkerPool>>) -> Self {
        Self { pool_handle }
    }
}

pub(crate) enum ProofSearchWorkerStatus {
    Ready,
    Running,
    Errored,
}

pub(crate) struct ProofSearchWorker {
    pub(crate) worker: AsyncWorker,
    status: ProofSearchWorkerStatus,
}

impl ProofSearchWorker {
    pub(crate) async fn new() -> Result<Self, Event> {
        let mut worker_options = WorkerOptions::new();
        worker_options.type_(web_sys::WorkerType::Module);
        let worker =
            Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist");
        let worker = AsyncWorker { worker };
        let message = worker.post_message_response(&"initial".into()).await?;
        assert_eq!(message.data(), "ready");
        Ok(Self {
            worker,
            status: ProofSearchWorkerStatus::Ready,
        })
    }

    pub(crate) fn status(&self) -> &ProofSearchWorkerStatus {
        &self.status
    }

    pub(crate) async fn search_proof(
        &mut self,
        disequality: PolynomialDisequality,
        depth: u32,
        previous_variable: u32,
    ) -> Result<ProofInProgressSearchResult, Event> {
        self.status = ProofSearchWorkerStatus::Running;
        let search_proof_request = bincode::serialize(&SearchProofRequest {
            disequality,
            depth,
            previous_split_variable: previous_variable,
        })
        .expect("serialize should work");
        let search_proof_request = Uint8Array::from(&search_proof_request[..]);

        let message = match measure! {
            self
                .worker
                .post_message_response(&search_proof_request)
                .await
        } {
            Ok(message) => message,
            Err(error) => {
                self.status = ProofSearchWorkerStatus::Errored;
                return Err(error);
            }
        };

        let proof_result_buffer: Uint8Array = message
            .data()
            .try_into()
            .expect("message data should be an Uint8Array");
        let proof_result_buffer: &[u8] = &proof_result_buffer.to_vec();
        let result =
            measure! {bincode::deserialize(proof_result_buffer).expect("deserialize should work")};
        self.status = ProofSearchWorkerStatus::Ready;

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
        console::log_1(&"worker terminated".into());
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
