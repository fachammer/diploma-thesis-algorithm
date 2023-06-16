use std::{cell::RefCell, future::Future, rc::Rc, task::Poll};

use wasm_bindgen::{memory, prelude::Closure, JsCast};
use web_sys::{console, MessageEvent, Worker, WorkerOptions};

pub(crate) struct ProofSearchWorker {
    pub(crate) worker: Worker,
}

impl ProofSearchWorker {
    pub(crate) async fn new() -> Self {
        let mut worker_options = WorkerOptions::new();
        worker_options.type_(web_sys::WorkerType::Module);
        let worker =
            Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist");
        WorkerReady::new(&worker).await;
        Self { worker }
    }
}

struct WorkerReady<'a> {
    worker: &'a Worker,
    is_ready: Rc<RefCell<bool>>,
    worker_message_callback: Option<Closure<dyn FnMut(MessageEvent)>>,
}

impl<'a> WorkerReady<'a> {
    fn new(worker: &'a Worker) -> WorkerReady {
        Self {
            worker,
            is_ready: Rc::new(RefCell::new(false)),
            worker_message_callback: None,
        }
    }
}

impl Drop for WorkerReady<'_> {
    fn drop(&mut self) {
        if let Some(callback) = &self.worker_message_callback {
            self.worker
                .remove_event_listener_with_callback("message", callback.as_ref().unchecked_ref())
                .expect("remove event listener should work");
        }
    }
}

impl Future for WorkerReady<'_> {
    type Output = ();

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        let is_ready = *self.is_ready.borrow();
        if is_ready {
            Poll::Ready(())
        } else {
            if self.worker_message_callback.is_none() {
                let waker = cx.waker().clone();
                let is_ready_clone = self.is_ready.clone();
                let worker_callback = Closure::once(Box::new(move |event: MessageEvent| {
                    assert_eq!(event.data(), "ready");
                    console::log_1(&"got ready message from async worker".into());
                    *is_ready_clone.borrow_mut() = true;
                    waker.wake();
                }) as Box<dyn FnOnce(_)>);
                self.worker
                    .add_event_listener_with_callback(
                        "message",
                        worker_callback.as_ref().unchecked_ref(),
                    )
                    .expect("add event listener should not fail");
                self.worker_message_callback = Some(worker_callback);

                self.worker
                    .post_message(&memory())
                    .expect("post message memory should work");
            }

            Poll::Pending
        }
    }
}
