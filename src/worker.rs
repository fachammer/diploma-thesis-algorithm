use std::{cell::RefCell, rc::Rc};

use wasm_bindgen::{memory, prelude::Closure, JsCast};
use web_sys::{console, MessageEvent, Worker, WorkerOptions};

pub(crate) fn setup_worker<F: Fn(Rc<RefCell<Worker>>) + 'static>(
    onready: F,
) -> Rc<RefCell<Worker>> {
    let mut worker_options = WorkerOptions::new();
    worker_options.type_(web_sys::WorkerType::Module);
    let worker = Rc::new(RefCell::new(
        Worker::new_with_options("./worker.js", &worker_options).expect("worker must exist"),
    ));

    let worker_clone = worker.clone();
    let worker_callback = Closure::wrap(Box::new(move |event: MessageEvent| {
        assert_eq!(event.data(), "ready");
        console::log_1(&"got ready message from worker".into());
        worker_clone.borrow().set_onmessage(None);

        onready(worker_clone.clone());
    }) as Box<dyn Fn(_)>);
    worker
        .borrow()
        .set_onmessage(Some(worker_callback.as_ref().unchecked_ref()));
    worker
        .borrow()
        .post_message(&memory())
        .expect("post message memory should work");
    // TODO: handle this leakage
    worker_callback.forget();
    worker
}
