use std::{cell::RefCell, pin::Pin, rc::Rc, task::Waker};

use futures::{pin_mut, Future};
use wasm_bindgen::{prelude::Closure, JsCast};

use crate::web_unchecked::window_unchecked;

struct SetTimeoutFuture {
    timeout_id: i32,
    waker: Rc<RefCell<Option<Waker>>>,
    completed: Rc<RefCell<bool>>,
}

impl SetTimeoutFuture {
    fn new(timeout: i32) -> Self {
        let waker: Rc<RefCell<Option<Waker>>> = Rc::new(RefCell::new(None));
        let completed: Rc<RefCell<bool>> = Rc::new(RefCell::new(false));

        let waker_clone = waker.clone();
        let completed_clone = completed.clone();
        let set_timeout_callback = Closure::once(Box::new(move || {
            *completed_clone.borrow_mut() = true;
            if let Some(waker) = waker_clone.borrow_mut().take() {
                waker.wake()
            }
        }) as Box<dyn FnMut()>);
        let timeout_id = window_unchecked()
            .set_timeout_with_callback_and_timeout_and_arguments_0(
                set_timeout_callback.as_ref().unchecked_ref(),
                timeout,
            )
            .expect("set timeout should work");
        set_timeout_callback.forget();

        Self {
            timeout_id,
            waker,
            completed,
        }
    }
}

impl Drop for SetTimeoutFuture {
    fn drop(&mut self) {
        window_unchecked().clear_timeout_with_handle(self.timeout_id);
    }
}

impl Future for SetTimeoutFuture {
    type Output = ();

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        if *self.completed.borrow() {
            std::task::Poll::Ready(())
        } else {
            self.waker.borrow_mut().replace(cx.waker().clone());
            std::task::Poll::Pending
        }
    }
}

pub(crate) async fn timeout(timeout: i32) {
    SetTimeoutFuture::new(timeout).await
}

pub(crate) enum Timeout<F>
where
    F: Future,
{
    FutureFirst(F::Output),
    TimeoutFirst(F),
}

pub(crate) async fn future_or_timeout<'a, F>(future: F, length: i32) -> Timeout<F>
where
    F: Future + Unpin,
{
    let timeout_future = timeout(length);
    pin_mut!(timeout_future);
    match futures::future::select(future, timeout_future).await {
        futures::future::Either::Left((result, _)) => Timeout::FutureFirst(result),
        futures::future::Either::Right((_, other)) => Timeout::TimeoutFirst(other),
    }
}
