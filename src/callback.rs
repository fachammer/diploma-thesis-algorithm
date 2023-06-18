use std::{cell::RefCell, pin::Pin, rc::Rc, task::Waker};

use futures::{pin_mut, Future};
use wasm_bindgen::{prelude::Closure, JsCast};
use web_sys::EventTarget;

use crate::web_unchecked::EventTargetUnchecked;

struct CallbackFuture<'a> {
    waker: Rc<RefCell<Option<Waker>>>,
    completed: Rc<RefCell<bool>>,
    event_target: &'a EventTarget,
    event: &'a str,
    callback: Closure<dyn FnMut()>,
}

impl<'a> CallbackFuture<'a> {
    fn new(event_target: &'a EventTarget, event: &'a str) -> Self {
        let waker: Rc<RefCell<Option<Waker>>> = Rc::new(RefCell::new(None));
        let completed: Rc<RefCell<bool>> = Rc::new(RefCell::new(false));

        let waker_clone = waker.clone();
        let completed_clone = completed.clone();
        let callback = Closure::once(Box::new(move || {
            *completed_clone.borrow_mut() = true;
            if let Some(waker) = waker_clone.borrow_mut().take() {
                waker.wake();
            }
        }) as Box<dyn FnMut()>);
        event_target
            .add_event_listener_with_callback_unchecked(event, callback.as_ref().unchecked_ref());

        Self {
            waker,
            completed,
            event_target,
            event,
            callback,
        }
    }
}

impl Drop for CallbackFuture<'_> {
    fn drop(&mut self) {
        self.event_target
            .remove_event_listener_with_callback_unchecked(
                self.event,
                self.callback.as_ref().unchecked_ref(),
            )
    }
}

impl Future for CallbackFuture<'_> {
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

pub(crate) async fn callback_async(event_target: &EventTarget, event: &str) {
    CallbackFuture::new(event_target, event).await
}

pub(crate) enum Callback<F>
where
    F: Future,
{
    FutureFirst(F::Output),
    CallbackFirst(F),
}

pub(crate) async fn future_or_callback<F>(
    future: F,
    event_target: &EventTarget,
    event: &str,
) -> Callback<F>
where
    F: Future + Unpin,
{
    let callback_future = callback_async(event_target, event);
    pin_mut!(callback_future);
    match futures::future::select(future, callback_future).await {
        futures::future::Either::Left((result, _)) => Callback::FutureFirst(result),
        futures::future::Either::Right((_, other)) => Callback::CallbackFirst(other),
    }
}
