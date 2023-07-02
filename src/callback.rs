use std::{cell::RefCell, pin::Pin, rc::Rc, task::Waker};

use futures::Future;
use wasm_bindgen::{prelude::Closure, JsCast};
use web_sys::{Event, EventTarget};

use crate::web_unchecked::EventTargetUnchecked;

struct CallbackFuture<'a> {
    waker: Rc<RefCell<Option<Waker>>>,
    completed: Rc<RefCell<Option<Event>>>,
    event_target: &'a EventTarget,
    event: &'a str,
    callback: Closure<dyn FnMut(Event)>,
}

impl<'a> CallbackFuture<'a> {
    fn new(event_target: &'a EventTarget, event: &'a str) -> Self {
        let waker: Rc<RefCell<Option<Waker>>> = Rc::new(RefCell::new(None));
        let completed: Rc<RefCell<_>> = Rc::new(RefCell::new(None));

        let waker_clone = waker.clone();
        let completed_clone = completed.clone();
        let callback = Closure::once(Box::new(move |event| {
            let _ = completed_clone.borrow_mut().insert(event);
            if let Some(waker) = waker_clone.borrow_mut().take() {
                waker.wake();
            }
        }) as Box<dyn FnMut(Event)>);
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
    type Output = Event;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        if let Some(event) = self.completed.take() {
            std::task::Poll::Ready(event)
        } else {
            self.waker.borrow_mut().replace(cx.waker().clone());
            std::task::Poll::Pending
        }
    }
}

pub(crate) async fn callback_async(event_target: &EventTarget, event: &str) -> Event {
    CallbackFuture::new(event_target, event).await
}
