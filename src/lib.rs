mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use std::{cell::RefCell, rc::Rc};

use proof_search::search_complete_proof;
use ui::SearchProof;
use wasm_bindgen::{
    prelude::{wasm_bindgen, Closure},
    JsCast, JsValue,
};
use web_sys::{console, DedicatedWorkerGlobalScope, MessageEvent};

mod parse;
mod ui;

#[wasm_bindgen]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    ui::setup();

    console::log_1(&"main ended".into());
}

#[wasm_bindgen]
pub fn main_worker() {
    console::log_1(&"hi from main_worker".into());
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    let scope = Rc::new(RefCell::new(
        js_sys::global()
            .dyn_into::<DedicatedWorkerGlobalScope>()
            .expect("worker scope must exist"),
    ));

    let scope_clone = scope.clone();

    let onmessage_handler: Closure<dyn Fn(_)> =
        Closure::wrap(Box::new(move |event: MessageEvent| {
            console::log_1(&"got message in worker".into());
            console::log_1(&event.data());

            let SearchProof { disequality } =
                serde_wasm_bindgen::from_value(event.data()).expect("from value should work");
            let proof_result = search_complete_proof(&disequality);

            scope_clone
                .borrow()
                .post_message(&JsValue::from(Box::into_raw(Box::new(proof_result))))
                .expect("post message should work");
        }));

    scope
        .borrow()
        .set_onmessage(Some(onmessage_handler.as_ref().unchecked_ref()));
    onmessage_handler.forget();
    scope
        .borrow()
        .post_message(&"ready".into())
        .expect("post message should work");
}
