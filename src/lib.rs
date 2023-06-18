mod callback;
mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;
mod timeout;
mod web_unchecked;
mod worker;

use std::{cell::RefCell, rc::Rc};

use js_sys::Uint8Array;
use proof_search::search_complete_proof;
use ui::SearchProof;
use wasm_bindgen::{
    prelude::{wasm_bindgen, Closure},
    JsCast,
};
use wasm_bindgen_futures::spawn_local;
use web_sys::{console, DedicatedWorkerGlobalScope, MessageEvent};

mod parse;
mod ui;

#[wasm_bindgen]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    spawn_local(ui::setup());
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
            console::log_2(&"got message in worker".into(), &event.data());
            let encoded_request: Uint8Array = event
                .data()
                .try_into()
                .expect("event data must be a uint8array work");
            let encoded_request = encoded_request.to_vec();

            let SearchProof { disequality } =
                bincode::deserialize(&encoded_request).expect("deserialize should work");
            let proof_result = search_complete_proof(&disequality);

            let proof_result = bincode::serialize(&proof_result).expect("serialize should work");
            let proof_result_encoded = Uint8Array::from(&proof_result[..]);

            scope_clone
                .borrow()
                .post_message(proof_result_encoded.unchecked_ref())
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
