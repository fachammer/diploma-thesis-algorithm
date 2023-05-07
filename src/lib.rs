mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn greet() {
    let s = "Hello, world";
    web_sys::console::log_1(&JsValue::from_str(s));
}

#[wasm_bindgen]
pub fn add(left: u32, right: u32) -> u32 {
    left + right
}
