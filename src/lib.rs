mod disequality;
mod multiset;
mod polynomial;
mod proof;
pub mod proof_search;
mod substitution;
mod term;

use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::console;

mod parse;
mod ui;

#[wasm_bindgen(start)]
pub fn main() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));

    ui::setup();

    console::log_1(&"main ended".into());
}
