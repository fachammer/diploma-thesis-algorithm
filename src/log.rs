/// # Examples
/// `
/// assert_eq!(crate::log::measure! { 2 + 2 }, 4)
/// `
macro_rules! measure {
    ($code:expr) => {{
        let start = crate::log::now();

        let result = $code;

        let end = crate::log::now();
        console::log_3(
            &(end - start).into(),
            &"milliseconds in".into(),
            &stringify!($code).into(),
        );

        result
    }};
}

pub(crate) use measure;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = performance)]
    static PERFORMANCE: web_sys::Performance;
}

pub(crate) fn now() -> f64 {
    PERFORMANCE.now()
}
