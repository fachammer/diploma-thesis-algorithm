importScripts("../pkg/thesis_algorithm.js");

self.onmessage = async (event) => {
  await wasm_bindgen("../pkg/thesis_algorithm_bg.wasm", event.data);
  wasm_bindgen.main_worker();
};
