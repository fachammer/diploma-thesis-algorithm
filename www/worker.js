import init from "./pkg/thesis_algorithm.js";

self.onmessage = async (event) => {
  const wasm_module = await init("./pkg/thesis_algorithm_bg.wasm");
  if (event.data === "step") {
    wasm_module.step_worker();
  } else {
    wasm_module.main_worker();
  }
};
