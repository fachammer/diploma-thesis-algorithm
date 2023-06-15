import init from "./pkg/thesis_algorithm.js";

self.onmessage = async (event) => {
  const mod = await fetch("./pkg/thesis_algorithm_bg.wasm");
  const wasm_module = await init(mod, event.data);
  wasm_module.main_worker();
};
