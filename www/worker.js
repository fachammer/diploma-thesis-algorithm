importScripts('../pkg/thesis_algorithm.js');

async function init() {
    await wasm_bindgen('../pkg/thesis_algorithm_bg.wasm');

    const { main_worker } = wasm_bindgen;
    main_worker();
}

init();
