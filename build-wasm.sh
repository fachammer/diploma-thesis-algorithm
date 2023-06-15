#!/bin/sh

set -ex

RUSTFLAGS='-C target-feature=+atomics,+bulk-memory' wasm-pack build --out-dir www/pkg --profiling --target web -- . -Z 'build-std=std,panic_abort'
