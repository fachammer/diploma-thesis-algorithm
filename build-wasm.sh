#!/bin/sh

set -ex

RUSTFLAGS='-C target-feature=+atomics,+bulk-memory' wasm-pack build --profiling --target no-modules -- . -Z 'build-std=std,panic_abort'
