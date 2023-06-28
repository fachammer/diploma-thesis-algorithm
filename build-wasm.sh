#!/bin/sh

set -ex

RUST_LOG=info wasm-pack build --no-typescript --no-pack --out-dir www/pkg --profiling --target web
