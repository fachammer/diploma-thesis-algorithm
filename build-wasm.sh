#!/bin/sh

set -ex

wasm-pack build --no-typescript --out-dir www/pkg --profiling --target web -- . -Z 'build-std=std,panic_abort'
