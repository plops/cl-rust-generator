#!/bin/sh
# Run the transpiler test suite for cl-rust-generator.
# Exits non-zero when a test fails, so it can be used in CI.
set -e
DIR=$(cd "$(dirname "$0")" && pwd)
exec sbcl --disable-debugger \
     --load "$DIR/transpiler-tests.lisp" \
     --eval '(cl-rust-generator::run-transpiler-tests)' \
     --quit
