#!/bin/sh
# Regenerate SUPPORTED_FORMS.md from the test cases in transpiler-tests.lisp.
set -e
DIR=$(cd "$(dirname "$0")" && pwd)
exec sbcl --disable-debugger \
     --load "$DIR/transpiler-tests.lisp" \
     --eval '(cl-rust-generator::generate-documentation)' \
     --quit
