#!/bin/sh
# Regenerate SUPPORTED_FORMS.md from the test cases in transpiler-tests.lisp.
set -e
DIR=$(cd "$(dirname "$0")" && pwd)
exec sbcl --disable-debugger \
     --eval '(ql:register-local-projects)' \
     --load "$DIR/transpiler-tests.lisp" \
     --eval '(cl-rust-generator::generate-documentation)' \
     --quit
