#!/bin/bash
set -eu

#
# Usage: ./scripts/run.sh "types/never"
#

cargo fmt
#cargo check

bash ./scripts/sort.sh
./scripts/success.py

# We are developing
export TEST="$1"
cargo test --test tests -- conformance | grep --color -E 'swc_ecma_ast|swc_ts_checker|$'

echo "$1" >> ./tests/done.txt
bash ./scripts/sort.sh
./scripts/success.py