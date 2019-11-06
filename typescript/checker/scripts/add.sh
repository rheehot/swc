#!/bin/bash
set -eu

#
# Usage: ./scripts/run.sh "types/never"
#

cargo fmt
#cargo check


# We are developing
TEST="$1" cargo test --test tests -- conformance

echo "$1" >> ./tests/done.txt

bash ./scripts/sort.sh