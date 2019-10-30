#!/bin/bash
set -eu

#
# Usage: ./scripts/run.sh "types/never"
#

cargo fmt
cargo check


# We are done and I don't want regression.
TEST="constEnums" cargo test --test tests -- conformance
TEST="tuple/castingTuple" cargo test --test tests -- conformance

# We are developing
TEST="$1" cargo test --test tests -- conformance
TEST="" cargo test --test tests -- conformance