#!/bin/bash
set -eu

#
# Usage: ./scripts/run.sh "types/never"
#

cargo fmt
#cargo check

if [[ $1 != */ ]] && [[ $1 != *\.ts ]]; then
  echo 'Wrong invokation. Argument must end with "/" or ".ts"'
  exit 1
fi

bash ./scripts/sort.sh
./scripts/success.py

# We are developing
export TEST="$1"
( set -o pipefail; cargo test --test tests -- conformance | grep --color -E 'swc_ecma_ast|swc_ts_checker|$' )

echo "$1" >> ./tests/done.txt
bash ./scripts/sort.sh
./scripts/success.py