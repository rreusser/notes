#!/bin/bash
# Run stdlib-js ESLint rules on blahpack files.
#
# Usage:
#   bin/lint.sh lib/blas/base/daxpy/lib/base.js    # Lint specific files
#   bin/lint.sh lib/blas/base/daxpy/lib/            # Lint a directory
#   bin/lint.sh --fix lib/blas/base/daxpy/lib/      # Auto-fix what's possible
#   bin/lint.sh                                     # Lint all lib/**/*.js

STDLIB_DIR="/Users/rreusser/gh/stdlib-js/stdlib"
BLAHPACK_DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Rules to disable (not applicable to our project)
DISABLE_RULES="--rule node/no-unpublished-require:off --rule stdlib/require-leading-slash:off --rule stdlib/no-builtin-math:off"

# Default: lint all lib/**/*.js
if [ $# -eq 0 ]; then
    set -- "lib/"
fi

# Convert relative paths to absolute
ARGS=()
for arg in "$@"; do
    if [ "${arg:0:1}" != "-" ] && [ -e "$BLAHPACK_DIR/$arg" ]; then
        ARGS+=("$BLAHPACK_DIR/$arg")
    else
        ARGS+=("$arg")
    fi
done

cd "$STDLIB_DIR" && \
    npx eslint \
    --no-eslintrc \
    -c etc/eslint/.eslintrc.js \
    $DISABLE_RULES \
    --ext .js \
    "${ARGS[@]}"
