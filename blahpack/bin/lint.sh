#!/bin/bash
# Run ESLint with stdlib + local rules on blahpack files.
#
# Usage:
#   bin/lint.sh lib/blas/base/daxpy/lib/base.js    # Lint specific files
#   bin/lint.sh lib/blas/base/daxpy/lib/            # Lint a directory
#   bin/lint.sh --fix lib/blas/base/daxpy/lib/      # Auto-fix what's possible
#   bin/lint.sh                                     # Lint all modules (one at a time)

BLAHPACK_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$BLAHPACK_DIR"

ESLINT_ARGS="--resolve-plugins-relative-to tools/eslint --ext .js"

# Collect flags (--fix, etc.) and paths separately
FLAGS=()
PATHS=()
for arg in "$@"; do
    if [ "${arg:0:1}" = "-" ]; then
        FLAGS+=("$arg")
    else
        PATHS+=("$arg")
    fi
done

# If no paths given, default to all modules under lib/
if [ ${#PATHS[@]} -eq 0 ]; then
    PATHS=("lib/")
fi

# Check if any path is the top-level lib/ directory
BATCH_MODE=false
for p in "${PATHS[@]}"; do
    case "$p" in
        lib/|lib) BATCH_MODE=true ;;
    esac
done

if [ "$BATCH_MODE" = true ]; then
    # Run per-module to avoid OOM from remark-based JSDoc rules
    TOTAL_ERRORS=0
    TOTAL_WARNINGS=0
    FAILED_MODULES=()

    MODULES=$(find lib/blas/base lib/lapack/base -mindepth 1 -maxdepth 1 -type d 2>/dev/null | sort)
    COUNT=$(echo "$MODULES" | wc -l | tr -d ' ')
    CURRENT=0

    for mod in $MODULES; do
        CURRENT=$((CURRENT + 1))
        OUTPUT=$(npx eslint $ESLINT_ARGS "${FLAGS[@]}" "$mod" 2>&1)
        EXIT=$?

        if [ $EXIT -ne 0 ]; then
            # Print module header + output only when there are issues
            echo "── ${mod} [$CURRENT/$COUNT] ──"
            echo "$OUTPUT"
            echo ""
            FAILED_MODULES+=("$mod")

            # Parse error/warning counts from the "✖ N problems (E errors, W warnings)" line
            SUMMARY=$(echo "$OUTPUT" | grep -E '^✖ [0-9]+ problems?' | head -1)
            ERRS=$(echo "$SUMMARY" | grep -oE '[0-9]+ errors?' | head -1 | grep -oE '[0-9]+')
            WARNS=$(echo "$SUMMARY" | grep -oE '[0-9]+ warnings?' | head -1 | grep -oE '[0-9]+')
            TOTAL_ERRORS=$((TOTAL_ERRORS + ${ERRS:-0}))
            TOTAL_WARNINGS=$((TOTAL_WARNINGS + ${WARNS:-0}))
        fi
    done

    # Also lint any top-level lib/*.js files
    TOP_FILES=$(find lib -maxdepth 1 -name '*.js' 2>/dev/null)
    if [ -n "$TOP_FILES" ]; then
        OUTPUT=$(npx eslint $ESLINT_ARGS "${FLAGS[@]}" $TOP_FILES 2>&1)
        if [ $? -ne 0 ]; then
            echo "── lib/*.js ──"
            echo "$OUTPUT"
            echo ""
        fi
    fi

    # Summary
    echo "════════════════════════════════════════"
    echo "Linted $COUNT modules"
    if [ ${#FAILED_MODULES[@]} -eq 0 ]; then
        echo "All clean."
        exit 0
    else
        echo "${#FAILED_MODULES[@]} modules with issues ($TOTAL_ERRORS errors, $TOTAL_WARNINGS warnings)"
        exit 1
    fi
else
    # Direct mode: lint exactly what was specified
    npx eslint $ESLINT_ARGS "${FLAGS[@]}" "${PATHS[@]}"
fi
