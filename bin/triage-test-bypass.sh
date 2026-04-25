#!/usr/bin/env bash
# Run test.ndarray.js for every module that has both lib/ndarray.js and
# test/test.ndarray.js, and print a one-line status per module:
#
#   <module>\tPASS|FAIL n_pass n_fail
#
# Optionally also captures the first few failure lines for FAIL modules into
# /tmp/triage-test-bypass-failures.log.

set -uo pipefail

OUT_LOG=${1:-/tmp/triage-test-bypass.tsv}
DETAIL_LOG=${2:-/tmp/triage-test-bypass-failures.log}

: > "$OUT_LOG"
: > "$DETAIL_LOG"

for pkg in blas lapack; do
    for mod in lib/$pkg/base/*/; do
        mod=${mod%/}
        routine=$(basename "$mod")
        test_file="$mod/test/test.ndarray.js"
        ndarray_file="$mod/lib/ndarray.js"
        [ -f "$test_file" ] || continue
        [ -f "$ndarray_file" ] || continue

        out=$(node --test "$test_file" 2>&1)
        n_pass=$(echo "$out" | grep -E "^ℹ pass " | awk '{print $3}')
        n_fail=$(echo "$out" | grep -E "^ℹ fail " | awk '{print $3}')
        n_pass=${n_pass:-0}
        n_fail=${n_fail:-0}

        if [ "$n_fail" = "0" ]; then
            echo -e "$mod\tPASS\t$n_pass\t$n_fail" >> "$OUT_LOG"
        else
            echo -e "$mod\tFAIL\t$n_pass\t$n_fail" >> "$OUT_LOG"
            {
                echo "=== $mod ==="
                echo "$out" | grep -E "✖|TypeError|RangeError|Error message" | head -8
                echo ""
            } >> "$DETAIL_LOG"
        fi
    done
done

echo "Triage complete. Summary:"
awk -F'\t' '{print $2}' "$OUT_LOG" | sort | uniq -c
