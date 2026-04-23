#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

PASSED=0
FAILED=0
ERRORS=""

for test_file in "$SCRIPT_DIR"/fortran/test_*.f90; do
  [ "$test_file" = "$SCRIPT_DIR/fortran/test_utils.f90" ] && continue
  [ ! -f "$test_file" ] && continue

  routine=$(basename "$test_file" .f90 | sed 's/^test_//')

  # Determine package by checking which source directory has the file
  if [ -f "$SCRIPT_DIR/../data/BLAS-3.12.0/${routine}.f" ]; then
    package="blas"
  elif [ -f "$SCRIPT_DIR/../data/lapack-3.12.0/SRC/${routine}.f" ]; then
    package="lapack"
  else
    echo "SKIP: $routine (source not found in BLAS or LAPACK)" >&2
    continue
  fi

  if "$SCRIPT_DIR/run_fortran.sh" "$package" "$routine" 2>&1; then
    PASSED=$((PASSED + 1))
  else
    FAILED=$((FAILED + 1))
    ERRORS="$ERRORS $routine"
  fi
done

echo ""
echo "=== Fixture Generation Summary ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
if [ -n "$ERRORS" ]; then
  echo "Errors:$ERRORS"
fi
