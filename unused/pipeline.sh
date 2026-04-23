#!/bin/bash
set -euo pipefail

# Multi-stage Fortran→JS translation pipeline with stored intermediates.
#
# Usage:
#   ./pipeline.sh init    <package> <routine>   Initialize pipeline, generate reference fixture
#   ./pipeline.sh step1   <routine>             Automated GOTO removal (Fortran→Fortran)
#   ./pipeline.sh step3   <routine>             Automated translation (Fortran→JS)
#   ./pipeline.sh verify  <routine> <step>      Verify a pipeline stage (1,2 = Fortran; 4 = JS)
#   ./pipeline.sh finalize <package> <routine>   Copy step4.js to lib/<package>/<routine>.js
#
# Steps 2 (AI-assisted Fortran restructuring) and 4 (reindexing) are manual.

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
PIPELINE_DIR="$ROOT_DIR/pipeline"

cmd="${1:-}"; shift || true

case "$cmd" in

  init)
    PACKAGE="$1"
    ROUTINE="$2"
    DIR="$PIPELINE_DIR/$ROUTINE"
    mkdir -p "$DIR"

    # Determine source path
    if [ "$PACKAGE" = "blas" ]; then
      SOURCE="$ROOT_DIR/BLAS-3.12.0/${ROUTINE}.f"
    elif [ "$PACKAGE" = "lapack" ]; then
      SOURCE="$ROOT_DIR/lapack-3.12.0/SRC/${ROUTINE}.f"
    else
      echo "Error: Unknown package '$PACKAGE'" >&2; exit 1
    fi

    if [ ! -f "$SOURCE" ]; then
      echo "Error: Source not found: $SOURCE" >&2; exit 1
    fi

    # Step 0: copy original
    cp "$SOURCE" "$DIR/step0.f"
    echo "step0.f: copied from $SOURCE" >&2

    # Store package for later use
    echo "$PACKAGE" > "$DIR/.package"

    # Generate reference fixture
    if [ -f "$ROOT_DIR/test/fortran/test_${ROUTINE}.f90" ]; then
      "$ROOT_DIR/test/run_fortran.sh" "$PACKAGE" "$ROUTINE"
      echo "Reference fixture generated." >&2
    else
      echo "Warning: No test program found (test/fortran/test_${ROUTINE}.f90). Skipping fixture." >&2
    fi
    ;;

  step1)
    ROUTINE="$1"
    DIR="$PIPELINE_DIR/$ROUTINE"

    if [ ! -f "$DIR/step0.f" ]; then
      echo "Error: Run 'init' first." >&2; exit 1
    fi

    # Run automated GOTO removal (output as free-form .f90)
    python "$ROOT_DIR/remove_goto.py" "$DIR/step0.f" 2>/dev/null > "$DIR/step1.f90"
    echo "step1.f90: automated GOTO removal complete" >&2

    # Count remaining GOTOs
    REMAINING=$(grep -c "GO TO\|GOTO" "$DIR/step1.f90" 2>/dev/null || echo 0)
    echo "Remaining GOTOs: $REMAINING" >&2

    # Verify against reference fixture
    PACKAGE=$(cat "$DIR/.package")
    if [ -f "$ROOT_DIR/test/fortran/test_${ROUTINE}.f90" ]; then
      "$ROOT_DIR/test/run_fortran.sh" "$PACKAGE" "$ROUTINE" \
        --source "$DIR/step1.f90" \
        --output "$DIR/step1-verify.jsonl"

      if diff -q "$ROOT_DIR/test/fixtures/${ROUTINE}.jsonl" "$DIR/step1-verify.jsonl" > /dev/null 2>&1; then
        echo "VERIFY step1: PASS (output matches reference)" >&2
      else
        echo "VERIFY step1: FAIL (output differs from reference)" >&2
        diff "$ROOT_DIR/test/fixtures/${ROUTINE}.jsonl" "$DIR/step1-verify.jsonl" >&2 || true
        exit 1
      fi
    fi
    ;;

  verify)
    ROUTINE="$1"
    STEP="$2"
    DIR="$PIPELINE_DIR/$ROUTINE"
    PACKAGE=$(cat "$DIR/.package")

    if [ "$STEP" = "1" ] || [ "$STEP" = "2" ]; then
      # Try .f90 first, fall back to .f
      if [ -f "$DIR/step${STEP}.f90" ]; then
        STEPFILE="$DIR/step${STEP}.f90"
      else
        STEPFILE="$DIR/step${STEP}.f"
      fi
      if [ ! -f "$STEPFILE" ]; then
        echo "Error: $STEPFILE not found" >&2; exit 1
      fi

      "$ROOT_DIR/test/run_fortran.sh" "$PACKAGE" "$ROUTINE" \
        --source "$STEPFILE" \
        --output "$DIR/step${STEP}-verify.jsonl"

      if diff -q "$ROOT_DIR/test/fixtures/${ROUTINE}.jsonl" "$DIR/step${STEP}-verify.jsonl" > /dev/null 2>&1; then
        echo "VERIFY step${STEP}: PASS" >&2
      else
        echo "VERIFY step${STEP}: FAIL" >&2
        diff "$ROOT_DIR/test/fixtures/${ROUTINE}.jsonl" "$DIR/step${STEP}-verify.jsonl" >&2 || true
        exit 1
      fi

    elif [ "$STEP" = "4" ]; then
      if [ ! -f "$DIR/step4.js" ]; then
        echo "Error: step4.js not found" >&2; exit 1
      fi
      # Copy to lib and run JS tests
      LIB_DIR="$ROOT_DIR/lib/$PACKAGE"
      mkdir -p "$LIB_DIR"
      cp "$DIR/step4.js" "$LIB_DIR/${ROUTINE}.js"
      if [ -f "$LIB_DIR/${ROUTINE}.test.js" ]; then
        node --test "$LIB_DIR/${ROUTINE}.test.js"
      else
        echo "Warning: No JS test found ($LIB_DIR/${ROUTINE}.test.js)" >&2
      fi

    else
      echo "Error: Step must be 1, 2, or 4" >&2; exit 1
    fi
    ;;

  step3)
    ROUTINE="$1"
    DIR="$PIPELINE_DIR/$ROUTINE"

    # Prefer step2 if it exists, then step1, then step0. Try .f90 first.
    if [ -f "$DIR/step2.f90" ]; then
      INPUT="$DIR/step2.f90"
    elif [ -f "$DIR/step2.f" ]; then
      INPUT="$DIR/step2.f"
    elif [ -f "$DIR/step1.f90" ]; then
      INPUT="$DIR/step1.f90"
    elif [ -f "$DIR/step1.f" ]; then
      INPUT="$DIR/step1.f"
    else
      INPUT="$DIR/step0.f"
    fi

    echo "step3: translating $INPUT → step3.js" >&2
    cat "$INPUT" | \
      python "$ROOT_DIR/fortran_to_estree.py" 2>/dev/null | \
      node "$ROOT_DIR/estree_to_js.js" > "$DIR/step3.js"
    echo "step3.js: automated translation complete" >&2
    ;;

  finalize)
    PACKAGE="$1"
    ROUTINE="$2"
    DIR="$PIPELINE_DIR/$ROUTINE"

    if [ ! -f "$DIR/step4.js" ]; then
      echo "Error: step4.js not found. Complete stage 4 first." >&2; exit 1
    fi

    LIB_DIR="$ROOT_DIR/lib/$PACKAGE"
    mkdir -p "$LIB_DIR"
    cp "$DIR/step4.js" "$LIB_DIR/${ROUTINE}.js"
    echo "Finalized: lib/$PACKAGE/${ROUTINE}.js" >&2
    ;;

  *)
    echo "Usage: ./pipeline.sh {init|step1|step3|verify|finalize} ..." >&2
    exit 1
    ;;
esac
