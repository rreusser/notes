#!/bin/bash
set -euo pipefail

# Usage: ./translate.sh <package> <routine>
# Example: ./translate.sh blas daxpy
#          ./translate.sh lapack dpotf2

PACKAGE="$1"
ROUTINE="$2"

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ "$PACKAGE" = "blas" ]; then
  SOURCE="$ROOT_DIR/BLAS-3.12.0/${ROUTINE}.f"
elif [ "$PACKAGE" = "lapack" ]; then
  SOURCE="$ROOT_DIR/lapack-3.12.0/SRC/${ROUTINE}.f"
else
  echo "Error: Unknown package '$PACKAGE'. Use 'blas' or 'lapack'." >&2
  exit 1
fi

if [ ! -f "$SOURCE" ]; then
  echo "Error: Source not found: $SOURCE" >&2
  exit 1
fi

OUTPUT_DIR="$ROOT_DIR/lib/$PACKAGE"
OUTPUT="$OUTPUT_DIR/${ROUTINE}.js"

mkdir -p "$OUTPUT_DIR"

echo "Translating $SOURCE → $OUTPUT" >&2

if [ "$PACKAGE" = "lapack" ]; then
  # For LAPACK, run GOTO removal first
  python "$ROOT_DIR/remove_goto.py" "$SOURCE" 2>/dev/null | \
    python "$ROOT_DIR/fortran_to_estree.py" 2>/dev/null | \
    node "$ROOT_DIR/estree_to_js.js" > "$OUTPUT"
else
  cat "$SOURCE" | \
    python "$ROOT_DIR/fortran_to_estree.py" 2>/dev/null | \
    node "$ROOT_DIR/estree_to_js.js" > "$OUTPUT"
fi

echo "Written: $OUTPUT" >&2
