#!/usr/bin/env bash
# Profile the Laplace solver example.
#
# Usage:
#   bin/profile.sh              # Quick timing breakdown
#   bin/profile.sh --cpu-prof   # Generate .cpuprofile for Chrome DevTools
#   bin/profile.sh --prof       # Generate V8 tick log (use with --prof-process)
#   bin/profile.sh --flame      # Generate .cpuprofile + instructions
#
# Environment variables:
#   STEP=0.005       Boundary discretization step (smaller = more points)
#   EVAL_POINTS=50000  Number of interior evaluation points
#   WARMUP=1         Number of warmup runs (for JIT)

set -euo pipefail
cd "$(dirname "$0")/.."

MODE="${1:-}"
SCRIPT="examples/laplace/profile.js"

case "$MODE" in
  --cpu-prof)
    echo "=== Generating CPU profile ==="
    echo "Output: CPU.*.cpuprofile"
    echo ""
    node --cpu-prof --cpu-prof-dir=. "$SCRIPT"
    PROF=$(ls -t CPU.*.cpuprofile 2>/dev/null | head -1)
    if [ -n "$PROF" ]; then
      echo ""
      echo "Profile written: $PROF"
      echo "Open in Chrome DevTools: chrome://inspect → Open dedicated DevTools → Performance → Load"
      echo "Or use: npx speedscope $PROF"
    fi
    ;;

  --prof)
    echo "=== Generating V8 tick log ==="
    node --prof "$SCRIPT"
    LOG=$(ls -t isolate-*.log 2>/dev/null | head -1)
    if [ -n "$LOG" ]; then
      echo ""
      echo "Processing tick log..."
      node --prof-process "$LOG" > profile-processed.txt
      echo "Written: profile-processed.txt"
      echo ""
      head -80 profile-processed.txt
    fi
    ;;

  --flame)
    echo "=== Generating CPU profile for flame graph ==="
    echo ""
    WARMUP=1 node --cpu-prof --cpu-prof-dir=. --cpu-prof-interval=100 "$SCRIPT"
    PROF=$(ls -t CPU.*.cpuprofile 2>/dev/null | head -1)
    if [ -n "$PROF" ]; then
      echo ""
      echo "Profile written: $PROF"
      echo ""
      echo "To view as flame graph:"
      echo "  npx speedscope $PROF"
      echo ""
      echo "To view in Chrome DevTools:"
      echo "  1. Open chrome://inspect"
      echo "  2. Click 'Open dedicated DevTools for Node'"
      echo "  3. Go to Performance tab → Load profile → select $PROF"
    fi
    ;;

  *)
    # Default: just timing breakdown
    node "$SCRIPT"
    ;;
esac
