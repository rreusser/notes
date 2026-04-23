#!/bin/bash
# Run the full test suite but only output failures.
# Usage: bin/test-failures.sh
#
# Outputs:
#   - Summary line (pass/fail counts)
#   - For each failure: the test name and error details
#   - Exit code 0 if all pass, 1 if any fail

set -o pipefail

OUTPUT=$(npm test 2>&1)
EXIT=$?

# Print summary
echo "$OUTPUT" | grep '^ℹ' | tail -8

# Print failures with context
FAILURES=$(echo "$OUTPUT" | grep '✖' | grep -v 'failing tests:' | head -30)
if [ -n "$FAILURES" ]; then
  echo ""
  echo "Failures:"
  echo "$FAILURES"
fi

exit $EXIT
