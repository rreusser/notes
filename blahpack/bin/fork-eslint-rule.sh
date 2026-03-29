#!/bin/bash
# Fork a stdlib ESLint rule into the local project for customization.
#
# Usage:
#   bin/fork-eslint-rule.sh no-dynamic-require
#   bin/fork-eslint-rule.sh capitalized-comments
#
# The rule is copied to tools/eslint/rules/<name>.cjs with @stdlib
# dependencies replaced by local shims.

set -e

STDLIB_RULES_DIR="/Users/rreusser/gh/stdlib-js/stdlib/lib/node_modules/@stdlib/_tools/eslint/rules"

if [ -z "$1" ]; then
    echo "Usage: $0 <rule-name>"
    echo ""
    echo "Available rules:"
    ls "$STDLIB_RULES_DIR" \
        | grep -v '^lib$\|^test$\|^examples$\|^scripts$\|^package.json$\|^README.md$' \
        | column
    exit 1
fi

RULE_NAME="$1"
STDLIB_RULE="${STDLIB_RULES_DIR}/${RULE_NAME}/lib/main.js"
BLAHPACK_DIR="$(cd "$(dirname "$0")/.." && pwd)"
LOCAL_RULE="${BLAHPACK_DIR}/tools/eslint/rules/${RULE_NAME}.cjs"

if [ ! -f "$STDLIB_RULE" ]; then
    echo "Error: rule '${RULE_NAME}' not found at ${STDLIB_RULE}"
    exit 1
fi

if [ -f "$LOCAL_RULE" ]; then
    echo "Warning: local rule already exists at ${LOCAL_RULE}"
    read -p "Overwrite? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Copy the rule
cp "$STDLIB_RULE" "$LOCAL_RULE"

# Copy any companion files (defaults.json, etc.) from the rule's lib/ directory
RULE_LIB_DIR="$(dirname "$STDLIB_RULE")"
for companion in "$RULE_LIB_DIR"/*.json; do
    [ -f "$companion" ] || continue
    base="$(basename "$companion")"
    dest="${BLAHPACK_DIR}/tools/eslint/rules/${RULE_NAME}-${base}"
    cp "$companion" "$dest"
    # Rewrite the relative require in the rule file
    sed -i '' "s|require( './${base}' )|require( './${RULE_NAME}-${base}' )|g" "$LOCAL_RULE"
    echo "Copied companion: ${dest}"
done

# Rewrite find-jsdoc utility
sed -i '' "s|require( '@stdlib/_tools/eslint/utils/find-jsdoc' )|require( '../find-jsdoc.cjs' )|g" "$LOCAL_RULE"

# Rewrite @stdlib/* requires to shims
# Each sed rewrites one known @stdlib package to the corresponding shim export
sed -i '' "s|require( '@stdlib/assert/contains' )|require( '../shims.cjs' ).contains|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/starts-with' )|require( '../shims.cjs' ).startsWith|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/ends-with' )|require( '../shims.cjs' ).endsWith|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-array' )|require( '../shims.cjs' ).isArray|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-object' )|require( '../shims.cjs' ).isObject|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-string' )|require( '../shims.cjs' ).isString|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-capitalized' )|require( '../shims.cjs' ).isCapitalized|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-uppercase' )|require( '../shims.cjs' ).isUppercase|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-node-builtin' )|require( '../shims.cjs' ).isNodeBuiltin|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/is-regexp-string' )|require( '../shims.cjs' ).isRegExpString|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/assert/has-own-property' )|require( '../shims.cjs' ).hasOwnProp|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/trim' )|require( '../shims.cjs' ).trim|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/base/trim' )|require( '../shims.cjs' ).trim|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/left-trim' )|require( '../shims.cjs' ).ltrim|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/base/uppercase' )|require( '../shims.cjs' ).uppercase|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/string/replace' )|require( '../shims.cjs' ).replace|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/utils/index-of' )|require( '../shims.cjs' ).indexOf|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/utils/copy' )|require( '../shims.cjs' ).copy|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/utils/regexp-from-string' )|require( '../shims.cjs' ).reFromString|g" "$LOCAL_RULE"
sed -i '' "s|require( '@stdlib/utils/keys' )|require( '../shims.cjs' ).objectKeys|g" "$LOCAL_RULE"

# Check for any remaining @stdlib requires that weren't handled
REMAINING=$(grep -n "@stdlib" "$LOCAL_RULE" 2>/dev/null | grep -v '@license\|@stdlib/_tools/eslint' || true)
if [ -n "$REMAINING" ]; then
    echo ""
    echo "Warning: some @stdlib imports were not automatically rewritten:"
    echo "$REMAINING"
    echo ""
    echo "You may need to add shims for these in tools/eslint/shims.cjs"
fi

echo "Forked: ${LOCAL_RULE}"
echo ""
echo "The local version will now override the stdlib version."
echo "Edit ${LOCAL_RULE} to customize the rule."
