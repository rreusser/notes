# Linting Guide

This project uses [stdlib-js](https://github.com/stdlib-js/stdlib)'s ESLint
configuration for code style enforcement. The rules are strict and specific
to the stdlib-js coding conventions.

## Running the linter

The linter runs from the stdlib repo (which has all plugins installed):

```bash
# Lint specific files
bin/lint.sh lib/blas/base/daxpy/lib/base.js

# Lint a whole module
bin/lint.sh lib/blas/base/daxpy/lib/

# Lint all BLAS base.js files
bin/lint.sh lib/blas/base/*/lib/base.js

# Lint everything (slow — ~230 modules)
bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js
```

**Do NOT use `--fix`** without extreme caution. ESLint's auto-fixer has
been observed to change arithmetic semantics (e.g., rewriting multi-line
function calls) and break tests. If you want auto-fix, run it on a single
file, verify tests pass, then move to the next.

## Rules we disable

These rules are disabled in `bin/lint.sh` because they don't apply:

- `node/no-unpublished-require` — we use relative require paths
- `stdlib/require-leading-slash` — same reason
- `stdlib/no-builtin-math` — we use `Math.*` directly, not `@stdlib/math`

## Common lint errors and how to fix them

### `no-mixed-operators` (most common, ~16,000 instances)

Add parentheses to clarify operator precedence. The rule flags any mix
of `+`/`-` with `*`/`/` without explicit grouping:

```javascript
// BAD
y[ iy ] += temp * A[ offsetA + j * sa2 + i * sa1 ];

// GOOD
y[ iy ] += temp * A[ offsetA + (j * sa2) + (i * sa1) ];
```

**Be extremely careful** — only add parentheses, never rearrange. Test
after every file. This is the highest-risk rule to fix because the
arithmetic expressions in BLAS/LAPACK are complex.

### `operator-assignment`

Replace `x = x OP expr` with `x OP= expr`:

```javascript
// BAD
B[ ib ] = alpha * B[ ib ];
B[ ib ] = B[ ib ] / A[ ia ];

// GOOD
B[ ib ] *= alpha;
B[ ib ] /= A[ ia ];
```

Safe to fix mechanically. Automated via regex in our codebase.

### `stdlib/section-header-empty-lines`

Section headers (`// MODULES //`, `// MAIN //`, `// EXPORTS //`, etc.)
need specific blank line counts:

- After `'use strict';` → **1 blank line** before first section header
- Between sections → **2 blank lines** before each section header

```javascript
'use strict';

// MODULES //     ← 1 blank line after 'use strict'

var base = require( './base.js' );


// MAIN //        ← 2 blank lines between sections
```

### `stdlib/capitalized-comments`

Comments must start with an uppercase letter:

```javascript
// BAD
// compute the result

// GOOD
// Compute the result
```

### `stdlib/vars-order`

Variable declarations should be in alphabetical order within a `var` block.

### Rules that need `eslint-disable` (can't fix)

These are intrinsic to complex LAPACK routines and should be disabled
per-file with a comment at the top:

```javascript
/* eslint-disable max-len, max-params, max-statements, max-depth, max-lines-per-function */
```

- `max-len` — lines over 80 chars (common in long expressions)
- `max-params` — functions with >10 parameters (LAPACK standard)
- `max-statements` — functions with >100 statements
- `max-depth` — nesting depth >4 (unavoidable in branchy code)
- `max-lines-per-function` — functions over 200 lines

### Rules that are cosmetic (low priority)

- `function-call-argument-newline` — consistency of argument line breaks
- `function-paren-newline` — paren placement in multi-line calls
- `@cspell/spellchecker` — spelling (needs custom BLAS/LAPACK dictionary)
- `new-cap` — constructor capitalization (Complex128Array etc.)
- `camelcase` — variable naming (we use Fortran-style sa1, sa2, etc.)

## Debugging workflow

1. **Lint one file** — `bin/lint.sh lib/<pkg>/base/<routine>/lib/base.js`
2. **Fix the errors** — edit the file
3. **Re-lint** — verify the fix
4. **Run that module's tests** — `node --test lib/<pkg>/base/<routine>/test/test.js`
5. **Only then move to the next file**

Never batch-fix across many files without testing. The `no-mixed-operators`
rule in particular can look correct but subtly change arithmetic if you
add parentheses in the wrong place.

## Audit tool

`python3 bin/audit.py --summary` shows overall conformance status.
`python3 bin/audit.py --failing` shows only non-conformant modules.
