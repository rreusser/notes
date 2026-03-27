---
name: blahpack-review
description: Review a module (or the full codebase) for convention violations, scaffolding remnants, and quality issues. The argument is the module path (e.g. `lib/blas/base/zhpmv`) or empty for full audit.
argument-hint: [module-path]
---

# Module Review

Review `$ARGUMENTS` for convention violations. Fix all errors found.

## Quick start

Run the automated audit first:
```bash
bash ${CLAUDE_SKILL_DIR}/audit.sh $ARGUMENTS
```

If no argument is given, this audits the full codebase. For a single module:
```bash
bash ${CLAUDE_SKILL_DIR}/audit.sh lib/<package>/base/<routine>
```

The audit checks scaffolding remnants, string conventions, Complex128Array
usage, and ndarray.js validation. Fix all errors and warnings it reports,
then apply the full manual checklist below.

## Bundled scripts

| Script | Purpose |
|--------|---------|
| `${CLAUDE_SKILL_DIR}/audit.sh` | Automated convention audit |
| `${CLAUDE_SKILL_DIR}/check-stub-tests.sh` | Detect scaffold-only test stubs |
| `${CLAUDE_SKILL_DIR}/fix_wrapper_docs.py` | Propagate base.js @param to ndarray.js/wrapper files |

---

Each item below has a verification command. A module is not ready for
integration until every applicable check passes.

---

## 1. No scaffolding remnants

Scaffolded content must be fully replaced with real implementations.

```bash
# No TODO in @param or @returns
grep -n "@param.*TODO\|@returns.*TODO\|{TODO}" lib/<pkg>/base/<routine>/lib/*.js

# No assert.fail stubs in tests
grep -n "assert.fail" lib/<pkg>/base/<routine>/test/test.js

# No 'not yet implemented' in base.js
grep -n "not yet implemented" lib/<pkg>/base/<routine>/lib/base.js

# Test file has real assertions (not just type checks)
# Must have >2 assert.* calls beyond the scaffolded type checks
```

---

## 2. String parameters use long-form values

Single-character Fortran flags are categorically forbidden in code AND
docstrings. See CLAUDE.md for the complete mapping table.

```bash
# No single-char strings in executable code
grep -n "'[A-Z0-9]'" lib/<pkg>/base/<routine>/lib/base.js | grep -v '//\|^\s*\*\|eslint\|require'

# No single-char strings in @param descriptions (should use backtick-quoted long-form)
grep -n "@param.*'[A-Z]'" lib/<pkg>/base/<routine>/lib/*.js | grep -v '`'
```

**d-prefix routines** must not list `'conjugate-transpose'` as a valid trans
value — it's meaningless for real data. Use `'no-transpose'` or `'transpose'`.

---

## 3. Complex arrays use Complex128Array

All z-prefix (and c-prefix) routines must accept `Complex128Array` at the API
boundary for complex data. `Float64Array` is only acceptable for genuinely real
parameters (RWORK, d, e, eigenvalues, scale factors, etc.).

```bash
# Check base.js @param types
grep "@param {Float64Array}" lib/<pkg>/base/<routine>/lib/base.js

# For z-prefix: every matrix/vector param (A, B, x, y, AP, etc.) must be Complex128Array
# Only these should remain Float64Array: WORK, RWORK, IWORK, d, e, w, S, s, scale,
# CNORM, FERR, BERR, VN1, VN2, ISUPPZ, IFAIL, BWORK, and scalar output arrays

# base.js must call reinterpret() for internal Float64 access
grep "reinterpret" lib/<pkg>/base/<routine>/lib/base.js
```

The pattern: accept `Complex128Array` in the signature, call
`reinterpret(x, 0)` at function entry, multiply strides/offsets by 2 for
Float64 indexing internally.

---

## 4. ndarray.js validates string parameters

Every string parameter must be validated using stdlib assertion helpers.
See [docs/ndarray-conformance.md](ndarray-conformance.md) for the full spec.

```bash
# Check that validators are imported and used
grep "isMatrixTriangle\|isTransposeOperation\|isDiagonalType\|isOperationSide" \
  lib/<pkg>/base/<routine>/lib/ndarray.js

# Check that TypeError is thrown for invalid strings
grep "throw new TypeError" lib/<pkg>/base/<routine>/lib/ndarray.js
```

---

## 5. Consistent array parameter conventions

All array parameters follow the `(array, stride, offset)` or
`(array, stride1, stride2, offset)` pattern.

| Array type | Convention | Example |
|-----------|-----------|---------|
| 1D vector | `x, strideX, offsetX` | `daxpy` |
| 2D matrix | `A, strideA1, strideA2, offsetA` | `dgemm` |
| Packed 1D | `AP, strideAP, offsetAP` | `dspmv` |
| Integer vector | `IPIV, strideIPIV, offsetIPIV` | `dgetrf` |
| Scalar output | `Float64Array(1)` or `Int32Array(1)` | `dlacn2` |

Verify naming:

```bash
# Stride names use 1/2 suffix (not 0/1)
grep "strideA0\|strideB0\|strideC0" lib/<pkg>/base/<routine>/lib/base.js

# ndarray.js signature matches base.js (minus internal workspace)
```

---

## 6. JSDoc accuracy

Docstrings must match the actual code — not the Fortran source, not the
scaffold, not a previous version.

```bash
# @param types match actual usage
# @param descriptions use long-form strings in backticks
# @returns type matches what the function returns
# @private is on base.js, NOT on ndarray.js
# No stale comments referencing Fortran conventions (LDA, INCX, etc.)
```

For z-prefix routines, verify stride/offset docs say "in complex elements"
(not "in Float64 elements") when the API accepts Complex128Array.

---

## 7. No complex division or absolute value inlined

Complex division and absolute value require numerically stable algorithms.

```bash
# Search for potential inline complex division
# (dividing re+im parts separately without cmplx.div)
grep -n "/ \(.*Re\|.*Im\|.*re\|.*im\)" lib/<pkg>/base/<routine>/lib/base.js | head -20

# Verify cmplx.div or cmplx.abs is used where needed
grep "cmplx\.\|zladiv" lib/<pkg>/base/<routine>/lib/base.js
```

Safe to inline: addition, subtraction, multiplication, conjugate,
real-scalar scaling, CABS1 (`|re|+|im|`), modulus (`sqrt(re^2+im^2)`).

Never inline: complex division, complex absolute value with overflow
protection, complex square root.

---

## 8. Test coverage meets thresholds

```bash
node --test --experimental-test-coverage lib/<pkg>/base/<routine>/test/test.js
```

Targets: **≥90% line coverage, ≥85% branch coverage** on base.js.

Verify the test file has substantive assertions:

```bash
# Count real test cases (not just scaffold type checks)
grep -c "^test(" lib/<pkg>/base/<routine>/test/test.js

# Verify no scaffold stubs remain
grep "assert.fail\|TODO.*implement" lib/<pkg>/base/<routine>/test/test.js
```

---

## 9. Lint passes

```bash
bin/lint.sh lib/<pkg>/base/<routine>/lib/base.js
```

Deferred rules (`max-len`, `max-params`, etc.) must be disabled with a single
`/* eslint-disable */` comment at file top listing only rules that fire.

---

## 10. Hermitian vs. symmetric correctness (z-prefix)

For zh-prefix (Hermitian) routines:
- Diagonal elements forced real (imaginary zeroed)
- Off-diagonal reads use conjugation
- Trans values: `'no-transpose'` / `'conjugate-transpose'`
- Alpha/beta types match Fortran (alpha may be real in rank-k updates)

For zs-prefix (symmetric) routines:
- Diagonal stays fully complex
- No conjugation anywhere
- Trans values: `'no-transpose'` / `'transpose'`
- Alpha and beta are complex

```bash
# Hermitian routines should have conjugation
grep -c "conj\|-.*Im\|- Av\[.*+ 1" lib/<pkg>/base/<routine>/lib/base.js

# Symmetric routines should NOT have conjugation
```

---

## 11. Return convention

| Routine type | Returns |
|-------------|---------|
| BLAS subroutine | Output array (`return y`, `return C`, `return AP`) |
| BLAS function | Scalar (`return dot`) |
| LAPACK with INFO | Integer status (`return info`, 0 = success, >0 = algorithm-specific) |

INFO values are **1-based** (matching Fortran). A 0-based loop finding an
issue at index `j` returns `j + 1`.

---

## 12. Full suite regression

After any change, verify:

```bash
node --test 'lib/**/test/test*.js' 'lib/*.test.js'
bin/check-stub-tests.sh
```

---

## Bulk audit commands

Run these across the entire codebase to find systemic issues:

```bash
# All single-char Fortran flags in @param
grep -rn "@param.*'[A-Z]'" lib --include='*.js' | grep -v '`' | wc -l

# All TODO @param
grep -rl "@param.*TODO\|{TODO}" lib --include='*.js' | wc -l

# z-prefix files missing reinterpret (possible Float64Array API)
for f in lib/blas/base/z*/lib/base.js lib/lapack/base/z*/lib/base.js; do
  grep -qL "reinterpret" "$f" 2>/dev/null && echo "$f"
done

# ndarray.js files without string validation
for f in lib/*/base/*/lib/ndarray.js; do
  if grep -q "uplo\|trans\|diag\|side" "$(dirname $f)/base.js" 2>/dev/null; then
    grep -qL "throw new TypeError" "$f" 2>/dev/null && echo "$f"
  fi
done

# d-prefix files mentioning conjugate-transpose
grep -rl "conjugate-transpose" lib/blas/base/d*/lib/ lib/lapack/base/d*/lib/ 2>/dev/null

# Scaffold stubs in test files
bin/check-stub-tests.sh
```

---

## Appendix: ndarray.js Conformance Criteria

Every `ndarray.js` must satisfy these requirements (from `docs/ndarray-conformance.md`).

### Validation of string parameters

Each string parameter must be validated using the appropriate stdlib assertion
helper. Throw `TypeError` with `format()` on invalid values.

| Parameter | Validator | Valid values |
|-----------|-----------|-------------|
| `trans`, `transa`, `transb` | `isMatrixTranspose` | `'no-transpose'`, `'transpose'`, `'conjugate-transpose'` |
| `uplo` | `isMatrixTriangle` | `'upper'`, `'lower'` |
| `side` | `isOperationSide` | `'left'`, `'right'` |
| `diag` | `isDiagonalType` | `'unit'`, `'non-unit'` |

For parameters without a stdlib helper (`direct`, `storev`, `job`, `vect`,
`norm`, `compz`, etc.), validate manually with a whitelist check.

### Validation of dimensions

All dimension parameters (`M`, `N`, `K`, etc.) must be `>= 0`.

### JSDoc requirements

- Same signature and `@param` types as base.js (no `@private`)
- `@throws` tags for each validation check
- `@returns` tag
- All `@param` descriptions use long-form strings in backticks (never single-char)
- z-prefix: complex arrays typed as `{Complex128Array}`, not `{Float64Array}`
- d-prefix: trans param lists `'no-transpose'` or `'transpose'` only (no `'conjugate-transpose'`)

### Required validator imports

```javascript
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
```
