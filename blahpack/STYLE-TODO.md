# stdlib-js Style Alignment Checklist

Differences between blahpack and stdlib-js conventions, with status and
proposed actions. See [docs/stdlib-style-differences.md](docs/stdlib-style-differences.md)
for detailed comparison with code examples.

## Done

- [x] **String parameters in base.js** — Converted all single-char Fortran
  params (`'U'`, `'N'`, `'T'`, etc.) to full-word stdlib convention
  (`'upper'`, `'no-transpose'`, `'transpose'`, etc.). 4,300+ changes across
  260+ files. Automated via `bin/convert_strings.py` (re-runnable for new files).

- [x] **main.js — `setReadOnly()`** — Converted all 224 `main.js` files from
  `Object.defineProperty` to `setReadOnly( fn, 'ndarray', ndarray )` using
  `@stdlib/utils/define-nonenumerable-read-only-property`.

## To do — high impact

- [x] **`ndarray.js` — parameter validation (207 modules)** — All 207
  applicable ndarray.js files now have stdlib-style validation: string param
  checks (isMatrixTranspose, isMatrixTriangle, isOperationSide, isDiagonalType,
  plus manual whitelist for norm, direct, storev, job, vect, compq/compz, etc.),
  dimension checks (M, N, K >= 0), early returns, and proper JSDoc with
  @throws tags. Automated via `bin/gen_ndarray_validation.py` (re-runnable).
  18 scalar utility routines (disnan, dlapy2, dlartg, etc.) have no params
  requiring validation and are excluded.
  See `docs/ndarray-conformance.md` for the full spec.

- [x] **`<routine>.js` — BLAS-style API (217 modules)** — Generated via
  `bin/gen_routine_js.py`. Implements `order`+`LDA` → stride conversion for
  matrix routines, `stride2offset` for vector routines. Validates `order`
  with `isLayout`. Known limitation: vector length for stride2offset uses
  a simple heuristic (always `N`), which is wrong for some routines when
  negative strides are used (e.g., dgemv's `y` should use `M` when
  trans='no-transpose'). Needs per-routine refinement.

- [x] **`index.js` — `@module` JSDoc** — All 228 index.js files now have
  `@module` tag with package name and `@example` blocks showing both the
  BLAS-style and ndarray usage. Automated via `bin/gen_index_examples.js`
  (re-runnable for new files). 2 minimal modules (dznrm2, dlamch) done manually.

- [x] **eslint directives** — Converted all 562 files from inline
  `// eslint-disable-line` to file-level `/* eslint-disable ... */` before
  `'use strict'`. Automated via `bin/convert_eslint.py` (re-runnable).

- [x] **License headers** — All 230 base.js files now have Apache-2.0 license
  headers. Automated via `bin/add_license.py` (re-runnable).

## To do — medium impact

- [ ] **JSDoc `@example` blocks in ndarray.js/base.js** — stdlib includes
  runnable code examples in `@example` sections of ndarray.js and base.js.
  We have `@param` tags only (index.js examples are now done — this is for
  the per-file JSDoc). These serve as inline documentation and are extracted
  for auto-generated docs.

- [ ] **Dependencies via `@stdlib/` paths** — stdlib uses package paths
  (`@stdlib/math/base/special/fast/min`). We use relative paths
  (`../../dlamch/lib/base.js`). For standalone use relative paths are fine,
  but for stdlib integration the package paths are needed. Likely a final
  integration step, not worth doing now.

## To do — low impact / optional

- [ ] **Variable naming (`i0`/`i1` vs `i`/`j`)** — stdlib numbers loop
  variables from innermost (0) to outermost (1). We use Fortran-style `i`/`j`.
  Both are readable. Low priority, high churn, no functional difference.

- [ ] **Layout-aware loop ordering** — stdlib uses `isRowMajor()` +
  `loopOrder()` for cache-optimal iteration in routines like dlacpy. We rely
  on strides being correct (which they are) but don't optimize iteration order
  for cache locality. Matters only for performance on large row-major matrices.

- [ ] **Beta-scaling via `dfill`/`dscal`** — stdlib calls library functions
  for the `y = beta*y` step in dgemv/dgemm. We use inline loops. Functionally
  identical, minor style difference.

- [ ] **Function decomposition in base.js** — stdlib splits large base.js
  into per-branch helper functions (e.g., `copyAll`, `copyUpper`,
  `copyLower`). We use if/else in one function. stdlib style is more verbose
  but each function is self-contained with its own JSDoc. Low priority.

- [ ] **Outer loop reset stride** — stdlib precomputes
  `da1 = strideA2 - (S0 * strideA1)` for the outer loop reset. We recompute
  `ia = offsetA + j * sa2` each iteration. Both are correct; stdlib's is one
  fewer multiply per outer iteration. Low priority.
