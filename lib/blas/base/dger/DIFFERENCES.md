# dger: Differences from stdlib-js Reference

Comparison of our translation (`lib/blas/base/dger/`) against the stdlib-js
reference (`@stdlib/blas/base/dger`).

---

## [STRUCTURAL] Missing Files

The following files exist in stdlib but are absent from our module:

- `benchmark/benchmark.js`, `benchmark.native.js`, `benchmark.ndarray.js`,
  `benchmark.ndarray.native.js`
- `benchmark/c/`, `benchmark/fortran/`
- `docs/types/test.ts`
- `examples/c/`
- `test/test.dger.js`, `test/test.dger.native.js`
- `test/test.ndarray.js`, `test/test.ndarray.native.js`
- `test/fixtures/` (30 individual JSON fixture files)
- `binding.gyp`, `include.gypi`, `manifest.json`
- `include/stdlib/blas/base/dger.h`, `dger_cblas.h`, `dger_fortran.h`
- `src/addon.c`, `src/dger.c`, `src/dger.f`, `src/dger_cblas.c`,
  `src/dger_f.c`, `src/dger_ndarray.c`
- `lib/dger.native.js`, `lib/native.js`, `lib/ndarray.native.js`

Our module has `LEARNINGS.integrated.md` (not in stdlib).

---

## [SIGNATURE] base.js -- Different Algorithm Structure

**Stdlib**: Uses `@stdlib/ndarray/base/assert/is-row-major` for layout
detection with extensive JSDoc notes explaining loop interchange
optimization for row-major vs column-major storage. The algorithm uses
`isRowMajor` to swap loop dimensions for cache-optimal access patterns.

**Ours**: Direct Fortran-to-JS translation. Simple nested loop without
layout-dependent loop interchange. Functionally correct but does not
optimize for cache locality based on layout.

---

## [CONVENTION] ndarray.js -- Duplicate/Truncated JSDoc

**Ours** has a JSDoc block above `'use strict'` (outside the module body)
AND a second truncated JSDoc block inside the `// MAIN //` section that
reads: "Performs the rank 1 operation A := alpha." (truncated mid-sentence).

**Stdlib** has a single clean JSDoc block inside the module.

---

## [SIGNATURE] ndarray.js -- Missing Stride Validation

**Stdlib** validates:
- `M >= 0` (RangeError)
- `N >= 0` (RangeError)
- `strideX !== 0` (RangeError, fifth argument)
- `strideY !== 0` (RangeError, eighth argument)

**Ours** validates:
- `M >= 0` (RangeError)
- `N >= 0` (RangeError)

Missing: `strideX !== 0`, `strideY !== 0`.

---

## [SIGNATURE] dger.js (Layout Wrapper) -- Missing Validation

**Stdlib** validates: order, M >= 0, N >= 0, strideX != 0, strideY != 0,
LDA >= max(1, vala).

**Ours** validates only: order (isLayout). Missing: M, N, strideX, strideY,
LDA.

---

## [CONVENTION] index.js -- Native Addon Loading

**Stdlib** uses `tryRequire`/`isError` pattern.
**Ours** re-exports `./main.js` directly.

---

## [DOCUMENTATION] index.js -- @module Description and Examples

**Stdlib**: Full mathematical description with Unicode, expected output
values in examples.

**Ours**: ASCII description, no expected output in examples.

---

## [DOCUMENTATION] README.md

**Stdlib**: Documents both BLAS-style and ndarray APIs.
**Ours**: Documents only the ndarray API.

---

## [DOCUMENTATION] docs/types/index.d.ts

**Stdlib**: Both BLAS-style (with `Layout` type) and `ndarray()` method.
Full JSDoc, examples, `@stdlib/types/blas` imports.

**Ours**: Only ndarray-style. Bare `string` types, no license, no examples.

---

## [TESTING] test/test.js

**Stdlib**: `tape` + `proxyquire`, split across 5 files, per-scenario
JSON fixtures.

**Ours**: `node:test`, single file, JSONL fixtures from shared directory.

---

## [BENCHMARK] Missing Benchmarks

Stdlib has JS, C, and Fortran benchmarks.
Ours has no `benchmark/` directory.

---

## [CONVENTION] package.json

| Field | Stdlib | Ours |
|---|---|---|
| `main` | `"./lib"` | `"./lib/index.js"` |
| `browser` | `"./lib/main.js"` | Absent |
| `gypfile` | `true` | Absent |
| `directories` | benchmark, include, src | Absent |
| `types` | `"./docs/types"` | Absent |
| `scripts` | `{}` | `{"test": "node --test test/test.js"}` |
| `homepage`, `repository`, `bugs` | Present | Absent |
| `engines`, `os`, `keywords` | Present | Absent |
| `description` | Unicode (`A = alpha*x*y^T + A`) | ASCII (`A := alpha*x*y**T + A`) |
