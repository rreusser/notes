# dsyr2: Differences from stdlib-js Reference

Comparison of our translation (`lib/blas/base/dsyr2/`) against the stdlib-js
reference (`@stdlib/blas/base/dsyr2`).

---

## [STRUCTURAL] Missing Files

- `benchmark/benchmark.js`, `benchmark.native.js`, `benchmark.ndarray.js`,
  `benchmark.ndarray.native.js`
- `benchmark/c/` (C benchmarks)
- `docs/types/test.ts`
- `examples/c/`
- `test/test.dsyr2.js`, `test/test.dsyr2.native.js`
- `test/test.ndarray.js`, `test/test.ndarray.native.js`
- `test/fixtures/` (30 individual JSON fixture files)
- `binding.gyp`, `include.gypi`, `manifest.json`
- `include/stdlib/blas/base/dsyr2.h`, `dsyr2_cblas.h`
- `src/addon.c`, `src/dsyr2.c`, `src/dsyr2_cblas.c`, `src/dsyr2_ndarray.c`
- `lib/dsyr2.native.js`, `lib/native.js`, `lib/ndarray.native.js`

Our module has `LEARNINGS.integrated.md` (not in stdlib).

---

## [SIGNATURE] base.js -- Matching Signatures

Both have the same base.js signature:
```
dsyr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA )
```

**Stdlib** uses `@stdlib/ndarray/base/assert/is-row-major` for layout
detection and swaps loop dimensions.

**Ours** uses direct stride variables. Both are functionally equivalent.

---

## [SIGNATURE] dsyr2.js (Layout Wrapper) -- Missing Validation

Both have the same signature:
```
dsyr2( order, uplo, N, alpha, x, strideX, y, strideY, A, LDA )
```

**Stdlib** validates: order, uplo, N >= 0, strideX != 0, strideY != 0,
LDA >= max(1, N).

**Ours** validates only: order (isLayout). Missing: uplo, N, strideX,
strideY, LDA.

---

## [SIGNATURE] ndarray.js -- Missing Stride Validation

Both have the same signature:
```
dsyr2( uplo, N, alpha, x, strideX, offsetX, y, strideY, offsetY, A, strideA1, strideA2, offsetA )
```

**Stdlib** validates: uplo, N >= 0, strideX != 0, strideY != 0,
strideA1 != 0, strideA2 != 0.

**Ours** validates: uplo, N >= 0, strideX != 0, strideY != 0.
Missing: strideA1 != 0, strideA2 != 0.

---

## [CONVENTION] ndarray.js -- Duplicate JSDoc

**Ours** has a JSDoc block above the `'use strict'` pragma (outside the
module body) and another inside the `// MAIN //` section. **Stdlib** has
only the inner block.

---

## [CONVENTION] index.js -- Native Addon Loading

**Stdlib** uses `tryRequire`/`isError` pattern.
**Ours** re-exports `./main.js` directly.

---

## [DOCUMENTATION] index.js -- @module Description

**Stdlib**: Full mathematical description with Unicode.
**Ours**: Short "Perform symmetric rank-2 update."

---

## [DOCUMENTATION] README.md

**Stdlib**: Both APIs documented.
**Ours**: Ndarray API only.

---

## [DOCUMENTATION] docs/types/index.d.ts

**Stdlib**: Both BLAS-style (with `Layout`, `MatrixTriangle` types) and
`ndarray()` method. Full JSDoc with examples.

**Ours**: Only ndarray-style. Bare `string` types. No license, no examples.

---

## [TESTING] test/test.js

**Stdlib**: `tape`, split into 5 test files (including native), 30 JSON
fixture files.

**Ours**: `node:test`, single file, JSONL fixtures.

---

## [BENCHMARK] Missing Benchmarks

Stdlib has 4 JS benchmark files and C benchmarks.
Ours has no `benchmark/` directory.

---

## [CONVENTION] package.json

| Field | Stdlib | Ours |
|---|---|---|
| `main` | `"./lib"` | `"./lib/index.js"` |
| `gypfile` | `true` | Absent |
| `directories` | benchmark, include, src | Absent |
| `types` | `"./docs/types"` | Absent |
| `scripts` | `{}` | `{"test": "node --test test/test.js"}` |
| `homepage`, `repository`, `bugs` | Present | Absent |
| `engines`, `os`, `keywords` | Present | Absent |
| `description` | Unicode math notation | Short generic description |
