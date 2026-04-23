# dtrmv: Differences from stdlib-js Reference

Comparison of our translation (`lib/blas/base/dtrmv/`) against the stdlib-js
reference (`@stdlib/blas/base/dtrmv`).

---

## [STRUCTURAL] Missing Files

- `benchmark/benchmark.js`, `benchmark/benchmark.ndarray.js`
- `docs/types/test.ts`
- `test/test.dtrmv.js` (BLAS-style tests)
- `test/test.ndarray.js`
- `test/fixtures/` (34 individual JSON fixture files)

Our module has `LEARNINGS.integrated.md` (not in stdlib).

---

## [SIGNATURE] base.js -- Matching Signatures

Both have the same base.js signature:
```
dtrmv( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX )
```

**Stdlib** uses `@stdlib/ndarray/base/assert/is-row-major` to detect layout
and swap loop dimensions. Variable naming uses `sa0`/`sa1` (dimension 0/1)
convention and `nonunit` boolean.

**Ours** uses similar structure with `sa1`/`sa2`, `nounit` boolean. The
variable naming differs (`nonunit` vs `nounit`) but logic is equivalent.

---

## [SIGNATURE] dtrmv.js (Layout Wrapper) -- Missing Validation

Both have the same signature:
```
dtrmv( order, uplo, trans, diag, N, A, LDA, x, strideX )
```

**Stdlib** validates: order, uplo, trans, diag, N >= 0, LDA >= max(1, N),
strideX != 0.

**Ours** validates only: order (isLayout). Missing: uplo, trans, diag, N,
LDA, strideX.

---

## [SIGNATURE] ndarray.js -- Missing Stride Validation

Both have the same signature:
```
dtrmv( uplo, trans, diag, N, A, strideA1, strideA2, offsetA, x, strideX, offsetX )
```

**Stdlib** validates: uplo, trans, diag, N >= 0, strideA1 != 0,
strideA2 != 0, strideX != 0.

**Ours** validates: uplo, trans, diag, N >= 0, strideX != 0.
Missing: strideA1 != 0, strideA2 != 0.

Uses import names: `isMatrixTriangle`, `isTransposeOperation`, `isDiagonal`
in stdlib vs `isMatrixTriangle`, `isDiagonalType` + duplicate
`isMatrixTranspose`/`isTransposeOperation` in ours.

---

## [CONVENTION] ndarray.js -- Duplicate Imports

**Ours** imports the transpose-operation checker twice under different names:
```js
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
...
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
```
This is dead code duplication.

**Stdlib** imports it once as `isTransposeOperation`.

Also, our diagonal type checker is named `isDiagonalType` while stdlib
names it `isDiagonal`.

---

## [CONVENTION] ndarray.js -- Duplicate JSDoc

**Ours** has a JSDoc block above the `'use strict'` pragma and another
inside the `// MAIN //` section. **Stdlib** has only the inner block.

---

## [CONVENTION] index.js -- Native Addon Loading

**Stdlib** uses `tryRequire`/`isError` pattern.
**Ours** re-exports `./main.js` directly.

---

## [DOCUMENTATION] index.js -- Examples

**Stdlib**: BLAS-style example with expected output values in comments.
**Ours**: Both BLAS-style and ndarray examples, no expected output.

---

## [DOCUMENTATION] README.md

**Stdlib**: Both APIs documented.
**Ours**: Ndarray API only.

---

## [DOCUMENTATION] docs/types/index.d.ts

**Stdlib**: Both BLAS-style (with `Layout`, `MatrixTriangle`,
`TransposeOperation`, `DiagonalType` types) and `ndarray()` signatures.
Full JSDoc with examples.

**Ours**: Only ndarray-style. Bare `string` types. No license, no examples.

---

## [TESTING] test/test.js

**Stdlib**: `tape`, split into 3 test files, 34 JSON fixture files.
**Ours**: `node:test`, single file, JSONL fixtures.

---

## [BENCHMARK] Missing Benchmarks

Stdlib has 2 JS benchmark files. Ours has none.

---

## [CONVENTION] package.json

| Field | Stdlib | Ours |
|---|---|---|
| `main` | `"./lib"` | `"./lib/index.js"` |
| `directories.benchmark` | Present | Absent |
| `types` | `"./docs/types"` | Absent |
| `scripts` | `{}` | `{"test": "node --test test/test.js"}` |
| `homepage`, `repository`, `bugs` | Present | Absent |
| `engines`, `os`, `keywords` | Present | Absent |
| `description` | Unicode (`x = A*x` or `x = A^T*x`) | ASCII (`x := A*x` or `x := A**T*x`) |
