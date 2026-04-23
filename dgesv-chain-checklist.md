# dgesv chain — cleanup checklist

## Critical

- [x] 1. **dlaswp signature: match stdlib published module** — base/ndarray parameter order aligned. Updated all callers (dgetrf, dgetrf2, dgetrs, dgbtrf). Standard interface also aligned (dropped separate strideIPIV).
- [x] 2. **dgetrs ndarray.js: remove duplicate import** — removed `isTransposeOperation`, kept `isMatrixTranspose`.
- [x] 3. **dgetrs ndarray.js: fix validation order** — all validation now happens before early returns.
- [x] 4. **dgesv TypeScript defs: fix return types** — changed `Float64Array` to `number`. Fixed `test.ts` with proper argument counts.
- [x] 5. **dgetrf.js: fix stride2offset IPIV length** — changed `N` to `Math.min(M, N)` in both dgetrf.js and dgetrf2.js.

## High — convention violations

- [x] 6. **Add license headers** to `dgesv/lib/ndarray.js`, `dgetrs/lib/ndarray.js`, `dgetrf2/lib/ndarray.js`.
- [x] 7. **Remove orphaned top-of-file JSDoc blocks** from same three ndarray.js files; moved `eslint-disable` to correct position.
- [x] 8. **Fix truncated JSDoc descriptions** — all first-sentence descriptions now end with a period (stdlib/jsdoc-leading-description-sentence rule).
- [x] 9. **Fix `@returns` in standard-interface files** — changed `{*} result` to `{integer} info` / `{Float64Array}` as appropriate.
- [x] 10. **Add missing `@throws` annotations** to all standard-interface JSDoc.
- [x] 11. **Fix "A * X = B,." typo** in base.js and ndarray.js.

## Medium — scaffold remnants

- [x] 12. **Fill in README.md examples** for dgesv. Other modules still have TODOs (pre-existing).
- [x] 13. **Fill in repl.txt examples** for dgesv.
- [x] 14. **Expand test.dgesv.js** — added row-major, column-major, and singular matrix correctness tests.
- [x] 15. **Fix test.dgesv.js argument types** — all test arguments now use correct types (Int32Array for IPIV, numbers for N/nrhs).
- [ ] 16. **Add row-major and non-unit-stride tests** to test.ndarray.js files — not done (pre-existing gap).
- [x] 17. **Fix benchmarks** — IPIV now uses `Int32Array` with valid pivot indices.
- [x] 18. **Fix examples/index.js** — uses proper types and real test data that produces correct results.
