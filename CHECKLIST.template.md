# Module Conformance Checklist

This checklist tracks what needs to be done to bring a blahpack module
into conformance with stdlib-js contribution standards. Items marked
with [AUTO] can be addressed programmatically; [MANUAL] requires human
review; [GENERATE] means the file should be generated from a template.

## File Structure

- [ ] [GENERATE] `benchmark/benchmark.js` — JS benchmarks for layout API
- [ ] [GENERATE] `benchmark/benchmark.ndarray.js` — JS benchmarks for ndarray API
- [ ] [GENERATE] `docs/types/index.d.ts` — TypeScript declarations (both APIs, proper types)
- [ ] [GENERATE] `docs/types/test.ts` — TypeScript compile-time assertion tests
- [ ] [AUTO] Remove `LEARNINGS.md` / `LEARNINGS.integrated.md` (not shipped to stdlib)
- [ ] [AUTO] Remove any files not in stdlib convention (e.g., extra scaffold artifacts)

## package.json

- [ ] [AUTO] `"main"` should be `"./lib"` (not `"./lib/index.js"`)
- [ ] [GENERATE] Add `"types": "./docs/types"`
- [ ] [GENERATE] Add `"homepage"`, `"repository"`, `"bugs"` fields
- [ ] [GENERATE] Add `"engines"`, `"os"` fields
- [ ] [GENERATE] Add `"keywords"` array
- [ ] [AUTO] `"scripts"` should be `{}` (not `{"test": ...}`)
- [ ] [GENERATE] Add `"description"` with full mathematical notation

## index.js

- [ ] [AUTO] Add native addon loading pattern (`tryRequire`/`isError` fallback)
- [ ] [MANUAL] `@module` should reference `@stdlib/<pkg>/base/<routine>`
- [ ] [MANUAL] `@example` blocks should show both layout API and `.ndarray()` with
      correct arguments and expected output comments

## base.js

- [ ] [MANUAL] Verify `@private` annotation present
- [ ] [MANUAL] JSDoc `@param` types should use `{NonNegativeInteger}`, `{PositiveInteger}`,
      `{integer}`, `{Float64Array}`, `{Complex128Array}` as appropriate
- [ ] [MANUAL] JSDoc `@param` descriptions should match stdlib verbosity (e.g.,
      "stride of the first dimension of `A` (a.k.a., leading dimension of the matrix `A`)")
- [ ] [MANUAL] `@example` blocks should show realistic usage with expected output
- [ ] [MANUAL] Review algorithm — stdlib may use optimized variants (e.g., blocked
      multiplication, ddot kernels) vs our direct Fortran translation

## ndarray.js (public ndarray API)

- [ ] [AUTO] Should NOT have input validation — stdlib ndarray is a pass-through to base.
      Validation lives in the layout wrapper only.
      **Exception**: Some stdlib ndarray files DO validate. Check the reference.
- [ ] [MANUAL] JSDoc should include `@example` blocks
- [ ] [MANUAL] JSDoc `@param` descriptions should match stdlib conventions
- [ ] [AUTO] Should NOT be `@private` (it's a public API)

## `<routine>.js` (layout wrapper)

- [ ] [MANUAL] Must accept `order` as first parameter (BLAS routines) or appropriate
      layout parameter (LAPACK routines)
- [ ] [MANUAL] Must validate ALL parameters with `@throws` annotations:
  - `order` via `isLayout`
  - String params (`uplo`, `trans`, `diag`, `side`) via appropriate validators
  - Dimension params (`M`, `N`, `K`) >= 0
  - Leading dimensions (`LDA`, `LDB`) >= max(1, relevant_dim)
  - Stride params != 0 where applicable
- [ ] [MANUAL] Must compute strides from `order` and call `base()` with stride/offset args
- [ ] [AUTO] Parameter names should match stdlib convention (e.g., `transA` not `transa`)

## main.js

- [ ] [AUTO] Should attach `.ndarray` from ndarray.js to the layout wrapper
- [ ] [AUTO] Should use `setReadOnly` from `@stdlib/utils/define-nonenumerable-read-only-property`

## README.md

- [ ] [GENERATE] Should document BOTH the layout API and the `.ndarray()` API
- [ ] [GENERATE] Should include mathematical notation in description
- [ ] [GENERATE] Should include parameter tables with types, descriptions
- [ ] [GENERATE] Should include notes about edge cases (quick returns, etc.)
- [ ] [GENERATE] Should include a complete usage example section

## docs/repl.txt

- [ ] [GENERATE] Should document both APIs with parameter descriptions, return values,
      notes, and examples

## Testing

- [ ] [GENERATE] Split into separate test files: `test.js` (exports), `test.<routine>.js`
      (layout API), `test.ndarray.js` (ndarray API)
- [ ] [GENERATE] Per-scenario JSON fixture files in `test/fixtures/` (not shared JSONL)
- [ ] [MANUAL] Verify test coverage >= 90% line, >= 85% branch on base.js

## Signature Conformance

- [ ] [AUTO] Verify function signature matches `bin/signature.py` output
- [ ] [MANUAL] For routines with stdlib reference: verify parameter order matches stdlib
- [ ] [MANUAL] For packed-storage routines: verify `order` parameter handling matches stdlib
      convention (some packed routines pass `order` through to base, others don't)

## Notes

- Native addon files (`native.js`, `ndarray.native.js`, C source, binding.gyp, etc.)
  are NOT generated here — they will be added when the native layer is implemented.
- Benchmark files should follow the stdlib pattern of testing each parameter combination.
- TypeScript types should use `@stdlib/types/blas` types (`Layout`, `TransposeOperation`,
  `MatrixTriangle`, `DiagonalType`) instead of bare `string`.
