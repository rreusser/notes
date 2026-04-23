# Conformance Plan: Status

Status: 664 modules. All 12,804 tests pass. Zero failures.

## Completed

- [x] **package.json** — main, scripts, types, homepage, repository, keywords (664 modules)
- [x] **index.js** — native addon loading pattern (660 modules)
- [x] **Fixtures** — split JSONL to per-scenario JSON, all tests use local requires (664 modules)
- [x] **Unused imports** — removed readFileSync/path from 34 test files
- [x] **Lint comments** — cleaned orphaned eslint-disable comments
- [x] **Keywords** — added to all 664 package.json files
- [x] **Benchmarks** — benchmark.js (659) + benchmark.ndarray.js (664) generated
- [x] **TypeScript tests** — docs/types/test.ts for 664 modules
- [x] **Type declarations** — docs/types/index.d.ts with dual signatures, @stdlib/types imports (664 modules)
- [x] **REPL docs** — docs/repl.txt with both APIs (664 modules)
- [x] **README.md** — dual API docs, license headers (664 modules; usage examples still TODO)
- [x] **Examples** — examples/index.js with working discreteUniform examples (417 stubs replaced)
- [x] **Test splitting** — test.js + test.<routine>.js + test.ndarray.js (664 modules)
- [x] **Layout wrapper validation** — string params, dimensions, leading dimensions (594 routine.js)
- [x] **ndarray dimension validation** — M/N/K >= 0 checks (406 ndarray.js)
- [x] **Stride checks** — stride != 0 in BLAS L2/L3 ndarray.js (48 modules)
- [x] **Norm validation** — 'max'/'one-norm'/'inf-norm'/'frobenius' (36 routine.js)
- [x] **String enum validation** — jobz, transr, fact, range, etc. (129 routine.js)
- [x] **Parameter name casing** — ndarray.js matches base.js (26 modules fixed)
- [x] **Conventions gate check** — zero failures, 40 warnings (all intentional)
- [x] **Norm normalization** — consistent 'max'/'one-norm'/'inf-norm'/'frobenius' across codebase

## In Progress

- [ ] **Scaffold updates** — updating bin/scaffold.py to generate conformant modules from the start
- [ ] **Gate check updates** — file-structure checks for new required files

## Deferred (requires upstream discussion)

### API Mismatches with stdlib (5 modules)

| Routine | Our API | stdlib API | Callers |
|---------|---------|------------|---------|
| dspmv | No `order` in ndarray/base | `order` in all layers | 5 |
| dspr | No `order` anywhere | `order` in all layers | 5+ |
| dlaswp/zlaswp | `incx` at end, extra `strideIPIV` | `inck` before IPIV | 5+5 |
| dladiv | Single `out` array | Separate `P`, `Q` arrays | 2 |
| dlassq | Returns `{scl, sumsq}` object | Output Float64Array param | 10+ |

These are breaking API changes with cascading caller updates, best addressed
in coordination with stdlib maintainers.

## Convention Warnings (non-blocking)

- **30** ndarray-matches-base-sig — intentional extra output/workspace params
- **8** routine-ld-validation — unusual param structures
- **2** index-native-loading — minimal modules (dznrm2, dlamch)
