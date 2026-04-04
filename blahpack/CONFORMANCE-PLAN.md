# Conformance Plan: Remaining Steps

Status: 664 modules. All tests pass. Fixture migration complete.

## Completed

- [x] **package.json** — main, scripts, types, homepage, repository, etc. (664 modules)
- [x] **index.js** — native addon loading pattern (660 modules)
- [x] **Fixtures** — split JSONL to per-scenario JSON, all tests use local requires (664 modules)

## Phase 1: Safe Mechanical Scripts (no judgment needed)

### 1.1 Remove unused imports (34 modules)
**Effort:** Script, ~30 minutes
**Risk:** None — imports are provably unused

34 test files still import `readFileSync` and/or `path` but never call them.
Write a script to detect and remove unused `require('fs')` and `require('path')`
imports from test files.

### 1.2 Add keywords to package.json (664 modules)
**Effort:** Script + template, ~1 hour
**Risk:** None

All 664 package.json files are missing `"keywords"`. Generate keywords from:
- Package type (blas/lapack)
- Routine name
- Standard keyword set: stdlib, math, linear algebra, float64, etc.
- Routine-specific: factorization, eigenvalue, solve, etc.

### 1.3 Clean up remaining readFileSync lint comments
**Effort:** Script, ~15 minutes
**Risk:** None

Remove orphaned `// eslint-disable-line node/no-sync` comments on lines that
no longer have `readFileSync`.

## Phase 2: Template-Based Generation

### 2.1 Generate benchmark stubs (0 → 664 modules)
**Effort:** Script + template, ~2 hours
**Risk:** Low — additive only, doesn't change existing files

Generate `benchmark/benchmark.js` and `benchmark/benchmark.ndarray.js` using
the routine's signature. Structure: create typed arrays, call routine in loop.
Follow stdlib pattern from the 21 overlapping modules.

### 2.2 Generate docs/types/test.ts (0 → 664 modules)
**Effort:** Script + template, ~2 hours  
**Risk:** Low — additive only

Generate TypeScript compile-time assertion tests. These test that the type
declarations accept correct types and reject incorrect ones.

### 2.3 Improve docs/types/index.d.ts (651 modules need fixes)
**Effort:** Script + template, ~3 hours
**Risk:** Low — overwrites existing but doesn't affect runtime

Current declarations use bare `string` types. Need to:
- Import `@stdlib/types/blas` types (Layout, TransposeOperation, etc.)
- Add BLAS-style call signature (currently only ndarray)
- Add JSDoc with `@example` blocks
- Add `/// <reference types>` header

### 2.4 Improve docs/repl.txt (651 modules need fixes)
**Effort:** Script + template, ~2 hours
**Risk:** Low — doesn't affect runtime

Generate from signature.py output with proper parameter descriptions,
return values, and examples for both APIs.

## Phase 3: Codemods (modify existing code)

### 3.1 ndarray.js validation removal (428 modules)
**Effort:** Codemod, ~2 hours + testing
**Risk:** MEDIUM — changes runtime behavior

stdlib's ndarray.js is a pass-through to base.js with no validation.
Our ndarray.js files validate parameters (throw TypeError/RangeError).
This validation should move to the layout wrapper instead.

**Decision needed:** Do we match stdlib exactly (remove ndarray validation)
or keep it as a defensive measure? If removing, need to ensure layout
wrappers have equivalent validation.

**Approach:** Two-step codemod:
1. Copy validation from ndarray.js to `<routine>.js` layout wrapper
2. Strip validation from ndarray.js, leaving only the base() delegation

### 3.2 Layout wrapper validation (379 wrappers missing validation)
**Effort:** Codemod/template, ~3 hours
**Risk:** Medium — adds throws but shouldn't break tests

280/659 layout wrappers have `@throws` annotations. The remaining 379 need:
- `order` validation (isLayout)
- String param validation (isMatrixTriangle, isTransposeOperation, etc.)
- Dimension range checks (M >= 0, N >= 0)
- Leading dimension checks (LDA >= max(1, relevant_dim))

Can be generated from signature metadata.

### 3.3 Parameter name casing (6 modules)
**Effort:** Codemod, ~30 minutes
**Risk:** Low

`transa` → `transA`, `transb` → `transB` in dgemm, dtrmm, dtrsm, zgemm,
ztrmm, ztrsm.

## Phase 4: Content Quality (requires judgment)

### 4.1 README.md cleanup (477 with TODOs)
**Effort:** Agent batches or template, ~4 hours
**Risk:** Low

477 READMEs have TODO placeholders. Generate from template with:
- Mathematical description
- Both API signatures documented
- Parameter tables
- Usage examples
- Notes section

### 4.2 examples/index.js cleanup (405 with TODOs)
**Effort:** Agent batches or template, ~3 hours
**Risk:** Low

405 examples have TODO/stub content. Generate working examples from
the routine's signature and test data.

### 4.3 Test file splitting (0/664 have split tests)
**Effort:** Codemod or agent batches, ~6 hours
**Risk:** Medium — restructures test files

stdlib splits tests into:
- `test.js` — exports, arity checks
- `test.<routine>.js` — BLAS-style layout API tests  
- `test.ndarray.js` — ndarray API tests

This is complex to automate because it requires understanding which tests
exercise which API.

## Phase 5: API Conformance (requires careful review)

### 5.1 Signature mismatches with stdlib (5 modules)
**Effort:** Manual, ~2 hours
**Risk:** HIGH — breaking API changes

Known mismatches:
- **dlassq** — returns `{scl, sumsq}` vs stdlib's `Float64Array out` param
- **dladiv** — single `out` array vs separate `P`, `Q` arrays  
- **dlaswp/zlaswp** — extra `strideIPIV`, different param order
- **dspmv/dspr** — missing `order` param in base

These affect callers and need careful migration with test updates.

## Priority Order

1. Phase 1 (mechanical scripts) — do first, zero risk
2. Phase 2.1-2.2 (benchmarks, TS tests) — additive, no risk
3. Phase 2.3-2.4 (improve existing docs) — low risk
4. Phase 3.2 (layout wrapper validation) — important for correctness
5. Phase 4.1-4.2 (README, examples) — visible quality improvement
6. Phase 3.1 (ndarray validation) — needs decision first
7. Phase 4.3 (test splitting) — high effort, can defer
8. Phase 5.1 (API mismatches) — defer until upstream discussion
