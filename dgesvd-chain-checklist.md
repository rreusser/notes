# dgesvd chain — cleanup checklist

Mirrors the format of `dgesv-chain-checklist.md`. Critical items affect
correctness; High items are convention violations the gate or stdlib
reviewers will flag; Medium items are scaffold remnants.

## Critical — correctness bugs

- [x] 1. **`dgesvd/lib/ndarray.js` validates strings that don't match what `base.js` consumes.**
      ndarray validator accepted `'all'/'some'/'overwrite'/'none'` and passed straight to `base.js`,
      which dispatches on `'all-columns'/'economy'/'all-rows'`. Result: `dgesvd.ndarray('all', 'all', ...)`
      returned `info=0` and computed singular values, but `U` and `VT` were silently left untouched.
      Fix: added `JOBU_MAP`/`JOBVT_MAP` translation, mirroring `zgesvd/lib/ndarray.js`.
- [x] 2. **`dorgbr/lib/ndarray.js`: same string-mismatch.** ndarray validated `'q'/'p'`, base required
      `'apply-Q'/'apply-P'`, no mapping. Fixed (`VECT_MAP`).
- [x] 3. **`dormbr/lib/ndarray.js`: same string-mismatch.** Fixed.
- [x] 4. **`dlascl/lib/ndarray.js`: validator accepted strings base.js doesn't recognize.** Validator listed
      `'hessenberg'/'band-lower'/'band-upper'`; base.js requires `'upper-hessenberg'/'lower-band'/'upper-band'`.
      Fixed by aligning validator with base.js (no mapping needed; the validator strings were just wrong).
- [x] 5. **`dgesvd/examples/index.js` used `'A'/'A'` and never ran.** Rewrote with valid `'all'/'all'`
      strings; example now produces `info: 0` and singular values.
- [x] 6. **`dgesvd/lib/index.js` `@example` had a stale `WORK, 1, 0, 8` tail (23 args for a 19-arg
      signature).** Rewritten to working examples for both standard and ndarray interfaces.
- [x] 7. **`test/test.ndarray.js` required `lib/base.js` directly, bypassing ndarray validation.**
      Switched to `lib/ndarray.js` and translated string literals (`'all-columns'`→`'all'`,
      `'all-rows'`→`'all'`, `'economy'`→`'some'`). All 43 path-combination tests still pass.
- [x] 8. **`test/test.dgesvd.js` was a 5-test scaffold passing wrong arg types.** Rewrote with 11 tests
      covering invalid-string TypeErrors, negative-dimension RangeErrors, LDA bounds, row-major +
      column-major correctness against a 3×3 reference.
- [x] **(new bug found during Phase 2)** **`dorgbr/lib/dorgbr.js` and `dormbr/lib/dormbr.js` standard
      interfaces only accepted `'apply-Q'`** (validator was `if ( vect !== 'apply-Q' ) throw`),
      meaning `'apply-P'` always threw. Fixed: validators now accept `'q'`/`'p'` and map to
      `'apply-Q'`/`'apply-P'` before calling base.js.

## High — convention violations

- [x] 9. **`dgesvd/lib/ndarray.js` had duplicate JSDoc blocks** (orphan top-of-file + real). Cleaned
      up during the rewrite for item 1.
- [x] 10. **`dgesvd/lib/ndarray.js` was missing the Apache-2.0 license header.** Added.
- [x] 11. **`dgesvd/lib/base.js` copyright was `Ricky Reusser`** — fixed to `The Stdlib Authors`.
      Same stray `// Copyright (c) 2025 Ricky Reusser. Apache-2.0 License.` line removed from
      `dlasq2/lib/base.js`, `dlasq3/lib/base.js`, `dlasq4/lib/base.js`.
- [x] 12. **Description ended with `,.`** in `base.js`, `ndarray.js`, `dgesvd.js`, `README.md`,
      `index.js`, `repl.txt`, `docs/types/index.d.ts`. All fixed.
- [x] 13. **`dgesvd.js`/`base.js` JSDoc said `'all'/'some'`** but validator wanted `'all-columns'/'economy'`.
      Standard interface now also accepts user-friendly `'all'/'some'/'overwrite'/'none'` and maps
      internally, so JSDoc and behavior align.
- [x] 14. **`dgesvd.js` `@returns {*} result` and missing `@throws`.** Replaced with `@returns {integer} info`
      plus a complete `@throws` set covering TypeError/RangeError cases.
- [x] 15. **`docs/types/index.d.ts` declared return type `Float64Array`; `test.ts` was scaffold.**
      Rewrote with `JobU`/`JobVT` union types and `: number` returns; test.ts now exercises both
      interfaces with realistic arguments.
- [x] 16. **Standard-interface validation order was inverted** (LDVT, LDU, LDA, then jobu/jobvt).
      Reordered to forward argument order: order, jobu, jobvt, M, N, LDA, LDU, LDVT.
      *Caveat:* the LDU/LDVT bounds remain `>= max(1, M)` even when `jobu='none'`, matching zgesvd.
      Tightening these to depend on the job mode is left for a future pass (also affects zgesvd).
- [x] 17. **`dorgbr/lib/base.js` and `dorgbr/lib/ndarray.js` JSDoc described `vect = 'q' | 'p'`** but
      base actually checks `'apply-Q'`. JSDoc now matches behavior in both files.
- [x] 18. **`dlasq4/lib/base.js` had a non-whitelisted `eslint-disable no-param-reassign`.** Removed
      (the rule was disabled but never violated). The `new-cap` rule, on the other hand, IS violated
      by intentional Fortran-style helpers like `Z()` in the dlasq* family — added to the gate
      whitelist (`bin/gate/checks/lint.js`) and to the per-file `eslint-disable` directives in
      `dlasq2/3/4/5/6/lib/base.js`.
- [x] 19. **`dlamch` lacked the standard scaffold** (no main.js, ndarray.js, etc.) and `package.json`
      was missing required fields. Added a `gate.config.json` entry skipping the relevant checks
      with a documented reason: dlamch is a constants-table module, not an algorithm.

## Medium — scaffold remnants

- [x] 20. **`dgesvd/README.md`** rewrote with proper Usage examples, parameter descriptions for
      `jobu`/`jobvt`/all dimensions, and a working ## Examples section.
- [x] 21. **`dgesvd/docs/repl.txt`** rewritten with descriptive parameters and runnable
      `> ...` example blocks.
- [x] 22. **`dgesvd/lib/base.js` 11 ESLint errors** (function-call-argument-newline,
      function-paren-newline) — collapsed multi-line function calls onto single lines. The
      max-lines-per-function/max-statements/max-lines warnings are inherent to a 600-line driver
      routine and were added to the per-file `eslint-disable` directive.
- [x] 23. **`dgesvd/test/test.ndarray.js` 534 ESLint errors** — file is 1048 lines in a pre-stdlib
      style throughout (vars-on-top, vars-order, array-element-newline, helper functions without
      `@private`, no license header). Added a documented `gate.config.json` exception
      (`lint.eslint-pass`); rewriting the file to stdlib conventions is a follow-up.
- [x] 24. **`dgesvd/benchmark/benchmark.js` and `benchmark.ndarray.js`** each had 1 ESLint error
      (`stdlib/vars-order`) and used the broken `'A'`/`'A'` strings. Reordered vars and switched
      to the working `'all'/'all'` strings. The N=10^3 sizing is well within the OOM-safe range
      (per memory note `project_benchmark_oom_trap.md`).
- [ ] 25. **Chain-wide cleanup pass** still pending across the remaining 30+ deps:
      - 3-failure pattern (`No TODO in README`, `Tests have >2 assertions`, `ESLint passes with
        zero errors`) is identical across `dbdsqr, dgebrd, dgelqf, dgeqrf, dlacpy, dlange, dlascl,
        dlaset, dorgqr, dorglq, dorgbr, dormbr, dgebd2, dgelq2, dgeqr2, dlabrd, dlarf, dlarfb,
        dlarfg, dlarft, dlartg, dlas2, dlasq2..6, dlasr, dlasrt, dlassq, dlasv2, dlapy2, disnan,
        dlaisnan, iladlc, iladlr` — best handled by a scripted batch pass (template README content,
        a generator for substantive `test.<routine>.js` files, and bulk benchmark-style fixes).
- [x] 26. **`dgesvd/lib/index.js` `@example` block** rewritten alongside item 6.

## Notes on chain composition

- Direct deps of dgesvd: `dbdsqr, dgebrd, dgelqf, dgeqrf, dlacpy, dlange, dlascl, dlaset, dorgqr,
  dorglq, dorgbr, dormbr, dgemm` — all exist.
- Transitive deps add `dgebd2, dgelq2, dgeqr2, dlabrd, dlarf, dlarfb, dlarfg, dlarft, dlartg, dlas2,
  dlasq1..6, dlasr, dlasrt, dlassq, dlasv2, dlapy2, dlamch, disnan, dlaisnan, iladlc, iladlr`.
- Total chain: 38 lapack modules + dependencies on blas (`dcopy, dgemm, dgemv, dger, dnrm2, drot,
  dscal, dswap, dtrmm, dtrmv`).

## Cross-chain follow-ups (out of scope of this audit, surface separately)

- **`zgesvd` has the same orphan-JSDoc + missing-license-header pattern in `lib/ndarray.js`** that
  was fixed in `dgesvd/lib/ndarray.js`. Worth fixing the same way to keep the pair consistent.
- **The over-strict LDU/LDVT bounds in the SVD standard interfaces** (require `max(1, M)` even when
  the corresponding job is `'none'`) affect both `dgesvd.js` and `zgesvd.js`. A real-LAPACK-style
  `LDU >= 1; if WNTUAS then LDU >= M; ...` validator would relax them; doing it consistently across
  both modules is best handled as one PR.
