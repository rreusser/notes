# dsytri2: Translation Learnings

## Translation pitfalls

- `dsytri2` is a pure dispatcher in the Fortran reference: it calls `ILAENV` once to pick `NBMAX`, then forwards to `dsytri` (if `NBMAX >= N`) or `dsytri2x` (otherwise). The JS port drops ILAENV entirely per project convention and hardcodes `NBMAX = 32`. The same threshold must be repeated in the BLAS-style wrapper (`dsytri2.js`) because it sizes `WORK` based on which branch will be taken — keep the constant in sync between `lib/base.js` and `lib/dsytri2.js` (and the test file which also references it for `makeWork`).
- Fortran `ILAENV` actually returns `NB = 1` for `DSYTRI2` (no specific tuning entry in `ilaenv.f`), so the reference implementation almost always reaches `DSYTRI2X` with a degenerate `NB=1` block size. We deliberately diverge by picking the conventional `NBMAX = 32`, which yields a sharper unblocked/blocked split and matches the JS-side `dsytri`/`dsytri2x` test coverage. The blocked path is therefore exercised only when `N > 32`, which the 40x40 fixtures cover.
- Removed all `LWORK` plumbing including the workspace-query short-circuit (`LWORK == -1`). The JS `ndarray.js` takes `WORK` as a pre-sized array; the layout wrapper allocates internally.
- `XERBLA` argument-error handling is replaced by validator throws in `ndarray.js` / `dsytri2.js`. Base.js does no validation.
- This is a direct real-prefix port of `zsytri2` (just-translated complex sibling). Structure was lifted verbatim, replacing `Complex128Array` with `Float64Array`, dropping `reinterpret`, and adjusting fixture indexing (no `2*` complex re/im factor).

## Dependency interface surprises

- `dsytri` (the unblocked sibling) needs `WORK` of length `N`. `dsytri2x` (the blocked sibling) needs `WORK` of length `(N+nb+1)*(nb+3)`. The same `WORK` slot in the API serves both dispatch branches — caller must size for whichever branch the routine will take. The layout wrapper handles this automatically; ndarray callers must allocate the larger of the two when uncertain.
- Both `dsytri` and `dsytri2x` return `info` as the JS return value (not an out-parameter). Just forward the return value.

## Coverage gaps

- The `info > 0` (singular `D`) path is not exercised by fixture tests because `dsytrf` does not produce a zero `1x1` pivot for well-conditioned test matrices. Same gap as `dsytri`/`dsytri2x`.

## Fortran deps

- `dsytrf` (driven by the Fortran test) brings transitive deps `dlasyf -> dsytf2 -> dsyr` plus `dlamch`, `disnan`, `dlaisnan`, and the ILAENV chain (`ilaenv`, `ieeeck`, `iparmq`). `deps.py` does not pull these — copied from `deps_dsytri2x.txt` and added the additional siblings (`dsytri`, plus all `dsytri2x` deps).
