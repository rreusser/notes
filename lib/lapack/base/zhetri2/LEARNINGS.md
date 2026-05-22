# zhetri2: Translation Learnings

## Routine character

-   `zhetri2` is a trivial dispatcher: it picks between the unblocked
    `zhetri` and the blocked `zhetri2x` based on `ILAENV(1, 'ZHETRF', ...)`
    vs `N`. The entire algorithm lives in those two leaves. The JS port
    is ~10 lines plus the dispatch.

## Translation pitfalls

-   The Fortran source has a workspace-query path (`LWORK = -1` writes
    `MINSIZE` to `WORK(1)`). Per project convention this is dropped — the
    caller sizes `WORK` themselves. The `LWORK` parameter is therefore
    consumed (not exposed in JS), exactly like `LDA` and `INFO`.
-   `ILAENV(1, 'ZHETRF', ...)` is replaced with a hardcoded `NBMAX = 32`
    (the convention documented in the skill). This means the dispatch
    threshold differs slightly from a tuned Fortran build, but downstream
    semantics are identical because `zhetri` and `zhetri2x` produce the
    same inverse to within floating-point rounding.

## Dependency interface surprises

-   `zhetri` and `zhetri2x` accept the *same* ndarray-style argument list
    (`uplo, N, A, sa1, sa2, oA, IPIV, sI, oI, WORK, sW, oW`), except that
    `zhetri2x` has an extra trailing `nb` argument. This made the
    dispatcher essentially mechanical.
-   Both leaves use the same `IPIV` convention (non-negative = 0-based
    row index for 1x1 pivots; negative = bitwise-NOT-encoded for 2x2),
    so the dispatcher passes `IPIV` straight through without
    re-encoding.

## Fortran test notes

-   `deps_zhetri2.txt` from `bin/init_routine.py` was missing the
    transitive deps of `ZHETRF` (`zhetrf`, `zhetf2`, `zlahef`, `zlacgv`,
    `dlamch`, `dlapy2`, `disnan`, `dlaisnan`). Cross-checked against
    `deps_zhetri2x.txt` and merged its dependency set in.
-   To exercise the blocked path, the Fortran test allocates a 40x40
    diagonally-dominant Hermitian matrix (so `N=40 > NBMAX=32`). All
    smaller cases (n=4, n=5) exercise the unblocked `zhetri` path.
    `NMAX = 40` keeps the column padding visible to the JS test as
    `FIXTURE_LDA = 40`.
-   Complex literal gotcha: `(3.0d0 + dble(i), 0.0d0)` is not a valid
    Fortran complex constructor when the components are runtime
    expressions. Use `dcmplx(...)`.

## Coverage

-   With 21 tests covering both dispatch arms, both UPLO branches, 1x1
    and 2x2 pivot configurations, the N=0 quick return, N=1 trivial
    case, the singular-D guard, and all validator failure modes, the
    base+ndarray+wrapper paths are exhaustively exercised.
