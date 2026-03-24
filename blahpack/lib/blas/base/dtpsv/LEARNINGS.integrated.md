# dtpsv: Translation Learnings

## Translation pitfalls

- Packed storage index tracking (`kk`) is 1D and advances differently for upper vs lower: upper decrements by `j+1` per column (going backward), lower increments by `N-j` per column (going forward). Converting from 1-based Fortran to 0-based JS: `kk` starts at `offsetAP + (N*(N+1)/2 - 1)*strideAP` for upper (last diagonal), `offsetAP` for lower (first diagonal).
- The inner loops use two parallel pointers (`ip` for AP, `ix` for x) that advance independently. The Fortran uses K as the loop variable for the AP pointer and separately tracks IX for x. In JS, the for-loop condition is on `ip` (the AP pointer) while `ix` is manually advanced.
- For the no-transpose branches, the `x[jx] !== 0.0` guard is an optimization that skips zero RHS entries. This means the loop structure is: check if nonzero, optionally divide by diagonal, then subtract from neighbors. The transpose branches do not have this guard since they accumulate first and set the result at the end.

## Dependency interface surprises

- N/A -- dtpsv has no BLAS/LAPACK dependencies (self-contained Level 2 routine).

## Automation opportunities

- The packed-storage pattern (dtpmv, dtpsv, dspmv, dspr, dspr2, etc.) shares the same KK-tracking loop structure. A generalized packed-storage template could reduce manual work for future packed routines.

## Coverage gaps

- 100% line, branch, and function coverage achieved with 18 tests covering all 8 uplo/trans/diag combinations, N=0, N=1, stride=2, stride=-1, zero RHS, and offset parameters.

## Complex number handling

- N/A -- dtpsv is a real (double precision) routine.
