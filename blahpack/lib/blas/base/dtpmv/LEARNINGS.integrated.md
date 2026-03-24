# dtpmv: Translation Learnings

## Translation pitfalls

- Packed storage KK pointer arithmetic requires careful mapping: Fortran's 1-based KK starts at 1 (upper) or N*(N+1)/2 (lower), which maps to `offsetAP` and `offsetAP + (N*(N+1)/2 - 1)*strideAP` in 0-based JS.
- The Fortran stride-1 specialization (INCX.EQ.1 branches) was dropped in favor of a single general-stride implementation using incremental pointers. This simplifies the code without performance loss in JS.
- Diagonal element location in packed storage differs between upper and lower. Upper: diagonal of column j is at `kk + j*strideAP`. Lower: diagonal of column j is at `kk - (N-1-j)*strideAP` (when iterating backward).
- The Fortran `KK = KK + J` (upper, no-trans) accumulates column lengths 1, 2, 3, ..., J. In JS this translates to `kk += (j+1) * strideAP` since j is 0-based.

## Dependency interface surprises

- N/A -- dtpmv is a leaf BLAS routine with no dependencies.

## Automation opportunities

- Packed-storage BLAS routines (dtpmv, dtpsv, dspmv, dspr, dspr2, dtbmv, dtbsv) all share the same KK-pointer pattern. A common transform could handle the 1-based-to-0-based KK arithmetic.

## Coverage gaps

- 100% line, branch, and function coverage achieved. All 8 parameter combinations (upper/lower x transpose/no-transpose x unit/non-unit) are covered, plus stride and edge cases.

## Complex number handling

- N/A -- dtpmv is a real (double precision) routine.
