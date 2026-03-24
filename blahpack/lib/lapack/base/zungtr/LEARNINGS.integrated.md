# zungtr: Translation Learnings

## Translation pitfalls

- Direct analog of dorgtr. The reflector shifting logic (copy column j+1 -> j for upper, shift right for lower) operates on complex elements via Float64Array view, requiring `sa1 = strideA1 * 2`, `sa2 = strideA2 * 2`.
- For UPLO='L', the submatrix offset for zungqr is `pa = offsetA + strideA1 + strideA2` in complex-element addressing (one row and one column down), same as dorgtr.
- For UPLO='U', zungql operates on the leading (N-1)x(N-1) submatrix starting at offsetA (no offset change), also same as dorgtr.

## Dependency interface surprises

- zungql and zungqr both take complex-element strides/offsets. The lwork parameter is passed through from the caller.
- N/A for the routines themselves; their conventions match expectations.

## Automation opportunities

- dorgtr -> zungtr translation is mechanical. Only differences: reinterpret for Float64 view, multiply strides by 2, use `Av[ia] = 1.0; Av[ia+1] = 0.0` instead of `A[ia] = 1.0`, replace dorgqr/dorgql with zungqr/zungql.

## Coverage gaps

- Both UPLO='U' and UPLO='L' paths are fully covered.
- N=0 and N=1 edge cases covered.
- 98.72% line coverage, 91.67% branch coverage.

## Complex number handling

- Element copying requires copying both real and imaginary parts: `Av[ia] = Av[src]; Av[ia+1] = Av[src+1]`.
- Setting elements to (1,0) or (0,0) requires two assignments.
- No complex arithmetic operations; only element copying and zero/one assignments.
