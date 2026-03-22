# zung2l: Translation Learnings

## Translation pitfalls

- Direct analog of dorg2l. The key index mapping for the diagonal: Fortran `A(M-N+J, J) = ONE` maps to 0-based `A[oA + (M-N+j)*sa1 + j*sa2] = (1,0)`.
- zlarf and zscal calls match the pattern in zung2r but with the QL reflector structure (from bottom of column upward).
- Fortran `ZSCAL(M-N+II-1, -TAU(I), ...)` count maps to `M-N+ii` elements in 0-based.

## Dependency interface surprises

- zlarf takes tau as Complex128Array + offset (not scalar), unlike dlarf which takes a scalar Float64.
- zscal takes a Complex128 scalar, not a Complex128Array. Required `new Complex128(-tauv[it], -tauv[it+1])`.

## Automation opportunities

- Pattern is identical to zung2r -> dorg2l translation. Could be automated as a "real-to-complex unblocked generator" transform.

## Coverage gaps

- The `ii > 0` condition in zlarf (skipping for first column when N=K) is covered by the K=3 test.
- The `M-N+ii > 0` condition for zscal is covered when M > N.

## Complex number handling

- Setting elements to zero/one requires zeroing both real and imaginary parts.
- `A(M-N+ii, ii) = 1 - TAU(i)` becomes `Av[ia] = 1 - tauv[it]; Av[ia+1] = -tauv[it+1]` (complex subtraction from 1).
- negTau constructed as `new Complex128(-re, -im)` for zscal.
