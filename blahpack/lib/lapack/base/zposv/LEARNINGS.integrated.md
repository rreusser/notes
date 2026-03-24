# zposv: Translation Learnings

## Translation pitfalls

- zposv is structurally identical to dposv: factorize (zpotrf), then solve (zpotrs) if factorization succeeds. The only difference is operating on Complex128Array instead of Float64Array.
- Unlike dgesv, zposv has no pivot array (IPIV). The Cholesky factorization has no row interchanges.

## Dependency interface surprises

- zpotrf and zpotrs both take Complex128Array with strides/offsets in complex elements. The uplo parameter passes through directly with no mapping needed.
- zpotrf returns info > 0 when the matrix is not positive definite (the k-th leading principal minor is not positive). This is correctly forwarded without calling zpotrs.

## Automation opportunities

- The real-to-complex driver pattern (dposv -> zposv) is purely mechanical: replace dpotrf->zpotrf, dpotrs->zpotrs, Float64Array->Complex128Array in the signature. A template could generate this.

## Coverage gaps

- 100% line and branch coverage achieved. The not-positive-definite test exercises the early return path (info > 0 from zpotrf). Both upper and lower paths tested via the 3x3 HPD matrix, along with N=0, NRHS=0 quick returns and identity matrix test.

## Complex number handling

- No direct complex arithmetic in zposv. All complex operations are delegated to zpotrf and zpotrs. The only Complex128Array handling is passing parameters through.
