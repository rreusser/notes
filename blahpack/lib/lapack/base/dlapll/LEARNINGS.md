# dlapll: Translation Learnings

## Translation pitfalls

- ssmin is an output-only scalar. Passed as a Float64Array of length 1 (ssmin[0]).
- dlarfg signature: `dlarfg(N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau)` -- alpha is passed as array+offset, and the second call uses `y[offsetY+strideY]` as alpha and `y[offsetY+2*strideY]` as the start of x.
- dlas2 returns via output array `out[0] = ssmin, out[1] = ssmax`.
- Module-level tauArr and svdOut arrays are reused across calls (thread-unsafe but fine for single-threaded JS).

## Dependency interface surprises

- dlarfg takes alpha as (array, offset) pair, not a scalar. The offset for the second call must skip the first element: `offsetY + strideY`.

## Missing automation

- N/A

## Coverage gaps

- The quick-return path (N<=1) is covered by the n_equals_1 test.

## Complex number handling

- N/A -- real-only routine.
