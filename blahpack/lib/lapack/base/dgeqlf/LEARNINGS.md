# dgeqlf: Translation Learnings

## Translation pitfalls

- The Fortran loop `DO I = K-KK+KI+1, K-KK+1, -NB` walks panels from
  the bottom-right of the K-leading submatrix up to the top-left. In
  0-based JS this becomes `for (i = (K-kk)+ki; i >= (K-kk); i -= nb)`.
- Each panel has "effective M" equal to `M - K + i + ib` (0-based),
  i.e. `M-K+I+IB-1` in 1-based Fortran. The panel is anchored at
  `A(0, N-K+i)` — it always starts at the top of the matrix, with its
  bottom ending `K-i-ib` rows from the matrix bottom.
- After the blocked loop terminates, `MU = M - KK`, `NU = N - KK`
  (directly derivable from the fact that the loop stops at
  `i = K-kk`; no need to translate Fortran's post-loop `I` value).
- The `backward` / `columnwise` direction passed to `dlarft`/`dlarfb`
  is critical — QL reflectors accumulate from the bottom up, in
  contrast to QR which uses `forward`.

## Workspace

- `LWORK` is a declared parameter but the base routine allocates
  internal workspace of size `N*NB` when the provided `WORK` is
  missing or too small. The user-supplied `lwork` argument is unused
  (suppressed via `eslint-disable no-unused-vars`) to preserve
  signature symmetry with the Fortran API.

## Testing

- With `NX = 128`, the blocked code path is only taken when
  `K > 128`. A 65x65 matrix falls entirely through the unblocked
  cleanup — you need at least ~150x150 to exercise the blocked
  branch and get full coverage of the panel loop and the
  `dlarft`/`dlarfb` calls.
