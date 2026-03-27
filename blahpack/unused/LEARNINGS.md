# Learnings

## zggev: Driver Routine for Generalized Eigenvalues

### Stride Convention Inconsistencies

The most critical finding: **different routines in the codebase use different stride conventions**, and mixing them up causes silent data corruption.

Two conventions coexist:
1. **Double-based strides** (stride1=2, stride2=2*LDA for column-major complex):
   Used by: `zgghrd`, `zhgeqz`, `zggbal`, `zggbak`, `zlaset`, `zrot`
2. **Complex-element strides** (stride1=1, stride2=LDA):
   Used by: `zgeqrf`, `zgeqr2`, `zlarf`, `zlarfb`, `zlarft`, `zlange`, `zlascl`, all BLAS routines

If a routine's source code contains `sa1 = strideA1 * 2`, it expects complex-element strides. If it directly uses `offset + i * strideA1` for real parts and `+1` for imaginary, it expects double strides.

### Workspace Array Aliasing Bugs

TAU and WORK must not alias. If TAU occupies `WORK[0..K*2-1]` and subsequent operations (zunmqr, zungqr) also use WORK[0..] as scratch, they clobber TAU values. Fix: allocate TAU as separate `new Float64Array(2*N)`.

Similarly, ztgevc uses `RWORK[0..2N-1]` for norms, clobbering the LSCALE/RSCALE data that zggbak needs later. Fix: allocate a separate RWORK for ztgevc.

### zung2r (ZUNGQR unblocked)

The standard ZUNG2R algorithm is fragile due to in-place modification ordering. Workaround: generate Q by applying reflectors to identity via `zunm2r('L', 'N', ...)`, which is more reliable.

### Helper routines implemented as local files

`zunm2r.js`, `zunmqr.js`, `zungqr.js`, `ztgevc.js`, `zlacpy.js` -- all in `lib/lapack/base/zggev/lib/`.

### Fortran test deps for driver routines

Must include: `ilaenv`, `ieeeck`, `iparam2stage`, `iparmq`, `la_constants`, `la_xisnan`, `zunm2r`, `zung2r`, `ztgevc`, `zlacpy`, plus all their transitive deps.

---

## zgeqrf Implementation

### Blocked QR factorization algorithm

`zgeqrf` implements blocked QR factorization by splitting the matrix into panels
of width NB (block size). For each panel:

1. Factor the panel using the unblocked algorithm (`zgeqr2`).
2. Form the triangular factor T of the block reflector (`zlarft`).
3. Apply the block reflector to the remaining columns (`zlarfb`).

Any trailing columns that don't fill a full block are handled by a final
unblocked `zgeqr2` call.

### Key design decisions

- **NB hardcoded to 32**: Fortran LAPACK queries ILAENV for optimal block size.
  In JS we hardcode NB=32, which is a reasonable default. This simplifies the
  code by removing ILAENV/LWORK workspace query logic entirely.

- **Internal WORK allocation**: When WORK is null or too small, the routine
  allocates internally. This avoids exposing LWORK workspace-size queries to
  the JS caller -- a Fortran-ism that doesn't belong in JS APIs.

- **T matrix allocated per call**: The NB x NB triangular factor T is allocated
  as a fresh Float64Array on each call. For NB=32, this is 2*32*32 = 2048
  doubles (16KB), which is negligible.

### Calling conventions for dependencies

All complex routines use interleaved real/imaginary pairs (Float64Array).
Strides are in complex-element units (not doubles):

- `zgeqr2(M, N, A, strideA1, strideA2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK)`
- `zlarft(direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT)`
- `zlarfb(side, trans, direct, storev, M, N, K, V, strideV1, strideV2, offsetV, T, strideT1, strideT2, offsetT, C, strideC1, strideC2, offsetC, WORK, strideWORK1, strideWORK2, offsetWORK)`

Note that `zlarfb` takes 2D WORK strides (strideWORK1, strideWORK2) while
`zgeqr2` takes 1D WORK stride (strideWORK).

### Offset arithmetic for panel indexing

When passing sub-panels to subroutines, the offset into the interleaved array
is computed as:

```
offsetA + 2 * (i * strideA1 + j * strideA2)
```

The factor of 2 accounts for interleaved real/imaginary pairs. Strides
(strideA1, strideA2) are in complex-element units.

### Fortran test compilation on macOS

- macOS has a case-insensitive filesystem, so `iparam2stage.f` resolves to
  `iparam2stage.F`. But gfortran treats `.f` as fixed-form without
  preprocessing and `.F` as fixed-form with preprocessing. Added `-cpp` flag
  to `run_fortran.sh` to always enable C preprocessing.

- `iparam2stage.F` references OpenMP symbols; with `-cpp`, the `#if
  defined(_OPENMP)` guards properly exclude those references.

### Coverage

All branches in `zgeqrf/lib/base.js` are covered (100% line, branch, and
function coverage) by 10 test cases:

- Small matrices (4x3, 3x3, 5x2, 2x5, 1x1): exercise the unblocked-only path
- M=0, N=0: exercise quick returns
- Offset test: verifies offsetA/offsetTAU work correctly
- Null WORK: verifies internal allocation
- Large 40x35 matrix: exercises the blocked code path (NB=32, one block + remainder)
