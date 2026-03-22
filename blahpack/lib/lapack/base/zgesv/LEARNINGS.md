# zgesv: Translation Learnings

## Translation pitfalls

- zgesv is a thin driver routine that calls zgetrf then zgetrs, identical in structure to dgesv. No indexing or stride complications in zgesv itself.
- For singular matrices, the exact info value may differ from Fortran due to different pivot orderings in the recursive zgetrf2 algorithm. Tests should check info > 0 rather than matching the exact Fortran info value.
- The Fortran test required computing b = A*x in Fortran itself (rather than hand-computing b) to avoid arithmetic errors with complex numbers. The 4x4 hand computation was wrong on first attempt.

## Dependency interface surprises

- zgesv passes Complex128Array for A and B. Strides and offsets are in complex-element units (not Float64 units). The underlying zgetrf and zgetrs handle the *2 conversion to Float64 indices internally.
- The IPIV output from zgesv is 0-based (JS convention), unlike Fortran's 1-based. Tests verify mathematical correctness (A*x = b) rather than comparing IPIV values.

## Automation opportunities

- The scaffold generator (bin/scaffold.py) failed because fparser is not installed in the environment. The module scaffold had to be created manually by copying from dgesv. This is a recurring issue when the venv is not activated.

## Coverage gaps

- base.js achieves 100% line, branch, and function coverage. The quick-return paths (N=0, NRHS=0) and the info > 0 (singular) path are all covered.

## Complex number handling

- zgesv itself does not perform complex arithmetic; it delegates entirely to zgetrf and zgetrs. The test file uses a zmatmat helper for verification that operates on Float64Array views of Complex128Arrays with interleaved real/imaginary pairs.
- Test inputs use Complex128Array constructor with flat interleaved [re, im, re, im, ...] values. Verification uses reinterpret() to get Float64Array views for element-wise comparison.
