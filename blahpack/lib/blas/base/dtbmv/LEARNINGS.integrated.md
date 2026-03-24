# dtbmv: Translation Learnings

## Translation pitfalls

- dtbmv is structurally identical to dtbsv but with different operations: multiply/add instead of divide/subtract. The loop directions are also swapped (dtbsv: upper/no-transpose goes backward for back-substitution; dtbmv: upper/no-transpose goes forward for matrix-vector multiply).
- The `kx` pointer tracking for non-unit stride requires careful attention to when `kx` updates. In the no-transpose/upper branch, `kx` advances when `j >= K`; in the transpose/upper branch, `kx` decrements at the start of each iteration (before the inner loop).
- For the transpose branches, the Fortran uses a different kx update pattern than no-transpose: kx is advanced/decremented inside the j-loop body rather than conditionally. This was directly ported from the Fortran reference.

## Dependency interface surprises

- N/A. dtbmv is a leaf routine with no BLAS/LAPACK dependencies.

## Automation opportunities

- The dtbsv -> dtbmv translation could be semi-automated: swap `/=` to `*=`, `-=` to `+=`, and reverse loop directions. A transform script for "solve to multiply" banded triangular routines would cover dtbsv/dtbmv and potentially ztbsv/ztbmv.

## Coverage gaps

- 100% line, branch, and function coverage achieved with 7 test cases covering: upper/lower x no-transpose/transpose x unit/non-unit, n=0, stride=2.

## Complex number handling

- N/A. This is a real-valued (d-prefix) routine.
