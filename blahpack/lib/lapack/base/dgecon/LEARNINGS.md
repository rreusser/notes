# dgecon: Translation Learnings

## Translation pitfalls

- RCOND is an output parameter: must be passed as Float64Array[1], not scalar.
- The Fortran code uses WORK(N+1) for the V array in dlacn2. In JS with
  stride-based indexing, this becomes offsetWORK + N * strideWORK.
- WORK is partitioned into 4 segments of N: [x, v, cnorm_lower, cnorm_upper].
  The dlatrs calls use different CNORM offsets for lower vs upper triangular.
- The norm='O' (letter O) is treated identically to '1'. The Fortran uses
  LSAME which is case-insensitive.
- DISNAN check: Fortran calls DISNAN(ANORM); JS uses `anorm !== anorm`.
- The loop around dlacn2 uses a labeled goto in Fortran (GO TO 10/20); in JS
  this becomes a while(true) loop with break.

## Dependency interface surprises

- dlacn2 takes EST and KASE as Int32Array/Float64Array (mutable containers),
  not scalars. This differs from the Fortran interface where they are plain
  scalar in/out parameters.
- dlatrs returns scale via Float64Array[1], and the caller must combine
  sl * su to get the total scale.
- The normin='Y' flag is set after the first dlacn2 iteration to skip
  recomputing column norms.

## Automation opportunities

- The dgecon/dpocon routines share nearly identical structure (reverse
  communication loop with dlatrs calls). Could be templated.

## Coverage gaps

- Lines 146-147 (scale overflow bail-out): requires dlatrs to return
  scale=0 or very tiny scale. Hard to trigger with normal matrices.
- Lines 158-159 (rcond NaN/overflow check): defensive path.
- Overall: 97.6% line / 89.66% branch.

## Complex number handling

- N/A — real-only routine.
