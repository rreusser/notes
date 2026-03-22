# dlaev2: Translation Learnings

## Translation pitfalls

- Returns FOUR values (rt1, rt2, cs1, sn1) via Fortran output parameters. In JS, return an object `{ rt1, rt2, cs1, sn1 }` following the dlartg pattern.
- The eigenvector computation has a sign correction step at the end (`if sgn1 === sgn2, swap cs1 and sn1 with sign flip`). This is easy to miss or get wrong.
- The `ab === 0` guard (when both b=0 and cs=0) prevents division by zero. Must be checked before `tn = -cs / tb`.
- The eigenvalue computation is identical to dlae2. Could potentially share code, but keeping them independent matches the Fortran reference structure.

## Dependency interface surprises

- N/A - dlaev2 has no dependencies.

## Automation opportunities

- The eigenvalue portion of dlaev2 is identical to dlae2. A future refactor could extract this shared logic, but the Fortran reference keeps them separate.

## Coverage gaps

- 100% line and branch coverage achieved. All branches for eigenvalue computation (sm < 0, sm > 0, sm = 0), eigenvector computation (acs > ab, ab = 0, else), and sign correction (sgn1 = sgn2 vs not) are exercised.

## Complex number handling

- N/A - real-only routine. The complex analogue is zlaev2.
