# idamax: Translation Learnings

## Translation pitfalls

- [x] Fortran IDAMAX returns 1-based index; JS version returns 0-based. Tests must subtract 1 from fixture values.
- [x] Fortran returns 0 for N<1 or INCX<=0; JS returns -1 (stdlib convention for "no index found").
- [x] Fortran has a stride-1 specialization path; JS drops this (per project convention) and uses a single general loop.

## Dependency interface surprises

- [x] None. IDAMAX is a leaf routine with no dependencies.

## Automation opportunities

- [x] The 1-based to 0-based return value conversion is a pattern shared with izamax. Both return index-1.
- [x] The `strideX <= 0 => return -1` guard is identical to izamax.

## Coverage gaps

- [x] None. 100% line, branch, and function coverage achieved.

## Complex number handling

- [x] N/A. This is the real-valued variant. Uses `Math.abs()` instead of `|Re| + |Im|` (izamax).
