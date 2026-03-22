# Dependency Calling Convention Reference

Surprises that have caused integration bugs. Check this before calling
a routine for the first time.

| Routine | Gotcha |
|---------|--------|
| `dlarfg`/`zlarfg` | alpha is `(array, offset)`, not a scalar — modified in-place |
| `dlarf` vs `zlarf` | Real: tau is a plain number. Complex: tau is `(Float64Array, offset)` |
| `zlarfb` | Takes 2D WORK strides `(strideWORK1, strideWORK2)` |
| `zgeqr2` | Takes 1D WORK stride `(strideWORK)` |
| `zlaqps` | Returns KB as a scalar (Fortran uses output parameter) |
| `zladiv` | `zladiv(x, y, out)` writes to `out` array, does NOT return result |
| `dtrsm` | Works correctly with same array A as both operator and target |

Add new entries here as they are discovered.
