# Questions

- `IPIV` is a consistently named array. It typically has type `Int32Array`, but its values are called out as `NonNegativeInteger`. Is signed really the correct type for this?
- `IPIV` is a very Fortran-style name. Should we just call it `pivot`?
- `DELTA` and `WORK` are the sign conventions it chose. `delta` and `work` would look nicer.

