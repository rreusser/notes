---
name: blahpack-signature
description: Generate the stdlib-js call signature for a BLAS/LAPACK routine
argument-hint: <routine>
---

Generate the stdlib-js call signature for a BLAS/LAPACK routine. Run:

```
python bin/signature.py $ARGUMENTS
```

The argument should be a path to a Fortran source file, e.g. `data/BLAS-3.12.0/daxpy.f` or `data/lapack-3.12.0/SRC/dpotf2.f`.

If the user provides just a routine name like `daxpy`, resolve it: check `data/BLAS-3.12.0/<name>.f` first, then `data/lapack-3.12.0/SRC/<name>.f`.
