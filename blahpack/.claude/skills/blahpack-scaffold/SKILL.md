---
name: blahpack-scaffold
description: Generate the stdlib-js module scaffold for a BLAS/LAPACK routine
argument-hint: <routine>
---

Generate the stdlib-js module scaffold for a BLAS/LAPACK routine. Run:

```
python bin/scaffold.py $ARGUMENTS
```

The first argument is the package (`blas` or `lapack`), the second is the routine name. Add `-d "description"` for the one-line description.

If the user provides just a routine name, determine the package by checking whether the source exists in `data/BLAS-3.12.0/` (blas) or `data/lapack-3.12.0/SRC/` (lapack).

After scaffolding, remind the user that `lib/base.js` contains a stub that needs to be implemented.
