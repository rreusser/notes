Run the full translation checklist for a BLAS/LAPACK routine. The argument is the routine name (e.g. `dpotf2`).

Follow the checklist in CLAUDE.md exactly:

1. Run `python bin/deps.py <routine>` — check all deps are implemented
2. Run `python bin/signature.py` on the source file — note the target signature
3. Read the Fortran source to understand the algorithm
4. Write the Fortran test (`test/fortran/test_<routine>.f90`)
5. Generate the fixture (`./test/run_fortran.sh <package> <routine>`)
6. Scaffold the module (`python bin/scaffold.py <package> <routine>`)
7. Implement `lib/base.js`
8. Write tests in `test/test.js`
9. Run `npm test` to verify

CRITICAL: Follow the automation-first rules in CLAUDE.md. After each manual step, ask yourself if it was mechanical and could be automated.
