Run the full translation checklist for a BLAS/LAPACK routine. The argument is the routine name (e.g. `dpotf2`).

Determine the package (blas or lapack) by checking if the source exists in `data/BLAS-3.12.0/` or `data/lapack-3.12.0/SRC/`.

1. `python bin/deps.py $ARGUMENTS` — check all deps are implemented
2. `python bin/init_routine.py <package> <routine> -d "<description>"` — scaffold + deps + test scaffold
3. Read stripped Fortran: `python bin/fortran_body.py <source-path>`
4. Write the Fortran test (`test/fortran/test_<routine>.f90`)
5. Generate fixture: `./test/run_fortran.sh <package> <routine>`
6. Regenerate JS test scaffold: `python bin/gen_test.py <package> <routine> > lib/<package>/base/<routine>/test/test.js`
7. Implement `lib/<package>/base/<routine>/lib/base.js`
8. Fill in JS test inputs and assertions
9. Run tests: `node --test lib/<package>/base/<routine>/test/test.js`
10. Check coverage: `node --test --experimental-test-coverage lib/<package>/base/<routine>/test/test.js`
11. Write `lib/<package>/base/<routine>/LEARNINGS.md` — translation pitfalls, dependency surprises, missing automation, coverage gaps

CRITICAL: Step 11 (LEARNINGS.md) is REQUIRED, not optional. Follow the automation-first rules in CLAUDE.md. After each manual step, ask yourself if it was mechanical and could be automated.
