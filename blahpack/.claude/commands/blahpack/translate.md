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
11. Fill in `lib/<package>/base/<routine>/LEARNINGS.md` with ACTUAL findings (not the template placeholders)
12. Verify LEARNINGS.md is filled in: `grep -c "TODO: Fill in" lib/<package>/base/<routine>/LEARNINGS.md` must return 0

CRITICAL: The translation is NOT COMPLETE until step 12 passes. A LEARNINGS.md with "TODO: Fill in" or template placeholders is a failure. Write at least one concrete bullet point per section, even if it's "No issues encountered." If a section truly doesn't apply, replace the placeholder with "N/A — no complex arithmetic in this routine" or similar.
