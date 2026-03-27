# Blahpack — Fortran BLAS/LAPACK to JavaScript

Semi-automated translation of reference BLAS/LAPACK to idiomatic JavaScript,
conforming to [stdlib-js](https://github.com/stdlib-js/stdlib) conventions.

stdlib-js reference clone: `/Users/rreusser/gh/stdlib-js/stdlib/`

## Project Structure

```
bin/                           # Pipeline scripts
  transform.py                 #   Composable code-mod pipeline
  signature.py                 #   Fortran → stdlib-js signature generator
  scaffold.py                  #   Module scaffold generator (stdlib-js structure)
  deps.py                      #   Dependency tree analyzer
  fortran_body.py              #   Strip Fortran to executable body only
  gen_test.py                  #   Generate JS test scaffold from fixtures
  init_routine.py              #   Single command: scaffold + deps + test scaffold
  audit.sh                     #   Convention audit (per-module or full codebase)
  lint.sh                      #   ESLint wrapper
  check-stub-tests.sh          #   Detect scaffold-only test stubs
data/                          # Reference Fortran source
  BLAS-3.12.0/
  lapack-3.12.0/
lib/                           # Output: stdlib-js conformant modules
  blas/base/<routine>/         #   e.g. lib/blas/base/ddot/
  lapack/base/<routine>/       #   e.g. lib/lapack/base/dpotf2/
test/                          # Fortran tests and fixtures
  fortran/                     #   Test programs (test_<routine>.f90)
  fortran/deps_<routine>.txt   #   LAPACK link dependencies
  fixtures/                    #   Reference outputs from Fortran (JSONL)
docs/                          # Reference documentation
  complex-numbers.md           #   Complex128Array patterns, arithmetic rules
  dependency-conventions.md    #   Calling convention gotchas
  review-guidelines.md         #   Review checklist (also in /blahpack-review skill)
  ndarray-conformance.md       #   ndarray.js validation spec
  goto-patterns.md             #   Fortran GOTO → JS restructuring
  performance-patterns.md      #   Optimization patterns with code examples
```

## Commands

```bash
python                          # Use venv python (NOT python3)
gfortran                       # GNU Fortran compiler (Homebrew)
node                            # Node.js v24+ (node:test built-in)
npm test                        # Run all JS tests
bash bin/audit.sh               # Full codebase convention audit
bash bin/audit.sh lib/<path>    # Audit a single module
bin/check-stub-tests.sh         # Check for scaffold-only test stubs
bin/lint.sh lib/<path>/base.js  # Lint a single file
```

## Skills

Use these skills for translation and review workflows:

- `/blahpack-translate <routine>` — Full end-to-end translation checklist
  with all conventions, pitfalls, string tables, and quality gates.
  This is the primary workflow for translating a new routine.

- `/blahpack-review [module-path]` — Review a module (or full codebase)
  for convention violations, scaffolding remnants, and quality issues.
  Runs `bin/audit.sh` and applies the full review checklist.

- `/blahpack-scaffold <package> <routine>` — Generate module scaffold
- `/blahpack-signature <routine>` — Generate stdlib-js call signature
- `/blahpack-deps <routine>` — Show dependency tree
- `/blahpack-status` — Show translation status
- `/blahpack-coverage` — Run test coverage analysis

## Automation-First Mindset

Any manual work that repeats across routines is a bug in the tooling. If you
perform the same mechanical transformation twice, stop and automate it (new
transform in `bin/transform.py` or script in `bin/`) before continuing.

## Known Limitations

- Fortran `.f90` files that use modules (`USE la_constants`) cannot be
  compiled by `run_fortran.sh` without module compilation ordering.
  Work around: write JS tests with hand-computed expected values.
- **`deps.py` misses transitive Fortran-only dependencies.** Routines
  that call `ILAENV` transitively need `ilaenv`, `ieeeck`, `iparmq` in
  their Fortran deps file for test compilation. These are not JS
  dependencies (ILAENV is replaced with hardcoded constants), but
  `deps_<routine>.txt` must include them for `run_fortran.sh` to link.
  Check the deps files of similar routines when compilation fails with
  undefined references.
