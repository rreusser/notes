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
  fortran_to_estree.py         #   Fortran parser → ESTree AST (outputs 1-based JS)
  estree_to_js.js              #   ESTree → formatted JavaScript
  remove_goto.py               #   GOTO restructuring (imported by transform.py)
  instrument.js                #   Module._load profiler (incl/excl time, call trees)
  bench-zggev.js               #   zggev profiling benchmark (multi-size, scaling)
  bench-blas.js                #   Leaf-node BLAS/LAPACK microbenchmarks
  profile-zhgeqz.js            #   V8 CPU profiler with per-line breakdown
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
pipeline/                      # Intermediate transform files
unused/                        # Deprecated experiments and old files
```

## Commands

```bash
python                          # Use venv python (NOT python3)
gfortran                       # GNU Fortran compiler (Homebrew)
node                            # Node.js v24+ (node:test built-in)
npm test                        # Run all JS tests
bin/lint.sh lib/<path>/base.js  # Lint a single file
bin/lint.sh lib/blas/base/*/lib/base.js lib/lapack/base/*/lib/base.js  # Lint all
```

---

## CRITICAL: Automation-First Mindset

This pipeline will be applied to hundreds of routines. Any manual work that
repeats across routines is a bug in the tooling, not an inherent cost.

### Rules for the AI assistant

**1. After every manual edit, ask: "Was this mechanical?"**

If you just performed a transformation that could be described as a rule
(e.g., "replace all `lda` with `sa2`", "wrap every array subscript in
`offsetA + ...`", "convert `for (i = 1; i <= n` to `for (i = 0; i < N`"),
then STOP. Do not continue to the next routine. Instead:

- State clearly: "I just performed a mechanical transformation: [description].
  This should be automated."
- Propose a new transform for `bin/transform.py` or a standalone script in `bin/`.
- Implement the automation.
- Verify it produces identical output to the manual step on the current routine.
- THEN proceed to the next routine using the new automation.

**2. Track repeated patterns in a log.**

When working through multiple routines, maintain a running tally at the end
of your response of manual steps you performed and how many times. Example:

```
## Automation Candidates
- Replaced `lda` → `sa2` in 3 routines (should be a transform)
- Added CommonJS boilerplate to 3 routines (should be a transform)
- Wrote identical test scaffolding 3 times (should be a generator)
```

If any item reaches count 2, stop and automate it before continuing.

**3. When automating, follow this process:**

a. Define the transform clearly: input pattern → output pattern.
b. Implement it (new transform in `bin/transform.py`, or new script in `bin/`).
c. Test it: run on the current routine, diff output against the manual result.
d. Register it: add to `python bin/transform.py --list` if it's a code-mod.
e. Document it: update this file's "Available transforms" list.
f. Then re-run the checklist using the new automation.

**4. The end state is: the human types one command, reviews the output, and
approves or requests changes.** Every manual step is a waypoint, not a destination.

---

## Checklist: Translating a New Routine

This is the end-to-end process for translating a Fortran routine to JavaScript.
Follow these steps in order. Each step has a verification gate.

### Step 0: Check dependencies and generate the expected signature

```bash
# Show full dependency tree
python bin/deps.py <routine>

# Show implementation order (leaves first)
python bin/deps.py <routine> --order
```

Ensure all dependencies are already implemented before starting a new routine.
If not, implement the leaves first.

```bash
python bin/signature.py data/BLAS-3.12.0/<routine>.f
# or
python bin/signature.py data/lapack-3.12.0/SRC/<routine>.f
```

This outputs the target `base.js` function signature in stdlib-js convention.
Save this — it defines the API contract for the final JS implementation.

### Step 1: Initialize the module

```bash
python bin/init_routine.py <package> <routine> -d "<one-line description>"
```

This single command:
- Generates the complete stdlib-js module scaffold (package.json, index.js, etc.)
- Auto-generates the Fortran deps file from the dependency tree
- Generates a JS test scaffold if a fixture already exists
- Prints a summary with the exact commands for remaining steps

### Step 2: Write the Fortran test

Create `test/fortran/test_<routine>.f90` using the `test_utils` module.

**Template:**
```fortran
program test_<routine>
  use test_utils
  implicit none
  ! Declare variables...

  ! Test case 1: basic operation
  ! Set up inputs...
  call <ROUTINE>(args...)
  call begin_test('basic')
  call print_array('result', result_array, n)  ! or print_scalar / print_int
  call end_test()

  ! Test case 2: edge case (n=0)...
  ! Test case 3: edge case (n=1)...
  ! etc.
end program
```

**Required test cases:**
- Basic operation with known inputs/outputs
- Edge cases: n=0 (quick return), n=1
- Scalar edge cases: alpha=0, alpha=1, beta=0, beta=1 (where applicable)
- Non-unit strides: incx=2, incy=-1 (for BLAS with stride params)
- For LAPACK with INFO: test a case that returns INFO > 0

**For complex arrays**, use EQUIVALENCE to print interleaved re/im pairs:
```fortran
double precision :: zx_r(20)
complex*16 :: zx(10)
equivalence (zx, zx_r)
call print_array('zx', zx_r, 2*n)
```

**Gate:** `./test/run_fortran.sh <package> <routine>` compiles and runs.

### Step 3: Generate fixture and JS test scaffold

```bash
./test/run_fortran.sh <package> <routine>
python bin/gen_test.py <package> <routine> > lib/<package>/base/<routine>/test/test.js
```

**Gate:** Fixture exists, JS test scaffold has one stub per case.

### Step 4: Implement base.js

Read the stripped Fortran source and translate to JavaScript:

```bash
python bin/fortran_body.py data/<source-path>/<routine>.f
```

The base.js must:
- Match the signature from Step 0 exactly
- Use 0-based indexing throughout
- Use stride/offset parameters for all array access
- Use `require()` for BLAS/LAPACK dependencies — NEVER inline them (see below)
- Use `Complex128Array` for complex array parameters, `Complex128` for complex scalars
- Use `reinterpret()` at function entry for Float64Array views
- Use `lib/cmplx.js` for complex arithmetic (scalar ops + indexed ops)
- Follow CommonJS format with stdlib-js section comments
- NOT include parameter validation (that goes in ndarray.js)
- **Implement ALL parameter combinations and code paths** (see below)

**CRITICAL: Complete implementations, not partial ones.**

Every routine must implement ALL valid parameter combinations from the
Fortran reference — not just the subset needed by the current caller.
Each module is a standalone, reusable building block. If the Fortran has
branches for SIDE='L'/'R', TRANS='N'/'T'/'C', UPLO='U'/'L',
STOREV='C'/'R', DIRECT='F'/'B', JOB='N'/'P'/'S'/'B', etc., implement
ALL of them. Do not stub out branches with `throw new Error('not yet
implemented')` just because the immediate dependency tree only exercises
one path.

Similarly, tests must cover all parameter combinations:
- If a routine has SIDE×TRANS×DIRECT×STOREV = 2×2×2×2 = 16 cases,
  test all 16 (or at minimum, every distinct code path).
- If a routine handles both M≥N and M<N, test both.
- Edge cases (N=0, M=0, alpha=0) are required regardless of whether
  any current caller triggers them.

The cost of a partial implementation is high: a future routine that calls
the incomplete path will silently fail or throw at runtime, far from the
root cause. A complete implementation is verified once and trusted forever.

**Do not inline standard LAPACK/BLAS routines.** If a helper function
corresponds to a named Fortran routine (has its own source file in the
reference LAPACK), check how many callers it has:

- **Multiple callers in reference LAPACK** → create a standalone module
  with its own `lib/`, `test/`, `package.json`. The parent routine
  `require()`s it. This is the common case.
- **Single caller only** → MAY be inlined in the parent's `base.js` as
  a local function. But it MUST still have unit tests (test it via the
  parent's test file with targeted test cases).

To check callers: `grep -rl "CALL ZFOO" data/lapack-3.12.0/SRC/`

Never duplicate a routine. If a standalone module already exists (e.g.,
`zunmqr`), always `require()` it — do not copy the implementation into
a parent routine's base.js.

**Complex arithmetic rules:** Comment all inlined complex operations with the
math they represent. **NEVER** inline complex division (`cmplx.div`), absolute
value (`cmplx.abs`), or square root — these require numerical stability
algorithms. Addition, subtraction, multiplication, conjugate, and real-scalar
scaling are safe to inline. See [docs/complex-numbers.md](docs/complex-numbers.md).

**GOTO restructuring:** See [docs/goto-patterns.md](docs/goto-patterns.md)
for the full pattern table. Common cases: `GO TO` inside DO → `continue`,
backward `GO TO` → `while`/`do-while`, forward `GO TO` → `if/else`.

### Step 5: Fill in JS tests and verify

Fill in the test stubs generated in Step 3 with actual input values
matching the Fortran test. Run:

```bash
node --test lib/<package>/base/<routine>/test/test.js
```

**Testing pitfalls (from experience):**

- **Pivoting routines (dgetrf, dgetrs, dgesv, zgeqp3):** Do NOT compare
  exact solution vectors against fixtures — pivoting tie-breaks differ
  between JS (0-based IPIV) and Fortran (1-based IPIV). Instead, verify
  the mathematical property (e.g., A*x ≈ b, or P*L*U ≈ A).
- **Blocked code paths (NB=32/64):** Require large matrices for fixtures.
  Workaround: use NB=2 for testing to exercise blocked paths with small
  matrices (dtrtri successfully used this approach).
- **Always test alpha != 1.0:** Initial tests often miss non-trivial
  scaling branches. Routines with alpha/beta parameters need explicit tests
  with e.g. alpha=2.0, beta=0.5.
- **Use well-conditioned matrices:** Near-singular or rank-deficient
  matrices cause floating-point rounding differences between blocked and
  unblocked paths. Use diagonally dominant matrices for fixture-based tests.
- **Reverse-pivot mode (dlaswp with incx<0):** Had a known bug — verify
  that backward iteration applies swaps in reverse order.

**Gate:** All tests pass against Fortran fixtures.

### Step 6: Verify test coverage

```bash
node --test --experimental-test-coverage lib/<package>/base/<routine>/test/test.js
```

Target: **≥90% line coverage, ≥85% branch coverage** on `base.js`.

If coverage is low, add targeted test cases:
- For `uplo`/`trans`/`side`/`diag` params: test ALL valid combinations
- Edge cases: N=0, M=0, alpha=0
- Overflow/underflow: test with 1e300 and 1e-300 values
- Non-unit and negative strides

**Systematically hard-to-cover paths:**
- Exceptional shifts in QZ iteration (require slow convergence)
- sfmin rescaling paths (require inputs near underflow threshold)
- STOREV='R' / backward direction in dlarfb/dlarft
- Iteration-limit-exceeded branches

### Step 7: Lint base.js

```bash
bin/lint.sh lib/<package>/base/<routine>/lib/base.js
```

Fix all **easy/mechanical** lint errors immediately:

| Rule | Fix |
|------|-----|
| `no-plusplus` | `x++` → `x += 1` |
| `no-unused-vars` | Delete unused `var` declarations and unused requires |
| `stdlib/vars-order` | Reorder `var` declarations by name length (longest first) |
| `stdlib/require-globals` | Add `var Float64Array = require( '@stdlib/array/float64' );` etc. |
| `indent` | Fix tab indentation to match surrounding code |
| `no-lonely-if` | `else { if (...) }` → `else if (...)` |
| `no-negated-condition` | Swap if/else branches |
| `operator-assignment` | `x = x + y` → `x += y` |
| `stdlib/empty-line-before-comment` | Add blank line before comment blocks |
| `no-restricted-syntax` (toUpperCase) | Use `require( '@stdlib/string/base/uppercase' )` |
| `no-restricted-syntax` (labels) | Replace labeled statements with boolean flags |
| JSDoc rules | Fix formatting per stdlib conventions (see below) |

**JSDoc conventions (stdlib):**
- Math formulas use backticks: `` `C = α*op(A)*op(B) + β*C` `` (Unicode Greek letters)
- Emphasis uses `_underscores_`, not `*asterisks*`
- List items: blank `*` line between multi-line items; tight spacing for single-line items
- Bracket notation in text: wrap in backticks (`` `A[i,j]` ``) to avoid markdown link parsing
- All functions need `@private` annotation and `@param`/`@returns` tags
- Fenced code blocks need a language flag (e.g., ` ```text `)
- Description must start uppercase and end with period

**Defer these rules** (address in bulk later via `eslint-disable` or codemods):
- `function-call-argument-newline`, `function-paren-newline` — requires reformatting all multi-arg calls
- `@cspell/spellchecker` — needs BLAS/LAPACK dictionary
- `new-cap` — `Complex128Array()` etc.
- `camelcase` — Fortran-style names (sa1, sa2)
- `max-depth`, `max-len`, `max-statements`, `max-lines-per-function`, `max-lines`, `max-params` — add `/* eslint-disable */` at file top

For deferred rules, add a single eslint-disable comment at the top of the file listing
only the rules that actually fire. Example:

```javascript
/* eslint-disable max-len, max-params, max-depth, max-statements */
```

**Gate:** Zero errors from the "easy" rules above.

### Step 8: Write LEARNINGS.md (MANDATORY — DO NOT SKIP)

**This step is NOT optional.** Every translation MUST produce a `LEARNINGS.md`
file in the module directory (`lib/<package>/base/<routine>/LEARNINGS.md`).
A translation is not complete without it.

**Verification:** `grep -c "TODO: Fill in" lib/<package>/base/<routine>/LEARNINGS.md`
must return `0`. Replace every template placeholder with a real finding or `N/A`.
Write at least one concrete bullet per section.

This file captures anything that would help translate the NEXT routine faster.
Include:

- **Translation pitfalls**: Index off-by-ones, stride convention confusion,
  places where the Fortran semantics were non-obvious.
- **Dependency interface surprises**: If a dependency's calling convention
  was unexpected, document it here AND add it to
  [docs/dependency-conventions.md](docs/dependency-conventions.md).
- **Missing automation**: If you performed a mechanical step that should
  be automated, note it here (the automation-first rules apply).
- **Coverage gaps**: If certain code paths were hard to test and why.
- **Complex number handling**: Any subtleties in how complex arithmetic
  was handled (e.g., "inlined cmplx.mul to avoid allocation in hot loop").

Keep it concise — bullet points, not prose. This file is read by future
sessions to avoid repeating mistakes.

### Step 9: Verify full suite

```bash
npm test
```

**Gate:** All tests pass, no regressions.

---

## Common Translation Pitfalls (from 40+ translated routines)

These are hard-won lessons extracted from LEARNINGS.md across all completed
translations. Read before starting any new routine.

### Fortran Idioms → JavaScript

| Fortran | JavaScript | Gotcha |
|---------|-----------|--------|
| `SIGN(A, B)` | `Math.abs(a) * (Math.sign(b) \|\| 1.0)` | `Math.sign(0)` returns 0, not +1. Fortran's `SIGN(A,0)` returns `+|A|`. The `\|\| 1.0` fallback is **critical** — omitting it caused a real NaN bug in zlarfg. |
| `DISNAN(X)` | `x !== x` (or `Number.isNaN(x)`) | Self-comparison NaN test |
| `DLAMCH('S')` | `Number.MIN_VALUE` or stdlib constant | Replace DLAMCH constants with direct numeric literals |
| `DLAMCH('E')` | `Number.EPSILON / 2` | Machine epsilon (half-precision) |
| `ILAENV(1, ...)` | `NB = 32` (hardcoded) | Remove ILAENV/LWORK workspace queries entirely — allocate internally |

### Index Convention Landmines

- **IPIV arrays are 0-based in base.js.** Fortran IPIV is 1-based. If
  comparing against fixtures, subtract 1 from Fortran values. The ndarray.js
  wrapper handles conversion from 1-based Fortran convention.
- **INFO return values remain 1-based** (matching Fortran): 0 = success,
  k > 0 = algorithmic outcome at position k. In implementation, this means
  `return j + 1` when the 0-based loop variable finds the problem.
- **Recursive/blocked INFO offset:** When a recursive or blocked call on a
  submatrix starting at column j fails, offset the returned info:
  `if (iinfo > 0) return iinfo + j`.
- **idamax, iladlc, iladlr** return 0-based indices in JS vs 1-based in
  Fortran. Tests must subtract 1 from fixture values.

### Quick-Return Conditions Are Subtle

Quick returns are more complex than simple dimension checks:
```javascript
// WRONG: if (M === 0 || N === 0) return 0;
// RIGHT (dgemm): also check compound conditions
if (M === 0 || N === 0 || ((alpha === 0.0 || K === 0) && beta === 1.0)) return 0;
```
When `beta !== 1`, you **must still scale C** even when `alpha === 0` or
`K === 0`. This pattern applies to dgemm, dsyrk, and similar routines.

### Preserve Fortran Operation Order

Do NOT let linters or formatters reorder statements. In dtrmm, a linter
moved the diagonal multiply before the off-diagonal loop, causing incorrect
results. The Fortran operation order is load-bearing — preserve it exactly.

### Workspace Aliasing Bugs

TAU and WORK must NEVER alias. If TAU occupies `WORK[0..K*2-1]` and
subsequent operations (zunmqr, zungqr) use WORK as scratch, they clobber
TAU. Fix: always allocate separate arrays. Same for ztgevc's RWORK vs
LSCALE/RSCALE data.

### Dependency Calling Convention Surprises

Before calling a dependency for the first time, check
[docs/dependency-conventions.md](docs/dependency-conventions.md) for known
gotchas (e.g., dlarfg takes alpha as array+offset, not scalar; dlarf vs
zlarf differ in tau parameter type; zlarfb vs zgeqr2 differ in WORK strides).

---

## stdlib-js API Conventions Reference

### Array Parameters: stride + offset

Every array parameter uses `(array, stride, offset)`:
- Vectors: `(x, strideX, offsetX)`
- Matrices: `(A, strideA1, strideA2, offsetA)`

### Naming Rules

| Fortran | JS (base.js) | Rule |
|---------|-------------|------|
| `N, M, K` | `N, M, K` | Primary dimensions uppercase |
| `k1, k2` | `k1, k2` | Secondary dimensions lowercase |
| `DA` | `alpha` | Conventional scalar renames |
| `DX(*)` + `INCX` | `x, strideX, offsetX` | 1D array: strip D prefix, INC→stride, add offset |
| `A(LDA,*)` + `LDA` | `A, strideA1, strideA2, offsetA` | 2D array: LDA consumed into strides |
| `DL(*)` | `DL, strideDL, offsetDL` | Multi-char array names stay uppercase |
| `IPIV(*)` | `IPIV, strideIPIV, offsetIPIV` | Integer arrays stay uppercase |
| `INFO` | return value | Removed from params, returned as integer |

Use `python bin/signature.py <file.f>` to generate the correct signature.

### Return Values

- BLAS subroutines: `return y` (return the output array)
- BLAS functions (ddot, dasum): `return <scalar>`
- LAPACK with INFO: `return info` (0=success, >0=algorithmic outcome)

### Module Files

| File | Role |
|------|------|
| `base.js` | Core algorithm. No validation. stride/offset API. CommonJS. |
| `ndarray.js` | Validation wrapper → calls base.js. Throws TypeError/RangeError. |
| `index.js` | Entry point. Tries native, falls back to JS. Exports `.ndarray`. |
| `main.js` | Attaches `.ndarray` property to the BLAS-style entry point. |
| `<routine>.js` | BLAS-style API wrapper (layout, no strides/offsets). |

For initial translation, only `base.js` and `test/test.js` are required.
The wrapper files (`ndarray.js`, `index.js`, `main.js`, `<routine>.js`) can be
added later following the stdlib-js patterns in the reference clone.

### Element Access Formula

`A(i, j)` in Fortran (1-based) → `A[offsetA + (i-1)*strideA1 + (j-1)*strideA2]`

With 0-based loop variables: `A[offsetA + i*strideA1 + j*strideA2]`

For column-major: `strideA1 = 1, strideA2 = LDA`
For row-major: `strideA1 = N, strideA2 = 1`

### Performance Patterns

Use incremental pointers (one add per element, not two multiplies). Drop
Fortran stride-1 specializations. Preserve zero-element guards. For `uplo`
routines, use layout-aware stride remapping. See
[docs/performance-patterns.md](docs/performance-patterns.md) for code examples.

**Proven optimizations (from zggev profiling):**

1. **Hoist invariant constants to module scope.** `dlamch()` calls, `Math.sqrt()`
   of constants, and any expression that depends only on machine constants must
   be computed once at module level, not per call. Example: `dladiv` called
   `dlamch('O')` 3× per call × 200K calls = 600K wasted function calls at
   N=200. Fix: move to module-level `var OV = dlamch('O');`.

2. **Cache typed-array element reads into local variables.** In rotation inner
   loops, read all 4 values (re/im of both elements) into locals before
   computing. This eliminates aliasing concerns for V8 and reduces bounds
   checks. Combined with incremental pointers, this gave **16% speedup** on
   zhgeqz's QZ sweep (the single hottest function at 53% of zggev time).

3. **Avoid redundant `reinterpret()` calls.** When `cx === cy` (same
   Complex128Array with different offsets — common in LAPACK rotation calls),
   skip the second `reinterpret()`. Measured 11-34% improvement on zrot at
   workload-relevant sizes.

4. **Function call overhead matters at high call counts.** Scalar JS BLAS
   peaks at ~5-6 GFLOPS for complex operations. At N=5, zrot spends more
   time entering/exiting the function than doing useful work. For routines
   called >100K times (zlartg, zrot, dladiv), every nanosecond of per-call
   overhead is amplified. Consider inlining critical inner rotations as
   zhgeqz does in doQZSweep.

5. **Size WORK buffers correctly and carve sub-allocations from them.**
   Driver routines (zggev, zgesvd) must allocate WORK large enough for
   all subroutines. Blocked routines (zgeqrf, zunmqr) need `N*NB+NB*NB`
   or more — check the actual formulas. If WORK is undersized, the callee
   silently allocates its own, wasting the caller's buffer. Similarly,
   carve the block reflector `T` from the tail of WORK instead of
   allocating a separate Complex128Array per call. This eliminated ~35%
   of per-call heap allocation in zggev (1.4MB → 0.9MB at N=100).

**Profiling workflow:** Use `bin/instrument.js` (Module._load hook) for
subroutine-level profiling, then `bin/profile-zhgeqz.js` (V8 inspector API)
for per-line breakdown. Run `bin/bench-blas.js` for leaf-node throughput.

### uplo/trans String Convention

**In `base.js`, use lowercase long-form strings:**
`'upper'`, `'lower'`, `'left'`, `'right'`, `'no-transpose'`, `'transpose'`,
`'conjugate-transpose'`, `'unit'`, `'non-unit'`, `'forward'`, `'backward'`,
`'columnwise'`, `'rowwise'`, `'frobenius'`, `'one-norm'`, `'inf-norm'`, `'max'`.

**NEVER use single-char Fortran strings** (`'U'`, `'L'`, `'N'`, `'T'`, `'C'`)
in `base.js` code. The `ndarray.js` wrapper normalizes from stdlib's public API
strings, and `base.js` functions use the long-form internally.

**When calling a dependency from `base.js`**, always pass long-form strings.
This is the #1 cause of silent bugs — a short-form string like `'L'` won't
match `=== 'left'` and will silently take the wrong branch.

For `job`-style parameters with many values (`'N'`, `'P'`, `'S'`, `'B'`),
single chars are acceptable since there are no long-form equivalents.

### Complex Number Support

For z-prefix (complex) routines, see [docs/complex-numbers.md](docs/complex-numbers.md)
for the full reference (reinterpret pattern, arithmetic APIs, test patterns).

Key rules (always in effect):
- API boundary uses `Complex128Array` (strides/offsets in complex elements)
  and `Complex128` scalars. Inside routines, `reinterpret(x, 0)` for Float64
  views, multiply strides/offsets by 2 for Float64 indexing.
- **NEVER** inline complex division or absolute value — use `cmplx.div`/`cmplx.abs`.
- **Stride convention mismatch** — Two conventions coexist and mixing them
  causes silent corruption. If a routine's source does `sa1 = strideA1 * 2`,
  it expects complex-element strides. If it directly indexes
  `offset + i*strideA1`, it expects double-based strides. See the doc for
  which routines use which convention.

---

## Known Limitations

- `translate-to-js` transform cannot parse fparser's free-form output for
  some routines. Work around: translate from original `.f` source, or from
  manually written `.f90`.
- Fortran `.f90` files that use modules (`USE la_constants`) cannot be
  compiled by `run_fortran.sh` without module compilation ordering.
  Work around: write JS tests with hand-computed expected values.
