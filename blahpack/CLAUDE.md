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
- Use `require()` for BLAS/LAPACK dependencies
- Use `lib/cmplx.js` for complex arithmetic
- Follow CommonJS format with stdlib-js section comments
- NOT include parameter validation (that goes in ndarray.js)

**Comment inlined operations.** When complex arithmetic or index calculations
are inlined for performance, add a comment showing the mathematical operation:
```javascript
// temp += conj(A[i,j]) * x[ix]
tr += aijR * xr + aijI * xi;   // real part (note: aijI sign flipped for conj)
ti += aijR * xi - aijI * xr;   // imag part

// y[iy] += alpha * temp
yr = A[ iy ];
yi = A[ iy + 1 ];
A[ iy ]     = yr + alphaR * tr - alphaI * ti;
A[ iy + 1 ] = yi + alphaR * ti + alphaI * tr;
```
This is especially important for complex multiply-add and conjugate multiply
— operations where the expanded form obscures the intent. Without these
comments, the code becomes very difficult to verify.

**NEVER inline complex division, absolute value, or square root.** Naive
implementations of these are numerically unstable (overflow, underflow,
catastrophic cancellation). Always use the library functions:
```javascript
cmplx.div( out, a, b );    // DO NOT expand to (ar*br+ai*bi)/(br*br+bi*bi) etc.
cmplx.abs( a );             // DO NOT expand to Math.sqrt(ar*ar + ai*ai)
Math.sqrt( cmplx.abs(a) );  // safe chained calls are fine
```
`cmplx.div` uses Smith's formula; `cmplx.abs` uses the `max/min` scaling
trick. Inlining these loses the numerical safety guarantees. Complex
addition, subtraction, multiplication, conjugate, and real-scalar scaling
are safe to inline.

For routines with GOTOs, restructure them during translation:

| Pattern | Fortran | JavaScript |
|---------|---------|-----------|
| Loop continue | `IF (cond) GO TO 40` inside DO 40 | `if (cond) continue` |
| Loop break | `GO TO 30` breaks loop, `30: INFO=J` | `info = j+1; return info` |
| While-loop | `10: ... IF (cond) GO TO 10` | `do { ... } while (cond)` |
| Skip-ahead | `IF (cond) GO TO 30` (30 below) | `if (!cond) { ... }` |

### Step 5: Fill in JS tests and verify

Fill in the test stubs generated in Step 3 with actual input values
matching the Fortran test. Run:

```bash
node --test lib/<package>/base/<routine>/test/test.js
```

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

### Step 7: Write LEARNINGS.md

**REQUIRED.** After completing every translation, write a `LEARNINGS.md` file
in the module directory (`lib/<package>/base/<routine>/LEARNINGS.md`).

This file captures anything that would help translate the NEXT routine faster.
Include:

- **Translation pitfalls**: Index off-by-ones, stride convention confusion,
  places where the Fortran semantics were non-obvious.
- **Dependency interface surprises**: If a dependency's calling convention
  was unexpected (e.g., "zlarf takes tau as array+offset, not a 2-element
  array"), document it so the next routine that calls it doesn't hit the
  same issue.
- **Missing automation**: If you performed a mechanical step that should
  be automated, note it here (the automation-first rules apply).
- **Coverage gaps**: If certain code paths were hard to test and why.
- **Complex number handling**: Any subtleties in how complex arithmetic
  was handled (e.g., "inlined cmplx.mul to avoid allocation in hot loop").

Keep it concise — bullet points, not prose. This file is read by future
sessions to avoid repeating mistakes.

### Step 8: Verify full suite

```bash
npm test
```

**Gate:** All tests pass, no regressions.

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

### Performance Patterns (from stdlib comparison)

**1. Incremental matrix pointer** — avoid recomputing offsets in inner loops:
```javascript
// GOOD: incremental (one add per element)
var ia = offsetA + sa1 * j;
for ( i = 0; i < M; i++ ) {
    A[ ia ] = ...;
    ia += sa0;
}

// AVOID: full recompute (two multiplies per element)
for ( i = 0; i < M; i++ ) {
    A[ offsetA + i*sa1 + j*sa2 ] = ...;
}
```

**2. Layout-aware triangle dispatch** — for routines with `uplo` parameter,
stdlib remaps strides so the inner loop always walks contiguous memory:
```javascript
var isrm = isRowMajor( [ strideA1, strideA2 ] );
// (col-major, upper) ↔ (row-major, lower) are same physical pattern
var sa0 = isrm ? strideA2 : strideA1;  // fast (inner) stride
var sa1 = isrm ? strideA1 : strideA2;  // slow (outer) stride
```

**3. Drop Fortran stride-1 specialization** — the general stride/offset loop
handles unit stride fine. Don't port the `IF (INCX.EQ.1)` branches.

**4. Preserve zero-element guards** — `if (x[jx] !== 0.0)` before column
updates is a meaningful optimization (skips entire column when x is zero).

### uplo/trans String Convention

In `base.js`, use single chars matching Fortran: `'U'`, `'L'`, `'N'`, `'T'`, `'C'`.
The `ndarray.js` wrapper normalizes from stdlib's full strings (`'upper'`,
`'lower'`, `'no-transpose'`, etc.).

### Complex Number Support

Complex numbers use `lib/cmplx.js` — a gl-matrix-style library where each
complex number is a 2-element `Float64Array` slice `[real, imag]`. Functions
never allocate; the caller provides `out`:

```javascript
var cmplx = require( '../../cmplx.js' );
var z = new Float64Array( 2 );
cmplx.mul( z, a, b );          // z = a * b
cmplx.mmadd( z, z, a, b );     // z = z + a * b
cmplx.conj( z, a );            // z = conj(a)
var r = cmplx.abs( a );        // r = |a| (returns scalar)
```

**Complex arrays** are interleaved: `[re0, im0, re1, im1, ...]`.

**CRITICAL convention for complex arrays:**
- **stride** = in complex elements (the routine multiplies by 2 internally)
- **offset** = Float64 index (direct position into the Float64Array)

This means: to start at the 3rd complex element (0-based index 2), pass
`offset = 4` (not 2), because each complex element occupies 2 Float64 slots.

Element k of vector zx:
- real: `zx[ offsetX + 2 * k * strideX ]`
- imag: `zx[ offsetX + 2 * k * strideX + 1 ]`

Element (i,j) of matrix A:
- real: `A[ offsetA + 2 * (i * strideA1 + j * strideA2) ]`
- imag: `A[ offsetA + 2 * (i * strideA1 + j * strideA2) + 1 ]`

Inside routines, the `2×` conversion happens at the top:
```javascript
var sx = 2 * strideX;  // convert complex stride to Float64 stride
var ix = offsetX;      // already a Float64 index
```

**Fortran complex test pattern** (use EQUIVALENCE to print interleaved):
```fortran
double precision :: zx_r(20)
complex*16 :: zx(10)
equivalence (zx, zx_r)
call print_array('zx', zx_r, 2*n)
```

**Translator directives**: The translator emits `@complex` comments:
```javascript
/* @complex ztemp */           // scalar complex variables
/* @complex-arrays zx, zy */   // array complex variables
```

---

## Composable Code-Mod Pipeline

For routines with GOTOs that benefit from incremental Fortran restructuring:

```bash
python bin/transform.py --list                              # List transforms
python bin/transform.py pipeline/<r> --init <pkg> <r>       # Initialize
python bin/transform.py pipeline/<r> --apply <transform>    # Apply transform
python bin/transform.py pipeline/<r> --verify <step>        # Verify step
```

Available transforms: `remove-trivial-goto`, `remove-trivial-if-goto`,
`free-form`, `translate-to-js`.

## Known Limitations

- `translate-to-js` transform cannot parse fparser's free-form output for
  some routines. Work around: translate from original `.f` source, or from
  manually written `.f90`.
- Fortran `.f90` files that use modules (`USE la_constants`) cannot be
  compiled by `run_fortran.sh` without module compilation ordering.
  Work around: write JS tests with hand-computed expected values.
