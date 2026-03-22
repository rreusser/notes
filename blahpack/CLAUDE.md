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

**4. Ruthlessly question the boundary between automated and manual steps.**

The current Step 4 (translate to JS) has 5 sub-steps (4a–4e) that are
described as "manual." Challenge each one:

- Can 4a (remove INFO, remove XERBLA block) be automated? Probably yes.
- Can 4b (naming: lda→strideA2, max→Math.max) be automated? Likely yes.
- Can 4c (1-based → 0-based loop conversion) be automated? For simple loops, yes.
- Can 4d (BLAS calls → require + offset) be automated? Partially.
- Can 4e (CommonJS boilerplate) be automated? Absolutely yes.

Each time you successfully automate one of these, update Step 4 to reflect
that it is now handled by a transform, and remove it from the manual checklist.

**5. The end state is: the human types one command, reviews the output, and
approves or requests changes.** Every manual step is a waypoint, not a destination.

### Examples of transforms that should exist but don't yet

These are patterns that have been identified as mechanical. Implement them
as transforms in `bin/transform.py` when the time comes:

- `commonjs-boilerplate` — Wrap JS output in `'use strict'; ... module.exports = fn;`
- `reindex-loops` — Convert `for (i = 1; i <= n` → `for (i = 0; i < N`
- `remove-xerbla-block` — Strip the parameter validation + XERBLA call block
- `rename-intrinsics` — `max(` → `Math.max(`, `sqrt(` → `Math.sqrt(`
- `add-require-imports` — Detect bare BLAS/LAPACK calls → add require() statements
- `generate-test-scaffold` — From fixture JSONL + signature, generate test/test.js
- `generate-package-json` — From routine name + package, generate package.json

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

### Step 1: Write the Fortran test

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

**For LAPACK routines:** also create `test/fortran/deps_<routine>.txt` listing
LAPACK source files needed for compilation (without .f extension, one per line):
```
<routine>
<dependency1>
<dependency2>
```

**Gate:** `./test/run_fortran.sh <package> <routine>` compiles and runs successfully.

### Step 2: Generate the reference fixture

```bash
./test/run_fortran.sh blas <routine>
# or
./test/run_fortran.sh lapack <routine>
```

This writes `test/fixtures/<routine>.jsonl` — the ground truth for all JS tests.

**Gate:** Fixture file exists and contains one JSON line per test case.

### Step 3: Fortran restructuring (LAPACK only, skip for BLAS)

BLAS routines have no GOTOs — skip to Step 4.

For LAPACK routines with GOTOs:

```bash
# Initialize pipeline
python bin/transform.py pipeline/<routine> --init lapack <routine>

# Apply automated transforms
python bin/transform.py pipeline/<routine> --apply remove-trivial-goto
python bin/transform.py pipeline/<routine> --apply free-form

# Verify each Fortran stage
python bin/transform.py pipeline/<routine> --verify <step_number>
```

Then manually restructure remaining GOTOs into the highest-numbered `.f90` file.
Apply these patterns:

| Pattern | Fortran | Restructured |
|---------|---------|-------------|
| Loop continue | `IF (cond) GO TO 40` inside DO 40 | `IF (cond) CYCLE` |
| Loop break + cleanup | `GO TO 30` breaks loop, `30: INFO=J` | Inline: `INFO=J; RETURN` |
| While-loop | `10: ... IF (cond) GO TO 10` | `DO WHILE (cond) ... END DO` |
| Skip-ahead | `IF (cond) GO TO 30` (30 is below) | `IF (.NOT. cond) THEN ... END IF` |

**Gate:** `python bin/transform.py pipeline/<routine> --verify <step>` → PASS

### Step 4: Translate to JavaScript

For BLAS (no GOTOs): translate directly from original source.
For LAPACK: translate from the restructured Fortran in the pipeline dir.

The automated translator (`fortran_to_estree.py | estree_to_js.js`) produces
raw 1-based JS. This raw output requires manual transformation to produce
the final `base.js`. The following changes are needed:

#### 4a: Structural changes
- Remove INFO from parameter list → return it instead
- Remove XERBLA calls → validation goes in ndarray.js, not base.js
- Remove parameter validation block (the IF/THEN/XERBLA/RETURN block)
- Remove LDA/LDB from params → they become strideA2/strideB2
- Match the signature from Step 0 exactly

#### 4b: Naming
- Dimension params uppercase: `n` → `N`, `m` → `M`
- Use stride/offset names from signature: `strideA1, strideA2, offsetA`
- Replace `lda` references with `strideA2` (or `sa2` local alias)
- Replace bare `lsame(x, 'U')` with `uplo === 'U' || uplo === 'u'`
- Replace `max(...)` with `Math.max(...)`
- Replace `sqrt(...)` with `Math.sqrt(...)`

#### 4c: Indexing (1-based → 0-based)
- Convert loops: `for (j = 1; j <= n; j++)` → `for (j = 0; j < N; j++)`
- Array subscripts already have `- 1` markers from the translator:
  `a[j - 1 + (i - 1) * lda]` → simplify to `A[offsetA + j*sa1 + i*sa2]`
  once j, i are 0-based
- Index variable inits: `ix = 1` → `ix = 0`
- Adjust BLAS call arguments: pass computed offsets instead of subarray refs

#### 4d: BLAS/LAPACK calls → require() with offset arithmetic
```javascript
// Fortran: CALL DDOT(J-1, A(1, J), 1, A(1, J), 1)
// JS:
var ddot = require('../../../../blas/base/ddot/lib/base.js');
ddot(j, A, strideA1, offsetA + j*strideA2, A, strideA1, offsetA + j*strideA2);
```

#### 4e: CommonJS boilerplate
```javascript
'use strict';
// require() imports at top
// ... implementation ...
module.exports = <routine>;
```

### Step 5: Scaffold the module

```bash
python bin/scaffold.py <package> <routine> -d "<one-line description>"
```

This generates the complete stdlib-js module structure:
- `package.json`, `lib/index.js`, `lib/main.js`, `lib/<routine>.js`, `lib/ndarray.js`
- `test/test.js`, `README.md`, `docs/`, `examples/`
- If `lib/base.js` already exists, it is NOT overwritten

The scaffold produces stubs with correct signatures (from `signature.py`).
Copy your translated `base.js` into `lib/base.js`.

Intermediate `package.json` files are needed at `lib/<package>/package.json`
and `lib/<package>/base/package.json` (empty `{}`) to override the root
`"type": "module"` for CommonJS compatibility.

### Step 6: Write the JS test

Generate the test scaffold, then fill in the test bodies:

```bash
python bin/gen_test.py <package> <routine> > lib/<package>/base/<routine>/test/test.js
```

This generates fixture loading, assertClose/assertArrayClose helpers, and
one test stub per fixture case. Fill in the input setup and assertion calls.

**Gate:** `node --test lib/<package>/base/<routine>/test/test.js` → all pass.

### Step 6b: Verify test coverage

```bash
node --test --experimental-test-coverage lib/<package>/base/<routine>/test/test.js
```

Check the coverage report for `base.js`. Target: **≥90% line coverage, ≥85% branch coverage**.

If coverage is low, identify the uncovered lines and add targeted test cases:

- **Uncovered branches in if/else chains**: Add test cases that exercise each branch.
  For routines with `uplo`, `trans`, `side`, `diag` parameters, test ALL valid
  combinations (U/L, N/T/C, L/R, U/N).
- **Uncovered quick-return paths**: Test edge cases (N=0, M=0, alpha=0, etc.).
- **Uncovered scaling/overflow paths**: Test with very large (1e300) and very small
  (1e-300) values to exercise safe-scaling code.
- **Uncovered stride paths**: Test non-unit strides and negative strides.

Do NOT add tests solely to hit coverage numbers. Every test should verify a
meaningful behavioral property. But low coverage usually indicates genuinely
untested code paths — branches that could contain bugs.

### Step 7: Update manifest and verify

```bash
# Update manifest.json
node -e "
var fs = require('fs');
var m = JSON.parse(fs.readFileSync('manifest.json','utf8'));
m.<package>.<routine> = {status:'complete', tests:N, passing:N};
fs.writeFileSync('manifest.json', JSON.stringify(m,null,2));
"

# Run full test suite
npm test
```

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
Stride is in complex elements. Element k of vector zx:
- real: `zx[ offsetX + 2 * k * strideX ]`
- imag: `zx[ offsetX + 2 * k * strideX + 1 ]`

**Complex matrices**: Element (i,j) of matrix A:
- real: `A[ offsetA + 2 * (i * strideA1 + j * strideA2) ]`
- imag: `A[ offsetA + 2 * (i * strideA1 + j * strideA2) + 1 ]`

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

```bash
python bin/transform.py --list                              # List transforms
python bin/transform.py pipeline/<r> --init <pkg> <r>       # Initialize
python bin/transform.py pipeline/<r> --apply <transform>    # Apply transform
python bin/transform.py pipeline/<r> --verify <step>        # Verify step
```

Each transform reads the highest-numbered file and writes the next:
`<routine>.00.f → <routine>.01.f90 → <routine>.02.f90 → <routine>.03.js`

Available transforms: `remove-trivial-goto`, `remove-trivial-if-goto`,
`free-form`, `translate-to-js`.

## Known Limitations

- `translate-to-js` transform cannot parse fparser's free-form output for
  some routines. Work around: translate from original `.f` source, or from
  manually written `.f90`.
- The automated translator's output requires significant manual work (Steps 4a-4e).
  This is by design — the translator handles syntax, the human/AI handles semantics.
- `ndarray.js`, `index.js`, `main.js` wrapper files are not yet generated
  automatically. Only `base.js` and `test/test.js` are produced per routine.
