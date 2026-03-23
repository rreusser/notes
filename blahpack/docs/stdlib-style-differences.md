# Stylistic Differences: stdlib-js vs blahpack

Based on comparison of 20+ overlapping BLAS/LAPACK implementations.

## 1. String parameters: full words vs single chars

**stdlib** uses full descriptive strings everywhere, including in `base.js`:
```javascript
// stdlib base.js
if ( uplo === 'upper' ) { ... }
if ( trans === 'no-transpose' ) { ... }
// Called as: dgemv('no-transpose', ...) or dlacpy('upper', ...)
```

**blahpack** uses single Fortran-style chars in `base.js`:
```javascript
// blahpack base.js
if ( uplo === 'U' || uplo === 'u' ) { ... }
if ( trans === 'N' || trans === 'n' || trans === 'no-transpose' ) { ... }
// Called as: dgemv('N', ...) or dlacpy('U', ...)
```

**stdlib convention:** `'no-transpose'`, `'transpose'`, `'conjugate-transpose'`,
`'upper'`, `'lower'`, `'all'`, `'left'`, `'right'`, `'unit'`, `'non-unit'`,
`'row-major'`, `'column-major'`.

## 2. Module architecture (4-file pattern)

Both projects use the same file structure, but the roles differ slightly:

| File | stdlib | blahpack |
|------|--------|----------|
| `base.js` | Private core. Full-word string params. No validation. `@private` | Private core. Single-char params. No validation. `@private` |
| `ndarray.js` | Public API. Validates params, calls `base.js`. Has `@example`. | Validation wrapper or passthrough to `base.js`. |
| `<routine>.js` | BLAS-style API (layout + LDA, no strides/offsets). Validates, computes strides, calls `base.js`. | Stub or similar. |
| `main.js` | Attaches `.ndarray` via `setReadOnly()`. | Attaches `.ndarray` directly. |
| `index.js` | tryRequire for native, falls back to main. Has `@module` JSDoc. | tryRequire for native, falls back to main. |

**Key difference:** stdlib's `<routine>.js` is a full BLAS-style API with
`order` (layout) and `LDA` parameters, computing strides/offsets internally.
This is the "classic BLAS" API. blahpack's `<routine>.js` is usually a stub.

## 3. Variable naming in base.js

**stdlib** uses numbered loop variables (`i0`/`i1` = innermost/outermost) and
stride increment variables (`da0`/`da1`, `db0`/`db1`):
```javascript
// stdlib — loop variables numbered from inner (0) to outer (1)
for ( i1 = 0; i1 < S1; i1++ ) {
    for ( i0 = 0; i0 < S0; i0++ ) {
        B[ ib ] = A[ ia ];
        ia += da0;
        ib += db0;
    }
    ia += da1;
    ib += db1;
}
```

**blahpack** uses Fortran-style `i`/`j` and descriptive stride aliases
(`sa1`/`sa2` or `strideA1`/`strideA2`):
```javascript
// blahpack — Fortran-style loop variables
for ( j = 0; j < N; j++ ) {
    ia = offsetA + j * sa2;
    for ( i = 0; i < M; i++ ) {
        y[ iy ] += temp * A[ ia ];
        ia += sa1;
    }
}
```

## 4. Layout-aware loop ordering

**stdlib** uses `isRowMajor()` checks and `loopOrder()` for cache-optimal
loop interchange:
```javascript
var isRowMajor = require( '@stdlib/ndarray/base/assert/is-row-major' );
var loopOrder = require( '@stdlib/ndarray/base/unary-loop-interchange-order' );

var o = loopOrder( [ M, N ], [ strideA1, strideA2 ], [ strideB1, strideB2 ] );
// Automatically determines optimal iteration order for cache locality
```

**blahpack** uses direct stride indexing without explicit layout dispatch
(the stride/offset API already handles both layouts implicitly).

## 5. Beta-scaling pattern

**stdlib** delegates to library functions:
```javascript
var dfill = require( '@stdlib/blas/ext/base/dfill' ).ndarray;
var dscal = require( '@stdlib/blas/base/dscal' ).ndarray;

if ( beta === 0.0 ) {
    dfill( ylen, 0.0, y, strideY, offsetY );
} else if ( beta !== 1.0 ) {
    dscal( ylen, beta, y, strideY, offsetY );
}
```

**blahpack** uses inline loops:
```javascript
if ( beta === 0.0 ) {
    for ( i = 0; i < leny; i++ ) { y[iy] = 0.0; iy += strideY; }
} else if ( beta !== 1.0 ) {
    for ( i = 0; i < leny; i++ ) { y[iy] = beta * y[iy]; iy += strideY; }
}
```

## 6. Validation style

**stdlib** ndarray.js uses assertion helpers and formatted error messages:
```javascript
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var format = require( '@stdlib/string/format' );

if ( !isMatrixTranspose( trans ) ) {
    throw new TypeError( format( 'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', trans ) );
}
```

**blahpack** ndarray.js has minimal or no validation in many modules.

## 7. JSDoc conventions

**stdlib** includes `@example` blocks with runnable code in both ndarray.js
and base.js. base.js is marked `@private`.

**blahpack** includes `@param` tags but often omits `@example` blocks.
base.js is marked `@private`.

## 8. Unrolled loops

Both projects keep stride-1 unrolled loops for Level 1 BLAS (daxpy, dcopy,
ddot, dscal). This is a direct port from the Fortran reference and both
preserve it.

## 9. Zero-element guard

Both preserve `if (tmp === 0.0)` guards before column updates as a
performance optimization.

## 10. main.js attachment

**stdlib** uses `setReadOnly()`:
```javascript
var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
setReadOnly( dgemv, 'ndarray', ndarray );
```

**blahpack** uses direct assignment:
```javascript
dgemv.ndarray = ndarray;
```

## 11. Outer loop reset stride

**stdlib** precomputes the "reset" stride for the outer loop:
```javascript
da1 = strideA2 - ( S0 * strideA1 );  // resets pointer after inner loop
```

**blahpack** recomputes the column/row offset each iteration:
```javascript
ia = offsetA + j * sa2;  // explicit offset per outer iteration
```

## 12. Function decomposition

**stdlib** splits base.js into multiple private helper functions per branch:
```javascript
// stdlib dlacpy/lib/base.js — 460 lines, 3 functions
function copyAll( M, N, A, ... ) { ... }
function copyUpper( M, N, A, ... ) { ... }
function copyLower( M, N, A, ... ) { ... }
function dlacpy( uplo, M, N, A, ... ) {
    if ( uplo === 'upper' ) return copyUpper(...);
    ...
}
```

**blahpack** uses one function with if/else branching (~78 lines for dlacpy).

## 13. Dependencies

**stdlib** uses `@stdlib/` package paths and utility wrappers:
```javascript
var isRowMajor = require( '@stdlib/ndarray/base/assert/is-row-major' );
var min = require( '@stdlib/math/base/special/fast/min' );
var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
```

**blahpack** uses relative paths and native JS APIs:
```javascript
var dlamch = require( '../../dlamch/lib/base.js' );
// Math.min, Math.abs, Object.defineProperty — no @stdlib wrappers
```

## 14. eslint directives

**stdlib**: File-level `/* eslint-disable max-len, max-params */` at top.
**blahpack**: Inline `// eslint-disable-line max-len, max-params` on declarations.

---

## Summary: What would need to change

To align blahpack with stdlib conventions:

1. **String params in base.js**: Change from `'U'`/`'L'`/`'N'`/`'T'`/`'C'`
   to `'upper'`/`'lower'`/`'no-transpose'`/`'transpose'`/`'conjugate-transpose'`
2. **Variable naming**: Consider `i0`/`i1`/`da0`/`da1` convention (optional — both are readable)
3. **main.js**: Use `setReadOnly()` for `.ndarray` attachment
4. **ndarray.js**: Add full validation with `isMatrixTranspose()`, `format()`
5. **<routine>.js**: Implement the BLAS-style API (layout + LDA)
6. **JSDoc**: Add `@example` blocks to ndarray.js and base.js
7. **Beta-scaling**: Consider using `dfill`/`dscal` (optional)
8. **Function decomposition**: Split large base.js into per-branch helpers
9. **Layout-aware loops**: Add `isRowMajor()` checks for cache-optimal iteration
