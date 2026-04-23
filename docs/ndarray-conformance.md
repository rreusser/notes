# ndarray.js Conformance Criteria

Every `ndarray.js` must satisfy the following requirements, matching stdlib-js
conventions.

## 1. License header

Apache-2.0 license block at top, matching stdlib format.

## 2. Validation of string parameters

Each string parameter must be validated using the appropriate stdlib assertion
helper. Throw `TypeError` with `format()` on invalid values.

| Parameter | Validator | Valid values |
|-----------|-----------|-------------|
| `trans`, `transa`, `transb` | `isMatrixTranspose` | `'no-transpose'`, `'transpose'`, `'conjugate-transpose'` |
| `uplo` | `isMatrixTriangle` | `'upper'`, `'lower'` |
| `side` | `isOperationSide` | `'left'`, `'right'` |
| `diag` | `isDiagonalType` | `'unit'`, `'non-unit'` |

For parameters without a stdlib helper (`direct`, `storev`, `job`, `vect`,
`norm`, `compz`, `jobu`, `jobvt`, etc.), validate manually with a whitelist
check.

Error format:
```javascript
if ( !isMatrixTranspose( trans ) ) {
    throw new TypeError( format(
        'invalid argument. First argument must be a valid transpose operation. Value: `%s`.', trans
    ) );
}
```

## 3. Validation of dimensions

All dimension parameters (`M`, `N`, `K`, etc.) must be checked `>= 0`:
```javascript
if ( M < 0 ) {
    throw new RangeError( format(
        'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', M
    ) );
}
```

## 4. Validation of strides

Vector strides (`strideX`, `strideY`) must be non-zero:
```javascript
if ( strideX === 0 ) {
    throw new RangeError( format(
        'invalid argument. Tenth argument must be non-zero. Value: `%d`.', strideX
    ) );
}
```

Matrix strides (`strideA1`, `strideA2`) are generally not checked (zero
strides are valid for broadcasting in some contexts).

## 5. Early return

Quick-return conditions should be in ndarray.js, not just base.js:
```javascript
if ( M === 0 || N === 0 || ( alpha === 0.0 && beta === 1.0 ) ) {
    return y;
}
```

## 6. JSDoc

- Same signature and `@param` types as base.js
- Add `@throws` tags for each validation check
- Add `@returns` tag
- Add `@example` block with a minimal runnable example
- NOT marked `@private` (ndarray.js is the public API)

## 7. Test requirements

**Tests must be written BEFORE the implementation.** For each module:

1. Add a test that calls `ndarray.js` with an invalid string param and asserts
   `TypeError` is thrown
2. Add a test that calls with negative dimensions and asserts `RangeError`
3. Add a test that calls with zero strides and asserts `RangeError`
4. Add a test for early-return conditions (N=0, etc.)
5. Verify these tests FAIL against the current passthrough ndarray.js
6. Then implement the validation to make them pass

## 8. Module requires

```javascript
var isMatrixTranspose = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isDiagonalType = require( '@stdlib/blas/base/assert/is-diagonal-type' );
var format = require( '@stdlib/string/format' );
```
