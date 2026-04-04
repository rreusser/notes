# dlantp

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real triangular matrix supplied in packed form.

<section class="usage">

## Usage

```javascript
var dlantp = require( '@stdlib/lapack/base/dlantp' );
```

#### dlantp( norm, uplo, diag, N, AP, WORK )

Returns the norm of a real triangular matrix in packed storage.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 3x3 upper triangular matrix:
//   A = [ 2.0  3.0  -1.0 ]
//       [ 0.0  5.0   2.0 ]
//       [ 0.0  0.0   7.0 ]
var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );

var result = dlantp( 'max', 'upper', 'non-unit', 3, AP, WORK );
// returns 7.0
```

The function has the following parameters:

-   **norm**: specifies which norm to compute: `'max'` (max abs value), `'one-norm'` (one-norm), `'inf-norm'` (infinity-norm), or `'frobenius'` (Frobenius norm).
-   **uplo**: specifies whether the upper or lower triangle is stored: `'upper'` or `'lower'`.
-   **diag**: specifies whether the diagonal is unit: `'unit'` or `'non-unit'`.
-   **N**: order of the matrix `A`.
-   **AP**: packed triangular matrix, length >= `N*(N+1)/2`.
-   **WORK**: workspace array, length >= `N` (used for `'inf-norm'`).

#### dlantp.ndarray( norm, uplo, diag, N, AP, strideAP, offsetAP, WORK, strideWORK, offsetWORK )

Returns the norm of a real triangular matrix in packed storage using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );

var result = dlantp.ndarray( 'max', 'upper', 'non-unit', 3, AP, 1, 0, WORK, 1, 0 );
// returns 7.0
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   When `diag` is `'unit'`, the diagonal elements of `AP` are not referenced; they are assumed to be one.
-   The `WORK` array is only used when computing the infinity norm (`'inf-norm'`).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlantp = require( '@stdlib/lapack/base/dlantp' );

// 3x3 upper triangular matrix (non-unit):
var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
var WORK = new Float64Array( 3 );

var maxNorm = dlantp( 'max', 'upper', 'non-unit', 3, AP, WORK );
// returns 7.0

var oneNorm = dlantp( 'one-norm', 'upper', 'non-unit', 3, AP, WORK );
// returns 10.0

var unitMax = dlantp( 'max', 'upper', 'unit', 3, AP, WORK );
// returns 3.0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
