# dtbcon

> Estimates the reciprocal condition number of a real triangular band matrix.

<section class="usage">

## Usage

```javascript
var dtbcon = require( '@stdlib/lapack/base/dtbcon' );
```

#### dtbcon( norm, uplo, diag, N, kd, AB, LDAB, rcond, WORK, IWORK )

Estimates the reciprocal condition number of a real triangular band matrix `A` stored in band format, in either the 1-norm or the infinity-norm.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 upper triangular band matrix with kd=1:
// [ 2  -1   0 ]
// [ 0   3  -1 ]
// [ 0   0   4 ]
// Band storage (2 rows x 3 cols, column-major):
var AB = new Float64Array( [ 0.0, 2.0, -1.0, 3.0, -1.0, 4.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtbcon( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 2, rcond, WORK, IWORK );
// info => 0
// rcond[ 0 ] contains the reciprocal condition number
```

The function has the following parameters:

-   **norm**: norm type (`'one-norm'` or `'inf-norm'`).
-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **diag**: specifies whether the diagonal is unit or non-unit (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix `A`.
-   **kd**: number of superdiagonals (upper) or subdiagonals (lower).
-   **AB**: triangular band matrix in band storage (column-major, `LDAB` rows by `N` columns).
-   **LDAB**: leading dimension of `AB` (must be at least `kd+1`).
-   **rcond**: output [`Float64Array`][mdn-float64array] of length 1; receives the reciprocal condition number.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `3*N`.
-   **IWORK**: workspace [`Int32Array`][mdn-int32array] of length at least `N`.

#### dtbcon.ndarray( norm, uplo, diag, N, kd, AB, strideAB1, strideAB2, offsetAB, rcond, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Estimates the reciprocal condition number using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 upper triangular band matrix with kd=1 (column-major band storage):
var AB = new Float64Array( [ 0.0, 2.0, -1.0, 3.0, -1.0, 4.0 ] );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtbcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 1, 2, 0, rcond, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// rcond[ 0 ] contains the reciprocal condition number
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The reciprocal condition number is computed as `RCOND = 1 / ( norm(A) * norm(inv(A)) )`.
-   A small value of `rcond` indicates that the matrix is nearly singular.
-   The matrix `A` must be stored in LAPACK band format.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtbcon = require( '@stdlib/lapack/base/dtbcon' );

// 4x4 upper triangular band matrix, kd=2:
// [ 10  -1   2   0 ]
// [  0   8  -2   1 ]
// [  0   0  12  -3 ]
// [  0   0   0   6 ]
var AB = new Float64Array([
    0, 0, 10,  // col 1
    0, -1, 8,  // col 2
    2, -2, 12, // col 3
    1, -3, 6   // col 4
]);
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 12 );
var IWORK = new Int32Array( 4 );

var info = dtbcon( 'one-norm', 'upper', 'non-unit', 4, 2, AB, 3, rcond, WORK, IWORK );
console.log( 'info:', info );
console.log( 'rcond:', rcond[ 0 ] );
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
