# dtbrfs

> Provides error bounds and backward error estimates for the solution to a system of linear equations with a triangular band coefficient matrix.

<section class="usage">

## Usage

```javascript
var dtbrfs = require( '@stdlib/lapack/base/dtbrfs' );
```

#### dtbrfs.ndarray( uplo, trans, diag, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Provides error bounds for the solution to a system with a real triangular band matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 4x4 upper triangular band matrix (kd=2) in band storage (col-major):
var AB = new Float64Array( [ 0, 0, 2, 0, 1, 4, 3, 5, 6, 2, 1, 3 ] );
var B = new Float64Array( [ 13.0, 31.0, 22.0, 12.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 12 );
var IWORK = new Int32Array( 4 );

var info = dtbrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 4, 2, 1,
    AB, 1, 3, 0, B, 1, 4, 0, X, 1, 4, 0,
    FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the form of the system (`'no-transpose'` or `'transpose'`).
-   **diag**: specifies whether the matrix has unit diagonal (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix A.
-   **kd**: number of super- or sub-diagonals of the triangular band matrix A.
-   **nrhs**: number of right-hand sides (columns of B and X).
-   **AB**: triangular band matrix A in band storage format.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **B**: right-hand side matrix B.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix X.
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output array for forward error bounds (length nrhs).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output array for backward errors (length nrhs).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array of length `3*N`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array of length `N`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The solution matrix `X` must be computed beforehand (e.g., by `dtbtrs`). This routine does not perform iterative refinement.
-   The band storage format stores the triangular band matrix A in a `(kd+1) x N` array where, for upper triangular, `AB(kd+i-j, j) = A(i,j)` and for lower triangular, `AB(i-j, j) = A(i,j)`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtbrfs = require( '@stdlib/lapack/base/dtbrfs' );

var AB = new Float64Array( [ 0, 0, 2, 0, 1, 4, 3, 5, 6, 2, 1, 3 ] );
var B = new Float64Array( [ 13.0, 31.0, 22.0, 12.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 12 );
var IWORK = new Int32Array( 4 );

var info = dtbrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 4, 2, 1,
    AB, 1, 3, 0, B, 1, 4, 0, X, 1, 4, 0,
    FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'FERR:', FERR );
console.log( 'BERR:', BERR );
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
