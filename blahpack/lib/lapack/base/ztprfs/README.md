# ztprfs

> Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed complex triangular coefficient matrix.

<section class="usage">

## Usage

```javascript
var ztprfs = require( '@stdlib/lapack/base/ztprfs' );
```

#### ztprfs.ndarray( uplo, trans, diag, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed complex triangular coefficient matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );

// 3x3 upper triangular packed (complex, interleaved re/im):
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 4.0, -1.0, 3.0, 2.0, 5.0, 0.0, 6.0, -0.5 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
var X = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the form of the system (`'no-transpose'` or `'conjugate-transpose'`).
-   **diag**: specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand sides.
-   **AP**: packed triangular matrix A as a [`Complex128Array`][mdn-typed-array].
-   **strideAP**: stride length for `AP` (in complex elements).
-   **offsetAP**: starting index for `AP` (in complex elements).
-   **B**: right-hand side matrix B as a [`Complex128Array`][mdn-typed-array].
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: solution matrix X as a [`Complex128Array`][mdn-typed-array].
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **FERR**: output [`Float64Array`][mdn-float64array] of length `nrhs` for forward error bounds.
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output [`Float64Array`][mdn-float64array] of length `nrhs` for backward errors.
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace [`Complex128Array`][mdn-typed-array] of length `2*N`.
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace [`Float64Array`][mdn-float64array] of length `N`.
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `ztprfs` computes componentwise relative forward error bounds (FERR) and componentwise relative backward errors (BERR) for each right-hand side.
-   The complex workspace array WORK must have at least `2*N` elements and the real workspace RWORK must have at least `N` elements.
-   Uses CABS1 (sum of absolute values of real and imaginary parts) for componentwise error bounds, consistent with the LAPACK reference implementation.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var ztprfs = require( '@stdlib/lapack/base/ztprfs' );

// 3x3 upper triangular packed (complex, interleaved re/im):
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 4.0, -1.0, 3.0, 2.0, 5.0, 0.0, 6.0, -0.5 ] );
var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
var X = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
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
