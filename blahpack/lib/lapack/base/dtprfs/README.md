# dtprfs

> Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed triangular coefficient matrix.

<section class="usage">

## Usage

```javascript
var dtprfs = require( '@stdlib/lapack/base/dtprfs' );
```

#### dtprfs.ndarray( uplo, trans, diag, N, nrhs, AP, strideAP, offsetAP, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Provides error bounds and backward error estimates for the solution to a system of linear equations with a packed triangular coefficient matrix.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 3x3 upper triangular packed: A = [2 1 3; 0 4 5; 0 0 6]
var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
var B = new Float64Array( [ 13.0, 23.0, 18.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// returns 0
```

The function has the following parameters:

-   **uplo**: specifies whether the matrix is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the form of the system (`'no-transpose'` or `'transpose'`).
-   **diag**: specifies whether the matrix is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix A.
-   **nrhs**: number of right-hand sides.
-   **AP**: packed triangular matrix A as a [`Float64Array`][mdn-float64array].
-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **B**: right-hand side matrix B as a [`Float64Array`][mdn-float64array].
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix X as a [`Float64Array`][mdn-float64array].
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output [`Float64Array`][mdn-float64array] of length `nrhs` for forward error bounds.
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output [`Float64Array`][mdn-float64array] of length `nrhs` for backward errors.
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length `3*N`.
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace [`Int32Array`][mdn-int32array] of length `N`.
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dtprfs` computes componentwise relative forward error bounds (FERR) and componentwise relative backward errors (BERR) for each right-hand side.
-   The workspace array WORK must have at least `3*N` elements and IWORK must have at least `N` elements.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtprfs = require( '@stdlib/lapack/base/dtprfs' );

// 3x3 upper triangular packed: A = [2 1 3; 0 4 5; 0 0 6]
var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
var B = new Float64Array( [ 13.0, 23.0, 18.0 ] );
var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
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
