# ztrrfs

> Provides error bounds and backward error estimates for the solution to a system of linear equations with a complex triangular coefficient matrix.

<section class="usage">

## Usage

```javascript
var ztrrfs = require( '@stdlib/lapack/base/ztrrfs' );
```

#### ztrrfs.ndarray( uplo, trans, diag, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Provides error bounds and backward error estimates for the solution to `A * X = B` or `A**H * X = B`, where `A` is a complex triangular matrix.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Upper triangular 2x2: A = [2+1i  1+2i; 0  4+1i]
var A = new Complex128Array( [ 2.0, 1.0, 0.0, 0.0, 1.0, 2.0, 4.0, 1.0 ] );
var B = new Complex128Array( [ 3.0, 3.0, 4.0, 1.0 ] );
var X = new Complex128Array( [ 1.0, 0.0, 1.0, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 4 );
var RWORK = new Float64Array( 2 );

var info = ztrrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 2, 1, A, 1, 2, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether `A` is upper or lower triangular (`'upper'` or `'lower'`).
-   **trans**: specifies the form of the system (`'no-transpose'` or `'conjugate-transpose'`).
-   **diag**: specifies whether `A` is unit triangular (`'unit'` or `'non-unit'`).
-   **N**: order of the matrix `A`.
-   **nrhs**: number of right-hand side columns in `B` and `X`.
-   **A**: triangular matrix as a [`Complex128Array`][mdn-typed-array].
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **B**: right-hand side matrix as a [`Complex128Array`][mdn-typed-array].
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix as a [`Complex128Array`][mdn-typed-array].
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bound for each right-hand side as a [`Float64Array`][mdn-float64array].
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bound for each right-hand side as a [`Float64Array`][mdn-float64array].
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array as a [`Complex128Array`][mdn-typed-array] of length at least `2*N`.
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: real workspace array as a [`Float64Array`][mdn-float64array] of length at least `N`.
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `WORK` should have length at least `2*N` and `RWORK` at least `N`.
-   The solution matrix `X` must be computed beforehand (e.g., by `ztrsv` or `ztrtrs`). This routine does not perform iterative refinement.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrrfs = require( '@stdlib/lapack/base/ztrrfs' );

// 3x3 upper triangular matrix (col-major):
var A = new Complex128Array( [ 2, 1, 0, 0, 0, 0, 1, 2, 4, 1, 0, 0, 3, 1, 5, 2, 6, 1 ] );
var B = new Complex128Array( [ 6, 4, 9, 3, 6, 1 ] );
var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = ztrrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
console.log( 'info:', info );
console.log( 'FERR:', FERR[ 0 ] );
console.log( 'BERR:', BERR[ 0 ] );
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
