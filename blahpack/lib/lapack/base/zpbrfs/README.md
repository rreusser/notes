# zpbrfs

> Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite band and provides error bounds.

<section class="usage">

## Usage

```javascript
var zpbrfs = require( '@stdlib/lapack/base/zpbrfs' );
```

#### zpbrfs.ndarray( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, AFB, strideAFB1, strideAFB2, offsetAFB, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK )

Improves the computed solution to a complex system A * X = B where A is Hermitian positive definite band and provides error bounds.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// Hermitian positive definite band matrix (upper, KD=1, N=3):
var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0 ] );
var AFB = new Complex128Array( [ 0.0, 0.0, 2.0, 0.0, 0.5, 0.5, 2.12132034355964239, 0.0, 0.942809041582063467, -0.471404520791031734, 2.21108319357026639, 0.0 ] );
var B = new Complex128Array( [ 4.0, 2.0, 10.0, 2.0, 13.0, 3.0 ] );
var X = new Complex128Array( [ 1.0, 0.0, 1.0, 1.0, 2.0, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zpbrfs.ndarray( 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **uplo**: specifies whether `'upper'` or `'lower'` triangle of `A` is stored.
-   **N**: order of matrix `A`.
-   **kd**: number of superdiagonals (upper) or subdiagonals (lower) of `A`.
-   **nrhs**: number of right-hand side columns.
-   **AB**: original Hermitian band matrix in band storage (`Complex128Array`).
-   **strideAB1**: stride of the first dimension of `AB` (in complex elements).
-   **strideAB2**: stride of the second dimension of `AB` (in complex elements).
-   **offsetAB**: starting index for `AB` (in complex elements).
-   **AFB**: Cholesky-factored band matrix from `zpbtrf` (`Complex128Array`).
-   **strideAFB1**: stride of the first dimension of `AFB` (in complex elements).
-   **strideAFB2**: stride of the second dimension of `AFB` (in complex elements).
-   **offsetAFB**: starting index for `AFB` (in complex elements).
-   **B**: right-hand side matrix (`Complex128Array`).
-   **strideB1**: stride of the first dimension of `B` (in complex elements).
-   **strideB2**: stride of the second dimension of `B` (in complex elements).
-   **offsetB**: starting index for `B` (in complex elements).
-   **X**: solution matrix, improved on exit (`Complex128Array`).
-   **strideX1**: stride of the first dimension of `X` (in complex elements).
-   **strideX2**: stride of the second dimension of `X` (in complex elements).
-   **offsetX**: starting index for `X` (in complex elements).
-   **FERR**: output forward error bounds (`Float64Array`, length `nrhs`).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds (`Float64Array`, length `nrhs`).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace array (`Complex128Array`, length >= `2*N`).
-   **strideWORK**: stride length for `WORK` (in complex elements).
-   **offsetWORK**: starting index for `WORK` (in complex elements).
-   **RWORK**: real workspace array (`Float64Array`, length >= `N`).
-   **strideRWORK**: stride length for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   This routine uses iterative refinement to improve the computed solution and provides forward and backward error bounds via the condition number estimation routine `zlacn2`.
-   The Cholesky factorization of `A` must be computed by `zpbtrf` before calling this routine. The factored matrix is passed as `AFB`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbrfs = require( '@stdlib/lapack/base/zpbrfs' );

// 3x3 HPD band matrix (upper, KD=1):
var AB = new Complex128Array( [ 0.0, 0.0, 4.0, 0.0, 1.0, 1.0, 5.0, 0.0, 2.0, -1.0, 6.0, 0.0 ] );
var AFB = new Complex128Array( [ 0.0, 0.0, 2.0, 0.0, 0.5, 0.5, 2.12132034355964239, 0.0, 0.942809041582063467, -0.471404520791031734, 2.21108319357026639, 0.0 ] );
var B = new Complex128Array( [ 4.0, 2.0, 10.0, 2.0, 13.0, 3.0 ] );
var X = new Complex128Array( [ 1.0, 0.0, 1.0, 1.0, 2.0, 0.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

var info = zpbrfs.ndarray( 'upper', 3, 1, 1, AB, 1, 2, 0, AFB, 1, 2, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
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
