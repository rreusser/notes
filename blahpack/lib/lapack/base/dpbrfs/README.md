# dpbrfs

> Improve the computed solution to a real system `A * X = B` where `A` is symmetric positive definite band, and provide error bounds and backward error estimates.

<section class="usage">

## Usage

```javascript
var dpbrfs = require( '@stdlib/lapack/base/dpbrfs' );
```

#### dpbrfs.ndarray( uplo, N, kd, nrhs, AB, strideAB1, strideAB2, offsetAB, AFB, strideAFB1, strideAFB2, offsetAFB, B, strideB1, strideB2, offsetB, X, strideX1, strideX2, offsetX, FERR, strideFERR, offsetFERR, BERR, strideBERR, offsetBERR, WORK, strideWORK, offsetWORK, IWORK, strideIWORK, offsetIWORK )

Improves the computed solution to `A * X = B` where `A` is symmetric positive definite band, and provides forward and backward error bounds.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpbtrf = require( '@stdlib/lapack/base/dpbtrf' );
var dpbtrs = require( '@stdlib/lapack/base/dpbtrs' );

// 3x3 SPD band matrix with KD=1, upper storage:
// Full: [4 1 0; 1 5 1; 0 1 6]
var ab = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 1.0, 6.0 ] );
var afb = new Float64Array( ab );

dpbtrf.ndarray( 'upper', 3, 1, afb, 1, 2, 0 );

var b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var x = new Float64Array( b );

dpbtrs.ndarray( 'upper', 3, 1, 1, afb, 1, 2, 0, x, 1, 3, 0 );

var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpbrfs.ndarray( 'upper', 3, 1, 1, ab, 1, 2, 0, afb, 1, 2, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
// x => refined solution
// FERR => forward error bound
// BERR => backward error bound
```

The function has the following parameters:

-   **uplo**: specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle of A is stored in band format.
-   **N**: order of the matrix `A`.
-   **kd**: number of superdiagonals (if `uplo` is `'upper'`) or subdiagonals (if `uplo` is `'lower'`) of `A`.
-   **nrhs**: number of right-hand side columns.
-   **AB**: original symmetric band matrix in band storage.
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: Cholesky-factored band matrix (from `dpbtrf`).
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
-   **B**: right-hand side matrix.
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix (improved on exit).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bounds (length `nrhs`).
-   **strideFERR**: stride length for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds (length `nrhs`).
-   **strideBERR**: stride length for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace array (length >= `3*N`).
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace array (length >= `N`).
-   **strideIWORK**: stride length for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dpbrfs` uses iterative refinement to improve the solution computed by `dpbtrs` and provides componentwise forward and backward error bounds.
-   The workspace array `WORK` is partitioned into three segments of length `N` for internal computations.
-   The routine uses `dlacn2` (reverse communication) to estimate the infinity-norm of `inv(A) * diag(W)` for computing forward error bounds.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dpbtrf = require( '@stdlib/lapack/base/dpbtrf' );
var dpbtrs = require( '@stdlib/lapack/base/dpbtrs' );
var dpbrfs = require( '@stdlib/lapack/base/dpbrfs' );

// 3x3 SPD band matrix with KD=1, upper storage
var ab = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 1.0, 6.0 ] );
var afb = new Float64Array( ab );

dpbtrf.ndarray( 'upper', 3, 1, afb, 1, 2, 0 );

var b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var x = new Float64Array( b );

dpbtrs.ndarray( 'upper', 3, 1, 1, afb, 1, 2, 0, x, 1, 3, 0 );

var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dpbrfs.ndarray( 'upper', 3, 1, 1, ab, 1, 2, 0, afb, 1, 2, 0, b, 1, 3, 0, x, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );

console.log( 'info:', info );
console.log( 'x:', x );
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
