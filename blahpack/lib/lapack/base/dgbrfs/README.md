# dgbrfs

> Improves the computed solution to a real system A \* X = B where A is a general band matrix and provides error bounds.

<section class="usage">

## Usage

```javascript
var dgbrfs = require( '@stdlib/lapack/base/dgbrfs' );
```

#### dgbrfs.ndarray( trans, N, kl, ku, nrhs, AB, sAB1, sAB2, oAB, AFB, sAFB1, sAFB2, oAFB, IPIV, sIPIV, oIPIV, B, sB1, sB2, oB, X, sX1, sX2, oX, FERR, sFERR, oFERR, BERR, sBERR, oBERR, WORK, sWORK, oWORK, IWORK, sIWORK, oIWORK )

Improves the computed solution to a real system of linear equations and provides error bounds and backward error estimates for the solution.

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 1x1 system: 3*x = 5
var AB = new Float64Array( [ 3.0 ] );
var AFB = new Float64Array( [ 3.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var B = new Float64Array( [ 5.0 ] );
var X = new Float64Array( [ 5.0 / 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 3 );
var IWORK = new Int32Array( 1 );

var info = dgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **trans**: specifies the transpose operation: `'no-transpose'` or `'transpose'`.
-   **N**: order of the matrix A.
-   **kl**: number of subdiagonals.
-   **ku**: number of superdiagonals.
-   **nrhs**: number of right-hand side columns.
-   **AB**: original band matrix in band storage (`Float64Array`).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: LU-factored band matrix from `dgbtrf` (`Float64Array`).
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
-   **IPIV**: pivot indices from `dgbtrf` (0-based, `Int32Array`).
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix (`Float64Array`).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix, improved on exit (`Float64Array`).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bounds (`Float64Array`).
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds (`Float64Array`).
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: workspace of length >= 3\*N (`Float64Array`).
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **IWORK**: integer workspace of length >= N (`Int32Array`).
-   **strideIWORK**: stride for `IWORK`.
-   **offsetIWORK**: starting index for `IWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   IPIV must contain 0-based pivot indices (as produced by `dgbtrf`).
-   AB uses standard BLAS band storage with `KL+KU+1` rows, diagonal at row `KU` (0-based).
-   AFB uses the factored band storage from `dgbtrf` with `2*KL+KU+1` rows.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbrfs = require( '@stdlib/lapack/base/dgbrfs' );

var AB = new Float64Array( [ 3.0 ] );
var AFB = new Float64Array( [ 3.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var B = new Float64Array( [ 5.0 ] );
var X = new Float64Array( [ 5.0 / 3.0 ] );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Float64Array( 3 );
var IWORK = new Int32Array( 1 );

var info = dgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
console.log( info );
// => 0
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

</section>

<!-- /.links -->
