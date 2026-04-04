# zgbrfs

> Improves the computed solution to a complex system A \* X = B where A is a general band matrix and provides error bounds.

<section class="usage">

## Usage

```javascript
var zgbrfs = require( '@stdlib/lapack/base/zgbrfs' );
```

#### zgbrfs.ndarray( trans, N, kl, ku, nrhs, AB, sAB1, sAB2, oAB, AFB, sAFB1, sAFB2, oAFB, IPIV, sIPIV, oIPIV, B, sB1, sB2, oB, X, sX1, sX2, oX, FERR, sFERR, oFERR, BERR, sBERR, oBERR, WORK, sWORK, oWORK, RWORK, sRWORK, oRWORK )

Improves the computed solution to a complex system of linear equations and provides error bounds and backward error estimates for the solution.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );

// 1x1 system: (3+2i)*x = (5+i)
var AB = new Complex128Array( 1 );
var abv = new Float64Array( AB.buffer );
abv[ 0 ] = 3.0;
abv[ 1 ] = 2.0;

var AFB = new Complex128Array( 1 );
var afbv = new Float64Array( AFB.buffer );
afbv[ 0 ] = 3.0;
afbv[ 1 ] = 2.0;

var IPIV = new Int32Array( [ 0 ] );

var B = new Complex128Array( 1 );
var bv = new Float64Array( B.buffer );
bv[ 0 ] = 5.0;
bv[ 1 ] = 1.0;

var X = new Complex128Array( 1 );
var xv = new Float64Array( X.buffer );
xv[ 0 ] = 1.307692;
xv[ 1 ] = -0.538462;

var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 2 );
var RWORK = new Float64Array( 1 );

var info = zgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
// info => 0
```

The function has the following parameters:

-   **trans**: specifies the transpose operation: `'no-transpose'` or `'conjugate-transpose'`.
-   **N**: order of the matrix A.
-   **kl**: number of subdiagonals.
-   **ku**: number of superdiagonals.
-   **nrhs**: number of right-hand side columns.
-   **AB**: original band matrix in band storage (`Complex128Array`).
-   **strideAB1**: stride of the first dimension of `AB`.
-   **strideAB2**: stride of the second dimension of `AB`.
-   **offsetAB**: starting index for `AB`.
-   **AFB**: LU-factored band matrix from `zgbtrf` (`Complex128Array`).
-   **strideAFB1**: stride of the first dimension of `AFB`.
-   **strideAFB2**: stride of the second dimension of `AFB`.
-   **offsetAFB**: starting index for `AFB`.
-   **IPIV**: pivot indices from `zgbtrf` (0-based, `Int32Array`).
-   **strideIPIV**: stride for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.
-   **B**: right-hand side matrix (`Complex128Array`).
-   **strideB1**: stride of the first dimension of `B`.
-   **strideB2**: stride of the second dimension of `B`.
-   **offsetB**: starting index for `B`.
-   **X**: solution matrix, improved on exit (`Complex128Array`).
-   **strideX1**: stride of the first dimension of `X`.
-   **strideX2**: stride of the second dimension of `X`.
-   **offsetX**: starting index for `X`.
-   **FERR**: output forward error bounds (`Float64Array`).
-   **strideFERR**: stride for `FERR`.
-   **offsetFERR**: starting index for `FERR`.
-   **BERR**: output backward error bounds (`Float64Array`).
-   **strideBERR**: stride for `BERR`.
-   **offsetBERR**: starting index for `BERR`.
-   **WORK**: complex workspace of length >= 2\*N (`Complex128Array`).
-   **strideWORK**: stride for `WORK`.
-   **offsetWORK**: starting index for `WORK`.
-   **RWORK**: real workspace of length >= N (`Float64Array`).
-   **strideRWORK**: stride for `RWORK`.
-   **offsetRWORK**: starting index for `RWORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   IPIV must contain 0-based pivot indices (as produced by `zgbtrf`).
-   AB uses standard BLAS band storage with `KL+KU+1` rows, diagonal at row `KU` (0-based).
-   AFB uses the factored band storage from `zgbtrf` with `2*KL+KU+1` rows.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgbrfs = require( '@stdlib/lapack/base/zgbrfs' );

var AB = new Complex128Array( 1 );
var AFB = new Complex128Array( 1 );
var IPIV = new Int32Array( [ 0 ] );
var B = new Complex128Array( 1 );
var X = new Complex128Array( 1 );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var WORK = new Complex128Array( 2 );
var RWORK = new Float64Array( 1 );

var info = zgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
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
